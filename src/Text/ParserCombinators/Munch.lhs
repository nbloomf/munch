---
title: Munch
---

Let's build a parser combinator library!

I've been using the Parsec library and its descendants to write parsers for years without really knowing how parser combinators work. Today we're going to try to fix that. In this module we'll be writing a barebones reimplementation of Parsec (with a few tweaks) from scratch while following along with some relevant papers.

* _Parsec: Direct Style Monadic Parser Combinators For The Real World_ by Daan Leijen and Erik Meijer
* _Indentation-Sensitive Parsing for Parsec_ by Michael Adams and Ömer Ağacan
* _Parsing Permutation Phrases_ by Arthur Baars, Andres Löh, and Doaitse Swierstra

I'm not super interested in optimization, but in the end we should have a usable parsing library. The goal is to (1) have a small set of parser combinators for building PEG-style parsers with (2) additional primitives for indentation-sensitivity (think _relevant whitespace_) and for parsing permutation phrases (think _command line flags_) and to (3) understand how it all works. Along the way I'll assume that you're an intermediate FPer -- comfortable with type inference and the basic monads -- but not necessarily familiar with the guts of parsec.

This document is a literate Haskell module with code and library documentation interspersed among the prose. Code blocks appear with syntax highlighting. Some code isn't part of the library, but just provides a concrete example. These blocks have a blueish background like this:

```haskell
-- This is just an example.
testing :: IO ()
```

Code that is part of the library appears on a gray background, like this block of compiler noises.

> {-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
> {-|
> Module      : Text.ParserCombinators.Munch
> Description : Simple parser combinator library
> Copyright   : (c) Automattic, Inc. 2018
> License     : GPL-3
> Maintainer  : Nathan Bloomfield, nathan.bloomfield@a8c.com
> Stability   : experimental
> Portability : POSIX
> 
> Parser combinator library with support for indentation sensitivity.
> -}
> module Text.ParserCombinators.Munch (
>     -- * Usage
>     -- $overview
>     Parser(), runParser, debugParser, parseIO
>   , Stream(..), Pos(..), DidConsume(..), CharStream()
>     -- * Basic Parsers
>   , char, bof, eof, satisfies, wouldFail, wouldSucceed, anyChar
>   , decimalDigit, hexDigit, lowerLatin, upperLatin, spaces, newline
>   , choice, manySepBy, someSepBy, string
>     -- * Errors
>   , (<?>), Error(..), BasicError(..), Annotation(..), ParseError()
>   , displayParseError
>     -- * Indentation
>   , readRef, adjustRef, localRef, consume, ignore
>   , Indentation(..), indent
>     -- ** Simple Indentation
>   , wrtRef, wrtPos, Endpoint(..), Relation(..), Dimension(..)
>     -- * Permutations
>   , permute, permuteSepBy, permuteIndent, permuteIndentSepBy
>   , (<$$>), (<&&>), (<$?>), (<&?>), Perms()
> ) where
> 
> import Data.List (intersperse, unwords)
> import Data.Semigroup
> import Data.String
> import Control.Applicative
> import Control.Monad
> import qualified Control.Monad.Fail as F



Contents
--------

* The Library
  * [Getting Started](#getting-started)
  * [Basic Combinators](#basic-combinators)
  * [Simple Streams](#simple-streams)
  * [Indentation](#indentation)
  * [Errors](#errors)
  * [Permutation Phrases](#permutation-phrases)
* Appendices
  * [Pretty Printing](#pretty-printing)
  * [An Indentation DSL](#an-indentation-dsl)
  * [Usage Overview](#usage-overview)
  * [Examples](/munch/munch-test.html)



Getting Started
---------------

A good way to begin thinking about the design of a library, especially in a language with strong types, is to start with the type signatures of the basic concepts. We don't have to write a fully formed type signature in one go -- we can start simple and add complexity one step at a time.

To a first approximation a _parser_ is a function taking a string of characters to a value of some other type `a`.

```haskell
parse :: String -> a
```

For the simplest possible parsers, and considering only the happy path where the input is always well-formed, this is good enough. But of course real parsers are complicated and real input is malformed, and the mark of a good parser is the error messages it reports when we go off of the happy path. If something goes wrong, parsing should report some diagnostic error. We can do this with `Either`.

```haskell
parse :: String -> Either ParseError a
```

Don't worry about what `ParseError` looks like for now -- just assume it carries some information about what broke.

This signature makes an implicit assumption: that our parser will consume the entire input string. This should be true for top-level parsers (if there are no errors) but the point of using combinators is to build big parsers out of small ones, and small parsers definitely should _not_ consume the entire input string. So the output should include the "unconsumed" portion of the input. We have to decide whether to return this inside or outside the `Either`; in other words, whether to return the unconsumed string with the error value. I'll put it inside, though I can't think of a reason why this is the only correct choice.

```haskell
parse :: String -> Either ParseError (a, String)
```

This is a decent parser type signature. However, as Leijen and Meijer discuss in their paper, there is a significant efficiency boost to be had if the parse function also returns whether or not it actually consumed any input. The Parsec paper does this with an intermediate type constructor, but I'm pretty sure we can get the same benefits from a tuple. We will make one tweak -- rather than just reporting whether or not any characters are consumed, we'll also report the inclusive range of character positions that were consumed.

```haskell
parse :: String -> (DidConsume, Either ParseError (a, String))
```

The `DidConsume` type is defined like so.

> -- | Represents whether a parser consumed part of the input stream.
> data DidConsume
>   = Declined         -- ^ The parse did not consume any characters.
>   | Consumed Pos Pos -- ^ The parse consumed input characters
>                      --   between two positions (inclusive).
>   deriving Show
> 
> -- | @Pos@ represents a position in the input stream consisting of
> --   a /line number/ and a /column number/, both of which are
> --   nonnegative integers. The @Ord@ instance is lexicographic by
> --   line, then column. 
> data Pos = Pos
>   { line :: Integer
>   , column :: Integer
>   } deriving (Eq, Ord, Show)

Note that "parse consumed characters" and "parse succeeded" are orthogonal. This is important. Note also that the location of `DidConsume` in the signature of `parse` fits the classic writer monad pattern. To make this legit, `DidConsume` will need a `Monoid` instance -- that is, we need a natural way to combine two pieces of information about consumption into a single one. The Parsec paper talks about this (albeit in the language of type constructors, rather than data): if `a` and `b` are `DidConsume`s, then `a <> b` should be `Consumed` if either `a` or `b` is, and should be `Declined` otherwise. The simplest way I can think of to combine two ranges is to construct their "convex hull"; the smallest range containing both. Concretely:

> -- | The product of two @Consumed@ ranges is their convex hull. This is
> --   the direct product of the @min@ and @max@ semigroups over @Integer@
> --   with a unit attached.
> instance Semigroup DidConsume where
>   Declined       <> Declined       = Declined
>   Consumed a1 b1 <> Declined       = Consumed a1 b1
>   Declined       <> Consumed a2 b2 = Consumed a2 b2
>   Consumed a1 b1 <> Consumed a2 b2 = Consumed (min a1 a2) (max b1 b2)
> 
> instance Monoid DidConsume where
>   mempty  = Declined
>   mappend = (<>)

This sort of makes sense; in the typical case where two ranges are directly adjacent, the convex hull is the same as concatenation. Note that this is a bona fide `Monoid` since it is (isomorphic to) the direct product of the min and max semigroups over `Integer` with a unit attached. Note, however, that we're not making any assumptions about whether the "left" endpoint in a consumed range comes before the "right" endpoint -- and if that's the case this isn't really computing the convex hull, although this shouldn't happen in practice.

Parsec makes one more basic adjustment to this type: rather than parsing `String`s, it parses values of any type satisfying a `Stream` class...

```haskell
parser :: (Stream s) => s -> (DidConsume, Either ParseError (a, s))
```

...which we'll define like so.

> -- | Class representing types of "parseable" text.
> class Stream s where
>   -- | If any characters remain in the stream, returns the
>   --   first one with its position and the rest of the stream.
>   uncons :: s -> Maybe (Char, Pos, s)
> 
> -- | @True@ if there are no characters left in the stream.
> isEmpty :: (Stream s) => s -> Bool
> isEmpty stream = case uncons stream of
>   Nothing -> True
>   Just _ -> False
> 
> -- | @Just@ the @Pos@ at the head of the stream, or @Nothing@
> --   if the stream is empty.
> pointer :: (Stream s) => s -> Maybe Pos
> pointer = fmap (\(_, pos, _) -> pos) . uncons

Essentially a type is a _stream_ if it makes sense to pop `Char`s from it in some canonical order -- `String` is not a great data structure if we care a lot about efficiency, so it's nice to make it optional.

I'm going to make one final adjustment to the parser type to accommodate _indentation sensitive parsing_. While writing this up, I read the Adams and Ağacan paper on parsing indentation, and have to admit I wasn't able to get it to work -- which, to be clear, is entirely my failing. That paper is a nice read and has some really good ideas. Although I couldn't get the exact implementation working, I will take inspiration and try to add indentation-sensitivity to this library. And since we're basically reimplementing parsec from scratch we can afford to sidestep some of the compromises that paper has to make as a result of trying to integrate with existing code.

We've already attached a `Pos` to each lexeme in the input stream. In the most general terms, a parser is _indentation sensitive_ if it only succeeds when it successfully consumes some characters _and_ the positions at which it succeeds satisfy an extra constraint. This definition is a little too general, so to narrow our space of design decisions lets be more specific.

Our parsers will carry one more piece of state, a `Pos` called the _reference position_. All intentation sensitive primitives will be defined in terms of this reference. But we have a choice to make. What kind of state is this? It certainly needs to be readable so we can detect invalid indentations. But does it need to be writeable? Should we model the indentation stack using `State` or `Reader`? By representing indentation with `State`, we allow any parser to implicitly fiddle with the reference position of any other. On the other hand, with `Reader`, any changes to the reference position are explicit at the call site. The tradeoff with `Reader` is that state changes _must_ be explicit. Since debugging parsers is hard enough without mutable state; I'll go with `Reader`.

So our final parser type looks like this. Note how `Pos` only appears on the left of an arrow.

> -- | The opaque type of a parser over `Char` streams of type `s`
> --   producing a value of type `a`. Parsers are built up using
> --   the @Applicative@, @Alternative@, and @Monad@ interfaces
> --   and the atomic parsers defined in this module.
> data Parser s a = Parser
>   { theParser :: Pos -> s -> (DidConsume, Either ParseError (a, s)) }

Last but not least we sometimes need to actually _run_ a parser, and since parsers are just functions that that means evaluate. We'll do this in three ways: one that returns all the gory details, one that just returns a result, and one that just pretty prints the result.

> -- | Run a parser against a stream, returning the result as well
> --   as the consumed range and the remaining stream.
> debugParser
>   :: Parser s a -> s -> (DidConsume, Either ParseError (a, s))
> debugParser (Parser q) stream = q (Pos 1 1) stream
> 
> -- | Run a parser against a stream, returning only the result.
> runParser
>   :: (Stream s) => Parser s a -> s -> Either ParseError a
> runParser (Parser q) stream = case q (Pos 1 1) stream of
>   (_, Left err) -> Left err
>   (_, Right (a, rest)) -> if isEmpty rest
>     then Right a
>     else Left $ Simply $ IncompleteParse (pointer rest)
> 
> -- | Pretty print the result of a parse.
> parseIO
>   :: (Stream s, Show a) => Parser s a -> s -> IO ()
> parseIO (Parser q) stream = case q (Pos 1 1) stream of
>   (_, Left err) -> do
>     putStrLn "Parse Error"
>     putStrLn $ pretty err
>   (_, Right (a, rest)) -> if isEmpty rest
>     then do
>       putStrLn "Parse OK"
>       putStrLn $ show a
>     else do
>       putStrLn "Parse Incomplete"
>       putStrLn $ show $ pointer rest



Basic Combinators
-----------------

The `Parser s` type constructor is defined as a stack of monads -- state, error, writer, and reader. When this happens our first instinct should be to write the monad implementation, because this gives us a huge amount of code for free. This part is mostly standard stuff.

> instance Functor (Parser s) where
>   fmap f (Parser q) = Parser $ \ref stream ->
>     let (c, result) = q ref stream in
>     case result of
>       Left err -> (c, Left err)
>       Right (a, rest) -> (c, Right (f a, rest))
> 
> instance Applicative (Parser s) where
>   pure x = Parser $ \_ stream ->
>     (Declined, Right (x, stream))
> 
>   af <*> ax = do
>     f <- af
>     x <- ax
>     return (f x)

The implementation of the `<|>` operator in the `Alternative` instance represents PEG style ordered choice; if the left parser fails, we pretend no input was consumed and try the right parser. Ignore what's happening with the errors here -- we'll get to that. Just note that `empty` is supposed to be neutral for `<|>`, which requires errors to form a monoid.

> -- | The @\<|>@ operator implements /ordered choice/; we try the left
> --   parser, and if it fails, pretend it did not consume any input and
> --   try the right parser. If both fail we report a combination of
> --   their error messages. @empty@ represents a generic failure. It's
> --   included for completeness but should usually be avoided.
> instance Alternative (Parser s) where
>   -- generic failure
>   empty = Parser $ \_ _ ->
>     (Declined, Left mempty)
> 
>   -- ordered choice
>   (Parser a) <|> (Parser b) = Parser $ \ref stream ->
>     case a ref stream of
>       (c, Right value) -> (c, Right value)
>       (_, Left err1) -> case b ref stream of
>         (c, Right value) -> (c, Right value)
>         (c, Left err2) -> (c, Left $ err1 <> err2)

The `>>` operator in the `Monad` instance represents PEG style sequencing.

> instance Monad (Parser s) where
>   return = pure
> 
>   (Parser x) >>= f = Parser $ \ref stream ->
>     let (c, result) = x ref stream in
>     case c of
>       Declined -> case result of
>         Left err -> (Declined, Left err)
>         Right (a, rest) -> theParser (f a) ref rest
> 
>       c1 ->
>         let
>           (c2, h) = case result of
>             Left err -> (Declined, Left err)
>             Right (a, rest) -> theParser (f a) ref rest
>         in (c1 <> c2, h)
> 
> -- | Default instance in terms of @Alternative@.
> instance MonadPlus (Parser s)

> instance (Stream s) => F.MonadFail (Parser s) where
>   fail msg = Parser $ \_ stream ->
>     (Declined, Left $ Simply $ Failure msg (pointer stream))

We can also give `Parser s a` a `Semigroup` and `Monoid` instance.

> -- | The @\<>@ implementation allows us to combine the results of
> --   two parsers. Compare to @>>@, @*>@, and @<*@, which combine two
> --   parsers but only return the result of one.
> instance (Semigroup a) => Semigroup (Parser s a) where
>   (<>) = liftA2 (<>)
> 
> instance (Monoid a) => Monoid (Parser s a) where
>   mempty  = return mempty
>   mappend = liftA2 mappend

At this point we've already got a huge built-in library of utility functions based on `Functor`, `Applicative`, `Alternative`, and `Monad`, but not any concrete parsers (other than `return`). To address this we'll also define some atomic parsers for recognizing characters and the beginning and end of the stream.

> -- | Expects the specified character.
> char :: (Stream s) => Char -> Parser s Char
> char c = Parser $ \_ stream ->
>   case uncons stream of
>     Nothing ->
>       (Declined, Left $ Simply $ UnexpectedEOF (Right $ Just c))
> 
>     Just (a,pos,rest) ->
>       if a == c
>         then (Consumed pos pos, Right (a, rest))
>         else (Declined, Left $ Simply $ UnexpectedChar a (Just c) pos)
> 
> -- | Expects the end of the stream.
> eof :: (Stream s) => Parser s ()
> eof = Parser $ \_ stream ->
>   case uncons stream of
>     Nothing ->
>       (Declined, Right ((), stream))
> 
>     Just (a,pos,_) ->
>       (Declined, Left $ Simply $ UnexpectedChar a Nothing pos)
> 
> -- | Expects the beginning of the stream.
> bof :: (Stream s) => Parser s ()
> bof = Parser $ \_ stream ->
>   case uncons stream of
>     Nothing ->
>       (Declined, Left $ Simply $ UnexpectedEOF (Right Nothing))
> 
>     Just (_,pos,_) ->
>       if pos == Pos 1 1
>         then (Declined, Right ((), stream))
>         else (Declined, Left $ Simply $ ExpectedBOF pos)

Slightly more general than `char` is `satisfies`, which accepts characters that satisfy some given predicate. In principle `satisfies` is redundant, since it can be implemented in terms of `char` and `<|>`. But in practice it makes error messages much nicer.

> -- | Expects a character satisfying the given predicate.
> satisfies
>   :: (Stream s)
>   => (Char -> Bool)
>   -> String         -- ^ Human-readable name for
>                     --   the class of recognized characters.
>   -> Parser s Char
> satisfies p msg = Parser $ \_ stream ->
>   case uncons stream of
>     Nothing ->
>       (Declined, Left $ Simply $ UnexpectedEOF (Left msg))
> 
>     Just (a,pos,rest) ->
>       if p a
>         then (Consumed pos pos, Right (a, rest))
>         else (Declined, Left $ Simply $ UnexpectedSatisfy a msg pos)

We can also define PEG-style positive and negative lookahead combinators; these allow for speculative parsing.

> -- | @wouldFail p@ succeeds if and only if @p@ fails (whether
> --   or not @p@ consumes input), but does not consume any input.
> wouldFail :: (Stream s) => Parser s a -> Parser s ()
> wouldFail (Parser q) = Parser $ \ref stream ->
>   let
>     (_,r) = q ref stream
>     h = case r of
>       Left _  -> Right ((), stream)
>       Right _ -> Left $ Simply $ UnexpectedSuccess (pointer stream)
>   in (Declined, h)
> 
> -- | @wouldSucceed p@ succeeds if and only if @p@ succeeds, (whether
> --   or not @p@ consumes input), but does not consume any input.
> wouldSucceed :: (Stream s) => Parser s a -> Parser s ()
> wouldSucceed (Parser q) = Parser $ \ref stream ->
>   let
>     (_,r) = q ref stream
>     h = case r of
>       Right _ -> Right ((), stream)
>       Left e  -> Left $ Because (Lookahead (pointer stream)) e
>   in (Declined, h)



Simple Streams
--------------

We've now more or less got a basic parser combinator library. Before we can actually use it we need an instance of `Stream`. For basic use we'll define `CharStream` -- a list of `Char`s with `Pos`s.

> -- | Simple and unoptimized list-based stream type.
> data CharStream
>   = CharStream [(Char, Pos)]
>   deriving Show
> 
> instance Stream CharStream where
>   uncons (CharStream str) = case str of
>     [] -> Nothing
>     (a,pos):as -> Just (a, pos, CharStream as)
> 
> instance IsString CharStream where
>   fromString = CharStream . annotate
>     where
>       annotate :: String -> [(Char, Pos)]
>       annotate = f 1 1
>         where
>           f :: Integer -> Integer -> [Char] -> [(Char, Pos)]
>           f ln col str = case str of
>             [] -> []
>             a:as -> if a == '\n'
>               then (a, Pos ln col) : f (ln+1) 1 as
>               else (a, Pos ln col) : f ln (col+1) as

We'll also define some commonly used parsers.

> -- | Expects any character.
> anyChar :: (Stream s) => Parser s Char
> anyChar = satisfies (const True) "any character"
> 
> -- | Expects a character in the range @['0'..'9']@.
> decimalDigit :: (Stream s) => Parser s Char
> decimalDigit = satisfies
>   (\c -> elem c "0123456789")
>   "decimal digit (0-9)"
> 
> -- | Expects a hexadecimal digit (0-9, a-f, A-F)
> hexDigit :: (Stream s) => Parser s Char
> hexDigit = satisfies
>   (\c -> elem c "0123456789abcdefABCDEF")
>   "hexadecimal digit (0-9, a-f, A-F)"
> 
> -- | Expects a character in the range @['a'..'z']@.
> lowerLatin :: (Stream s) => Parser s Char
> lowerLatin = satisfies
>   (\c -> elem c "abcdefghijklmnopqrstuvwxyz")
>   "lower case latin letter (a-z)"
> 
> -- | Expects a character in the range @['A'..'Z']@.
> upperLatin :: (Stream s) => Parser s Char
> upperLatin = satisfies
>   (\c -> elem c "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
>   "upper case latin letter (A-Z)"
> 
> -- | Expects zero or more space characters.
> spaces :: (Stream s) => Parser s String
> spaces = many $ char ' '
> 
> -- | Expects a newline character.
> newline :: (Stream s) => Parser s Char
> newline = char '\n'

And some derived combinators.

> -- | Tries the argument parsers one at a time in order,
> --   backtracking on failure. Returns the value of the
> --   first succeeding parser.
> choice :: (Stream s) => [Parser s a] -> Parser s a
> choice ps = foldr (<|>) empty ps
> 
> -- | One or more @p@, separated by @sep@.
> someSepBy :: Parser s u -> Parser s a -> Parser s [a]
> someSepBy sep p = (:) <$> p <*> many (sep >> p)
> 
> -- | Zero or more @p@, separated by @sep@.
> manySepBy :: Parser s u -> Parser s a -> Parser s [a]
> manySepBy sep p = (someSepBy sep p) <|> return [] 



Indentation
-----------

Recall that every parser runs in the context of a special `Pos` value called the _reference position_. Now we will define some primitive combinators that affect the behavior of parsers using this value.

First off, since the reference position is a reader context, we should have combinators for getting its value and making local changes: `readRef` gets the value of the reference position, `adjustRef` applies an arbitrary function to the reference in a context, and `localRef` sets it to a specific value in a context. Note that the scope of these changes is strictly limited.

> -- | Returns the current reference position.
> readRef :: (Stream s) => Parser s Pos
> readRef = Parser $ \ref stream -> (Declined, Right (ref, stream))
> 
> -- | Apply a function to the reference position, and run
> --   a parser in that context.
> adjustRef :: (Stream s) => (Pos -> Pos) -> Parser s a -> Parser s a
> adjustRef f (Parser p) = Parser $ \ref stream -> p (f ref) stream
> 
> -- | Set the reference position to a given value, and run
> --   a parser in that context.
> localRef :: (Stream s) => Pos -> Parser s a -> Parser s a
> localRef = adjustRef . const

Next it will be handy to have a combinator that returns the consumed range of characters. Thinking of `DidConsume` as the log monoid for the `Writer` monad pattern, this is analogous to the standard `listen` function, with one exception: `consume p` should fail if `p` declines to consume characters.

> -- | Run a parser against the stream, and return the consumed
> --   range with the result. Fails if the parser declines to
> --   consume any input.
> consume :: (Stream s) => Parser s a -> Parser s (a, (Pos, Pos))
> consume (Parser p) = Parser $ \ref stream ->
>   let (c,r) = p ref stream in
>   case c of
>     Declined ->
>       let pos = pointer stream in
>       case r of
>         Right _ ->
>           (Declined, Left $ Simply $
>             UnexpectedDecline pos)
>         Left err ->
>           (Declined, Left $ Because (DeclineReason pos) err)
> 
>     Consumed u1 u2 ->
>       let
>         h = case r of
>           Left err -> Left err
>           Right (a, rest) -> Right ((a, (u1, u2)), rest)
>         in (Consumed u1 u2, h)

Similarly useful is a combinator that forces a parser to report that it did not consume any characters, even if it did alter the stream. This is useful for removing whitespace from the stream without reporting it as "consumed". Compare `ignore` to `lookahead`: both run a parser and report no characters consumed, but `lookahead` returns the original stream while `ignore` returns the altered stream.

> -- | Run a parser against the stream, and if it succeeds,
> --   report success but do not report the consumed range.
> ignore :: (Stream s) => Parser s a -> Parser s ()
> ignore (Parser q) = Parser $ \ref stream ->
>   case q ref stream of
>     (_, Left err)       -> (Declined, Left err)
>     (_, Right (_,rest)) -> (Declined, Right ((), rest))

Recall that a parser is indentation sensitive if, when it consumes characters, there is a predicate that must be satisfied by the reference position and the range of consumed characters in order for the parse to be valid. To impose some discipline on this, we'll wrap this predicate in a type with a human-readable error message.

> -- | Represents an indentation strategy.
> data Indentation = Indentation
>   { relation :: Pos -> (Pos, Pos) -> Bool
>     -- ^ True if the consumed range is valid with respect
>     --   to the reference position.
> 
>   , message :: Pos -> (Pos, Pos) -> String
>     -- ^ Human-readable error message for when the
>     --   indentation is invalid.
>   }

Now the fundamental combinator for building indentation sensitive parsers, `indent`, simply applies this predicate to the result of a successful parse result.

> -- | Run a parser and, if it succeeds, verify that the
> --   consumed range satisfies the given indentation.
> indent
>   :: (Stream s)
>   => Indentation
>   -> Parser s a
>   -> Parser s a
> indent ind (Parser q) = Parser $ \ref stream ->
>   let (c, r) = q ref stream in
>   case c of
>     Declined -> (c, r)
>     Consumed u1 u2 ->
>       case r of
>         Left _ -> (c, r)
>         Right _ ->
>           if relation ind ref (u1,u2)
>             then (c, r)
>             else ( Declined
>                  , Left $ Simply $
>                      UnexpectedIndentation
>                        (message ind ref (u1,u2)) (u1, u2)
>                  )

Constructing `Indentation` values is not difficult, but a little tedious; later on we'll make a little built-in DSL for handling the most common cases.



Errors
------

The basic combinators can do an okay job of reporting useful errors as-is. But the Parsec authors go one step further to provide an explicit error message combinator, which gives much finer control over semantic errors. The `<?>` function tries to run a parser, and if it fails, gives a higher level error message.

> -- | Run a parser, and on failure, attach an error message.
> (<?>) :: (Stream s) => Parser s a -> String -> Parser s a
> (Parser q) <?> msg = Parser $ \stack stream ->
>   let (c, result) = q stack stream in
>     case result of
>       Right value ->
>         (Declined, Right value)
>       Left err ->
>         (Declined, Left $ Because (Note msg (pointer stream)) err)
> 
> infix 0 <?>

For example, we can use `<?>` with `mapM` and `char` to parse specific strings with a better error message.

> string :: (Stream s) => String -> Parser s ()
> string str = mapM_ char str <?> str

So far we've glossed over the details of the `ParseError` type, but now it's time to unpack that. The purpose of an error type for a parser is to give human users relevant information about what went wrong. At the same time, we don't want to expect readers of the errors to know how this parsing library works, since in practice they'll be using some other tool and shouldn't need to care what parsing library it used.

Our errors so far have been pretty simple. We have some _basic_ errors, including those about unexpected characters and indentation. We also have a couple of _compound_ errors, like the unexplained `Because` and `OneOf` and whatever is happening inside the implementation of `<|>`. The compound errors come in two flavors. In `<|>` we have two errors and want to report that both occurred; this takes two errors and mushes them into one. In `<?>` and `wouldSucceed` we have an error but want to annotate it with some extra information. This takes an error and an annotation and produces an error.

This looks like a free algebra, in the universal algebra sense. We have a set of atoms (the basic errors) and some operations (mushing and annotation) and want to construct the "smallest" type that is closed under them. Assume for the moment that "mushing" (combining errors as in `<|>`) has to be associative -- which it does, by the alternative functor laws. Then what we have is a free monoid on the basic errors (under mush) being acted on freely by the set of annotations. (!!!)

We can model this algebra with the following type; `a` is the type of annotations and `e` the type of basic errors.

> -- | Structured error type.
> data Error a e
>   = OneOf [Error a e]     -- ^ List of failure reasons
>   | Because a (Error a e) -- ^ Annotated failure
>   | Simply e              -- ^ Simple failure
>   deriving (Eq, Show)

We can make `Error a e` a monoid, where `OneOf []` is the identity. Multiplication is _almost_ concatenation; recall that the free monoid on `a` is the type of lists of `a`, and multiplication is just concat. The annotations make this a little more complicated for `Error a e`.

> -- | @Error a e@ is a free monoid over @e@ being acted on
> --   freely by @a@.
> instance Semigroup (Error a e) where
>   (OneOf es1) <> (OneOf es2) = OneOf (es1 ++ es2)
>   (OneOf es1) <> y           = case es1 of
>                                  [] -> y
>                                  _  -> OneOf $ es1 ++ [y]
>   x           <> (OneOf es2) = case es2 of
>                                  [] -> x
>                                  _  -> OneOf $ [x] ++ es2
>   x           <> y           = OneOf [x,y]
> 
> instance Monoid (Error a e) where
>   mempty  = OneOf []
>   mappend = (<>)

Now the basic errors are just a roster of the bad things that can happen when we pop a `Char` from the stream, or look at the reference position (to be discussed later).

> -- | Low-level reasons why a parse can fail.
> data BasicError
>   = UnexpectedEOF (Either String (Maybe Char))
>   | UnexpectedChar Char (Maybe Char) Pos
>   | UnexpectedSatisfy Char String Pos
>   | UnexpectedIndentation String (Pos, Pos)
>   | UnexpectedSuccess (Maybe Pos)
>   | UnexpectedDecline (Maybe Pos)
>   | ExpectedBOF Pos
>   | IncompleteParse (Maybe Pos)
>   | Failure String (Maybe Pos)
>   deriving (Eq, Show)

Our annotations come in a couple of flavors.

> -- | Represents a reason why a parse failed, above
> --   the level of unexpected character or EOF.
> data Annotation
>   = Note String (Maybe Pos)   -- ^ Comes from @\<?>@
>   | Lookahead (Maybe Pos)     -- ^ Comes from @wouldFail@
>   | DeclineReason (Maybe Pos) -- ^ Comes from @consume@
>   deriving (Eq, Show)

So the `ParseError` type looks like this:

> -- | Synonym for the type of trees of parse errors.
> type ParseError = Error Annotation BasicError

`ParseError` values are essentially trees, and this structure means they can be very precise.



Permutation Phrases
-------------------

A _permutation phrase_ is a list of words that can be in any order without changing the meaning of the phrase. These phrases are interesting for parsers because they show up in lots of places -- command line arguments, JSON data, and HTML attributes are three examples -- but can be awkward to handle. The paper _Parsing Permutation Phrases_ has a very nice approach to parsing permutation phrases with combinators, and can even deal with optional items. We'll follow along with that paper and with its implementation in Parsec to get similar functionality here. The amount of code needed to do this is surprisingly small, though it does use the `ExistentialQuantification` language extension.

This approach uses a tree of all possible orderings of a list of parsers, which is exponential in size, but uses lazy evaluation to aggressively prune this tree so that only a quadratic amount of it is actually constructed.

> -- | Opaque type representing a parser for permutation
> --   phrases. To construct values of this type, see
> --   @\<$$>@, @\<$?>@, @\<&&>@, and @\<&?>@.
> data Perms s a
>   = Perms (Maybe a) [Branch s a]
> 
> data Branch s a
>   = forall x. Branch (Perms s (x -> a)) (Parser s x)
> 
> perm :: a -> Perms s a
> perm a = Perms (Just a) []

I'm 95% sure these `Functor` instances are legitimate, although the Parsec authors use ad-hoc functions instead of `fmap`. I'm not sure why.

> instance Functor (Perms s) where
>   fmap f (Perms x xs) =
>     Perms (fmap f x) (map (fmap f) xs)
> 
> instance Functor (Branch s) where
>   fmap f (Branch t p) = Branch (fmap (f .) t) p

We have a mini-DSL for building permutation parsers consisting of four combinators: `<$$>`, `<$?>`, `<&&>`, and `<&?>`.  To use these we need a single function accepting one or more arguments that we want to parse in any order. Then we list out the parsers for each argument, prefixing with `<&&>` (if the argument is required) or `<&?>` (if the argument is optional). The first argument is prefixed with `<$$>` or `<$?>`. (This sort of mimics the `Applicative` style.)

> -- | Append a required term to a permutation phrase.
> (<&&>) :: Perms s (a -> b) -> Parser s a -> Perms s b
> t@(Perms u bs) <&&> p =
>   Perms Nothing $ (Branch t p) : map insert bs
>     where
>       insert (Branch w q) = Branch ((fmap flip w) <&&> p) q
> 
> -- | Start a permutation phrase with a required term.
> (<$$>) :: (a -> b) -> Parser s a -> Perms s b
> f <$$> p = perm f <&&> p
> 
> -- | Append an optional term to a permutation phrase.
> (<&?>) :: Perms s (a -> b) -> (a, Parser s a) -> Perms s b
> t@(Perms u bs) <&?> (x,p) =
>   Perms (fmap ($ x) u) $ (Branch t p) : map insert bs
>     where
>       insert (Branch w q) = Branch ((fmap flip w) <&?> (x,p)) q
> 
> -- | Start a permutation phrase with an optional term.
> (<$?>) :: (a -> b) -> (a, Parser s a) -> Perms s b
> f <$?> (x,p) = perm f <&?> (x,p)
> 
> infixl 1 <&&>, <&?>
> infixl 2 <$$>, <$?>

Once we've constructed a permutation phrase, we can convert it to a normal parser with `permute`.

> -- | Convert a permutation phrase to a @Parser@.
> permute :: (Stream s) => Perms s a -> Parser s a
> permute (Perms u bs) = choice $ map branch bs ++ nil
>   where
>     nil = case u of
>       Nothing -> []
>       Just x  -> [return x]
> 
>     branch (Branch w p) = do
>       x <- p
>       f <- permute w
>       return (f x)

Another common situation is that we want terms to be permutable but also separated by a fixed delimiter, like a comma. I'm pretty sure we can't do this in terms of `permute` and the other combinators, so we'll provide a combinator for it here.

> -- | Convert a permutation phrase to a @Parser@, with
> --   terms separated by @psep@.
> permuteSepBy :: (Stream s) => Parser s () -> Perms s a -> Parser s a
> permuteSepBy = psep (pure ())
>   where
>     psep
>       :: (Stream s)
>       => Parser s () -> Parser s () -> Perms s a -> Parser s a
>     psep init sep (Perms u bs)
>       = choice $ map branch bs ++ nil
>       where
>         nil = case u of
>           Nothing -> []
>           Just x  -> [return x]
> 
>         branch (Branch w p) = do
>           init
>           x <- p
>           f <- psep sep sep w
>           return (f x)

Likewise there are cases where the indentation of items 2 through n in the permuted list depend on the indentation of item 1. For instance, the items in the list should be indented to the same column, but we don't know _which_ column until the first item is parsed. We can't do this in terms of `permute` and the other combinators, so we'll provide it here.

> -- | Convert a permutation phrase to a @Parser@, with
> --   all terms indented with respect to the start
> --   position of the first.
> permuteIndent
>   :: (Stream s)
>   => Indentation -> Perms s a -> Parser s a
> permuteIndent ind = pind
>   where
>     pind
>       :: (Stream s)
>       => Perms s a -> Parser s a
>     pind (Perms u bs)
>       = choice $ map branch bs ++ nil
>       where
>         nil = case u of
>           Nothing -> []
>           Just x  -> [return x]
> 
>         branch (Branch w p) = do
>           (x,(u,_)) <- consume p
>           f <- localRef u $ indent ind $ pind2 w
>           return (f x)
> 
>     pind2 :: (Stream s) => Perms s a -> Parser s a
>     pind2 (Perms u bs)
>       = choice $ map branch bs ++ nil
>       where
>         nil = case u of
>           Nothing -> []
>           Just x  -> [return x]
> 
>         branch (Branch w p) = do
>           x <- p
>           f <- pind2 w
>           return (f x)

Finally, combining indentation with an item separator.

> -- | Convert a permutation phrase to a @Parser@, with
> --   terms separated and indented with respect to the
> --   start position of the first.
> permuteIndentSepBy
>   :: (Stream s)
>   => Indentation -- ^ Indentation of items 2 through n
>                  --   with respect to the start position of item 1
>   -> Parser s () -- ^ Separator
>   -> Perms s a
>   -> Parser s a
> permuteIndentSepBy ind = pindsep (pure ())
>   where
>     pindsep
>       :: (Stream s)
>       => Parser s () -> Parser s () -> Perms s a -> Parser s a
>     pindsep init sep (Perms u bs)
>       = choice $ map branch bs ++ nil
>       where
>         nil = case u of
>           Nothing -> []
>           Just x  -> [return x]
> 
>         branch (Branch w p) = do
>           init
>           (x,(u,_)) <- consume p
>           f <- localRef u $ indent ind $ pindsep2 sep w
>           return (f x)
> 
>     pindsep2
>       :: (Stream s)
>       => Parser s () -> Perms s a -> Parser s a
>     pindsep2 sep (Perms u bs)
>       = choice $ map branch bs ++ nil
>       where
>         nil = case u of
>           Nothing -> []
>           Just x  -> [return x]
> 
>         branch (Branch w p) = do
>           sep
>           x <- p
>           f <- pindsep2 sep w
>           return (f x)

In principle, `permute`, `permuteSepBy`, and `permuteIndent` can all be implemented in terms of `permuteIndentSepBy`, but I think to do so would obscure what's happening.



Pretty Printing
---------------

The error information reported by our parsers is pretty good -- we get a nice tree structure of errors with customizable text. To give them a little more zing and pep we can pretty print the errors to be a little more readable.

> -- | Types which can be pretty printed.
> class Pretty t where
>   pretty :: t -> String
> 
> instance Pretty Pos where
>   pretty (Pos ln col) = concat
>     [ "l", show ln, "c", show col ]

The `Pretty` instance for `Char` makes control characters visible; otherwise they'd be printed verbatim in the error text.

> instance Pretty Char where
>   pretty c = case c of
>     '\n' -> "'\\n' (newline)"
>     '\t' -> "'\\t' (tab)"
>     '\v' -> "'\\v' (vertical tab)"
>     '\r' -> "'\\r' (carriage return)"
>     _ -> ['\'', c, '\'']

We can then pretty print `BasicError`s, recalling what they mean from how they are used.

> instance Pretty BasicError where
>   pretty e = case e of
>     UnexpectedEOF z -> case z of
>       Left str -> unwords
>         [ "expected", str, "but reached end of stream" ]
>       Right c -> case c of
>         Nothing ->
>           "expected beginning of stream, but reached end of stream"
>         Just w -> unwords
>           [ "expected", pretty w, "but reached end of stream" ]
> 
>     UnexpectedChar c z pos -> case z of
>       Nothing -> unwords
>         [ "expected EOF but read", pretty c, "at", pretty pos ]
>       Just d -> unwords
>         [ "expected", pretty d, "but read", pretty c, "at", pretty pos ]
> 
>     UnexpectedSatisfy c name pos -> unwords
>       [ "expected", name, "but read", pretty c, "at", pretty pos ]
> 
>     UnexpectedDecline pos -> case pos of
>       Nothing ->
>         "expected to consume characters but encountered EOF"
>       Just u -> unwords
>         [ "expected to consume characters at", pretty u ]
> 
>     UnexpectedIndentation msg (u1,u2) ->
>       unwords [ "expected", msg ]
> 
>     ExpectedBOF pos -> unwords
>       [ "expected beginning of stream, but found position", pretty pos ]
> 
>     IncompleteParse pos -> case pos of
>       Nothing -> "expected to consume the entire stream"
>       Just u -> unwords
>         [ "expected to consume the entire stream"
>         , "but characters remain at position", pretty u
>         ]
> 
>     Failure msg pos ->
>       let loc = case pos of
>             Nothing -> "end of stream:"
>             Just u -> "at " ++ pretty u ++ ":"
>       in unwords [ loc, msg ]

`Annotation`s are similar.

> instance Pretty Annotation where
>   pretty a = case a of
>     Note msg pos -> case pos of
>       Just z -> unwords [ msg, "at", pretty z ]
>       Nothing -> msg
>     Lookahead pos -> case pos of
>       Just z -> unwords [ "successful lookahead at", pretty z ]
>       Nothing -> "successful lookahead at end of stream"
>     DeclineReason pos -> case pos of
>       Just z -> unwords [ "consumption at", pretty z ]
>       Nothing -> "consume at EOF"

Now `ParseError`s are trees, and we can make the structure of a parse error more clear by formatting it as one. We'll use the helper type `Tree` to represent arbitrary rose trees, and `renderTree` to print them.

> data Tree a
>   = T a [Tree a]
>   deriving Show
> 
> instance Functor Tree where
>   fmap f (T x bs) = T (f x) (map (fmap f) bs)
> 
> renderTree :: Tree String -> String
> renderTree = render . addPrefix
>   where
>     addPrefix :: Tree String -> Tree String
>     addPrefix (T x bs) = T x $ mapLast
>       (mapRoot ("├─ " ++) ("│  " ++))
>       (mapRoot ("└─ " ++) ("   " ++))
>       (map addPrefix bs)
> 
>     mapRoot :: (a -> b) -> (a -> b) -> Tree a -> Tree b
>     mapRoot f g (T x bs) = T (f x) (map (fmap g) bs)
> 
>     mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
>     mapLast f g as = case as of
>       []   -> []
>       x:[] -> [g x]
>       x:xs -> (f x) : mapLast f g xs
> 
>     render :: Tree String -> String
>     render = concat . intersperse "\n" . flatten
> 
>     flatten :: Tree a -> [a]
>     flatten (T x bs) = x : concatMap flatten bs

Now we can pretty print `Error a e`s by converting them to `Tree String`s first.

> instance (Pretty a, Pretty e) => Pretty (Error a e) where
>   pretty = renderTree . toTree
>     where
>       toTree :: (Pretty a, Pretty e) => Error a e -> Tree String
>       toTree err = case err of
>         OneOf es -> case es of
>           [] -> T "unspecified failure :(" []
>           _  -> T "one of the following:" $ map toTree es
>         Because a err ->
>           let
>             msg = unwords
>               [ "expected", pretty a, "which failed due to" ]
>           in T msg [toTree err]
>         Simply e -> T (pretty e) []
> 
> displayParseError :: ParseError -> String
> displayParseError = pretty



An Indentation DSL
------------------

The `Indentation` type for specifying indentation relations is a little too powerful. The vast majority of the time we'll only need one of a small number of different indentation strategies -- indent by some amount, indent an extra $k$ spaces, indent to the same level. To simplify things we'll also provide a mini DSL for constructing indentations of two basic kinds.

The most basic indentation pattern goes something like this: look at the (start|end) (column|line) of a successful parse, comparing it to the corresponding part of (the reference point|some specified point) using (one of a small number of relations). We can wrap up all such indentation strategies behind two functions: `wrtRef` and `wrtPos`.

First we need two helper types: one to represent the "dimensions" of a `Pos`, and one to represent a handful of different relations on the integers.

> -- | Represents either the first or last consumed position of a
> --   successful parse.
> data Endpoint
>   = Start | End
>   deriving (Eq, Show)
> 
> -- | The dimensions of a @Pos@.
> data Dimension
>   = Column | Line
>   deriving (Eq, Show)
> 
> -- | Basic relations on @Integer@.
> data Relation
>   = Eq          -- ^ Equal to
>   | Neq         -- ^ Not equal to
>   | Lt          -- ^ Strictly less than
>   | Gt          -- ^ Strictly greater than
>   | Leq         -- ^ Less than or equal to
>   | Geq         -- ^ Greater than or equal to
>   | Add Integer -- ^ Fixed offset
>   | Fix Integer -- ^ Fixed value
>   deriving (Eq, Show)

Now `wrtRef` takes an endpoint, a dimenstion, and a relation and constructs an indentation strategy against the reference position.

> -- | Constructs a simple indentation strategy with respect to
> --   the reference position.
> wrtRef :: Endpoint -> Dimension -> Relation -> Indentation
> wrtRef pt dim rel = Indentation
>   { relation = \ref range ->
>       getRel rel (getDim dim ref) (getDim dim $ getPt pt range)
>   , message = \ref range ->
>       let
>         labelRef = if compareRef rel
>           then [ "that of the reference position at", pretty ref ]
>           else []
>       in
>         unwords $
>           [ labelPt pt, labelDim dim, "of successful parse, at"
>           , (pretty $ getPt pt range) ++ ",", "to", labelRel rel
>           ] ++ labelRef
>   }

`wrtPos` is almost identical, but rather than the reference position, compares against a specified `Pos` value.

> -- | Constructs a simple indentation strategy with respect to the
> --   given position.
> wrtPos :: Pos -> Endpoint -> Dimension -> Relation -> Indentation
> wrtPos pos pt dim rel = Indentation
>   { relation = \_ range ->
>       getRel rel (getDim dim pos) (getDim dim $ getPt pt range)
>   , message = \_ range ->
>       let
>         labelRef = if compareRef rel
>           then [ "that of position", pretty pos ]
>           else []
>       in
>         unwords $
>           [ labelPt pt, labelDim dim, "of successful parse, at"
>           , (pretty $ getPt pt range) ++ ",", "to", labelRel rel
>           ] ++ labelRef
>   }

These helper functions are only needed by `wrtRef` and `wrtPos`, and aren't part of the library proper.

> getPt :: Endpoint -> (Pos, Pos) -> Pos
> getPt pt = case pt of Start -> fst; End -> snd
> 
> getDim :: Dimension -> Pos -> Integer
> getDim dim = case dim of Column -> column; Line -> line
> 
> getRel :: Relation -> Integer -> Integer -> Bool
> getRel rel = case rel of
>   Eq    -> (==)
>   Neq   -> (/=)
>   Lt    -> (<)
>   Leq   -> (<=)
>   Gt    -> (>)
>   Geq   -> (>=)
>   Add k -> \u v -> v == u+k
>   Fix k -> \_ v -> v == k
> 
> labelPt :: Endpoint -> String
> labelPt pt = case pt of Start -> "start"; End -> "end"
> 
> labelDim :: Dimension -> String
> labelDim dim = case dim of Column -> "column"; Line -> "Line"
> 
> labelRel :: Relation -> String
> labelRel rel = case rel of
>   Eq    -> "equal"
>   Neq   -> "not equal"
>   Lt    -> "be less than"
>   Leq   -> "be less than or equal to"
>   Gt    -> "be greater than"
>   Geq   -> "be greater than or equal to"
>   Add k -> case compare k 0 of
>     GT -> "be exactly " ++ show k ++ " more than"
>     EQ -> "equal"
>     LT -> "be exactly " ++ show (-k) ++ " less than"
>   Fix k -> "be exactly " ++ show k
> 
> compareRef :: Relation -> Bool
> compareRef rel = case rel of Fix _ -> False; _ -> True

Now we can build indentation strategies by saying things like this:

```haskell
-- must start on the same column as the reference position
wrtRef Start Column Eq
  :: Indentation

-- must start on a later line than position u
indent (wrtPos u Start Line Gt)
  :: (Stream s) => Parser s a -> Parser s a
```

These read _almost_ like normal language -- "with respect to the reference, the start column is equal". Fun!



Usage Overview
--------------

Here's the library documentation for inclusion on hackage.

> -- $overview
> --
> -- This library implements monadic parser combinators in the style of
> -- Parsec, but with a couple of tweaks. The major differences are
> -- as follows.
> --
> --   * The @\<|>@ operator from the @Alternative@ class gives the
> --     PEG-style /ordered choice/; @p \<|> q@ attempts @p@, and
> --     if this fails, pretends that @p@ did not consume input and
> --     attempts @q@.
> --
> --   * There is a family of /indentation sensitive/ combinators.
