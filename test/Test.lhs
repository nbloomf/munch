---
title: Munch Examples
---

In [Munch](/Munch.html) we built a little parser combinator library. Now let's test it.

This module consists of a list of parsers, together with lists of _good_ inputs (with expected outputs) and _bad_ inputs (with expected errors). These examples form an automated test suite for the library.

First the usual compiler noises.

> module Main (main) where
> 
> import Text.ParserCombinators.Munch
> import Control.Applicative
> import System.Exit (exitFailure)



Test Helpers
------------

Although the Munch library can operate on any type implementing the `Token` class, these examples will only test the built-in `Char` instance.

> runCharParse
>   :: Parser Char a
>   -> String
>   -> Either (ParseError Char) a
> runCharParse q str =
>   runParser q (toStream str)

`parse_results` is a helper function taking a parser and a list of good inputs and outputs; for each example it checks the actual output against the expectation. The entire suite fails if any example fails.

> parse_results
>   :: (Eq a, Show a) => Parser Char a -> [(String, a)] -> IO ()
> parse_results = mapM_ . parse_result_is
>   where
>     parse_result_is
>       :: (Eq a, Show a) => Parser Char a -> (String, a) -> IO ()
>     parse_result_is q (str, x) =
>       case runCharParse q str of
>         Left err -> do
>           putStrLn $ '\n' : displayParseError err
>           exitFailure
>         Right a -> if a == x
>           then return ()
>           else do
>             putStrLn "=> Value Error!"
>             putStrLn "=> Expected:"
>             putStrLn $ show x
>             putStrLn "=> Actual:"
>             putStrLn $ show a
>             exitFailure

Similarly, `parse_failures` takes a parser and a list of bad inputs and outputs, failing if actual output doesn't match.

> parse_failures
>   :: (Eq a, Show a)
>   => Parser Char a -> [(String, ParseError Char)] -> IO ()
> parse_failures = mapM_ . parse_failure_is
>   where
>     parse_failure_is
>       :: (Eq a, Show a)
>       => Parser Char a -> (String, ParseError Char) -> IO ()
>     parse_failure_is q (str, fail) =
>       case runCharParse q str of
>         Right a -> do
>           putStrLn $ '\n' : "Expected failure:"
>           putStrLn $ show a
>           exitFailure
>         Left err -> if err == fail
>           then return ()
>           else do
>             putStrLn "=> Error Error!"
>             putStrLn "=> Expected:"
>             putStrLn $ show fail
>             putStrLn "=> Actual:"
>             putStrLn $ show err
>             exitFailure

Now `test_parser` wraps all this up, running good and bad tests for a given parser and reporting the result.

> test_parser
>   :: (Eq a, Show a)
>   => String -- ^ Test label
>   -> Parser Char a
>   -> [(String, a)] -- ^ Good cases
>   -> [(String, ParseError Char)] -- ^ Bad cases
>   -> IO ()
> test_parser name p oks fails = do
>   putStrLn $ "==> " ++ name
>   parse_results p oks
>   parse_failures p fails
>   let k = length oks + length fails
>   putStrLn $ "OK: " ++ show k ++ " cases"



The Tests
---------

Let's start simple with `char`. There's only one way this parser can succeed, and a few ways it can fail.

> p_00 = char 'a'
> 
> ok_00 =
>   [ ("a", 'a')
>   ]
> fail_00 =
>   [ ("ab", Simply $ IncompleteParse $ Just (Pos 1 2))
>   , ("b", Simply $ UnexpectedToken 'b' (Just 'a') (Pos 1 1))
>   , ("", Simply $ UnexpectedEOF $ Right (Just 'a'))
>   ]

Next is `anyToken`.

> p_01 = anyToken
> 
> ok_01 =
>   [ ("a", 'a')
>   , ("\n", '\n')
>   ]
> fail_01 =
>   [ ("", Simply $ UnexpectedEOF $ Left "any token")
>   , ("ab", Simply $ IncompleteParse $ Just (Pos 1 2))
>   ]

Now to try `many`. Note the failure case `"a"` here; `many decimalDigit` looks for 0 or more decimal digit, and finds none. This is a successful parse. Control then passes `"a"` to `eof`, which fails when it encounters `'a'`. So the actual failure message is raised by `eof`.

> p_02 = many decimalDigit <* eof
> 
> ok_02 =
>   [ ("1", "1")
>   , ("463", "463")
>   , ("", "")
>   ]
> fail_02 =
>   [ ("a", Simply $ UnexpectedToken 'a' Nothing (Pos 1 1))
>   ]

Here's a test using `<|>` and `<?>`. Note the difference in behavior between this parser and the last that arises because we're using `>>` rather than `<*`.

> p_03 = (smol <|> big) >> eof
>   where
>     smol = some lowerLatin <?> "little word"
>     big  = some upperLatin <?> "big word"
> 
> ok_03 =
>   [ ("abc", ())
>   , ("ABC", ())
>   ]
> fail_03 =
>   [ ("abcD", Simply $ UnexpectedToken 'D' Nothing (Pos 1 4))
>   , ( ""
>     , OneOf
>         [ Because (Note "little word" Nothing)
>           (Simply (UnexpectedEOF
>           (Left "lower case latin letter (a-z)")))
>         , Because (Note "big word" Nothing)
>           (Simply (UnexpectedEOF
>           (Left "upper case latin letter (A-Z)")))
>         ]
>     )
>   , ( "5"
>     , OneOf
>         [ Because (Note "little word" (Just (Pos 1 1)))
>           (Simply $ UnexpectedSatisfy '5'
>           "lower case latin letter (a-z)" (Pos 1 1))
>         , Because (Note "big word" (Just (Pos 1 1)))
>           (Simply $ UnexpectedSatisfy '5'
>           "upper case latin letter (A-Z)" (Pos 1 1))
>         ]
>     )
>   ]

Here's a parser for a simplified telephone number format. These numbers have three ternary digits with an optional one-digit area code, and are formatted as either `(N) N-NN` or `N-NN`.

> p_04 = (no_area_code <|> with_area_code) >> eof
>   where
>     no_area_code =
>       digit >> char '-' >> digit >> digit
> 
>     with_area_code =
>       char '(' >> digit >> char ')' >> char ' ' >> no_area_code
> 
>     digit =
>       satisfies (\c -> elem c ['0','1','2']) "ternary digit"
> 
> ok_04 =
>   [ ("1-11", ())
>   , ("(2) 1-01", ())
>   ]
> 
> fail_04 =
>   [ ( "1-13"
>     , OneOf
>         [ Simply $ UnexpectedSatisfy '3' "ternary digit" (Pos 1 4)
>         , Simply $ UnexpectedToken '1' (Just '(') (Pos 1 1)
>         ]
>     )
>   ]

Here's a parser testing positive and negative lookahead.

> p_05 = do
>   wouldSucceed $ some (char 'a' <|> char 'b')
>   some (char 'a' <|> char 'b')
>   wouldFail $ char '%'
>   many (char '_')
>   return ()
> 
> ok_05 =
>   [ ("a", ())
>   , ("babbab____", ())
>   ]
> 
> fail_05 =
>   [ ( "c"
>     , Because (Lookahead (Just (Pos {line = 1, column = 1})))
>         (OneOf
>           [ Simply (UnexpectedToken 'c' (Just 'a') (Pos 1 1))
>           , Simply (UnexpectedToken 'c' (Just 'b') (Pos 1 1))
>           ])
>     )
>   , ( "%"
>     , Because (Lookahead (Just (Pos {line = 1, column = 1})))
>         (OneOf
>           [ Simply (UnexpectedToken '%' (Just 'a') (Pos 1 1))
>           , Simply (UnexpectedToken '%' (Just 'b') (Pos 1 1))
>           ])
>     )
>   , ( "aabba%"
>     , Simply (UnexpectedSuccess (Just (Pos 1 6)))
>     )
>   ]

Now some basic indentation.

> p_06 = do
>   localRef (Pos 1 5) $ do
>     ignore spaces
>     indent (wrtRef Start Column Eq) $ char 'a'
>   eof
> 
> ok_06 =
>   [ ("    a", ())
>   ]
> 
> fail_06 =
>   [ ( "a"
>     , Simply (UnexpectedIndentation
>         ("start column of successful parse, at l1c1, to " ++
>          "equal that of the reference position at l1c5")
>         (Pos 1 1, Pos 1 1))
>     )
>   ]

And a permutation.

> p_07 =
>   let f x y z = [x,y] ++ z in
>   permute (f <$$> char 'a' <&?> ('_', char 'b') <&&> some (char 'c'))
> 
> ok_07 =
>   [ ("abc", "abc")
>   , ("cca", "a_cc")
>   ]
> 
> fail_07 =
>   [ ( "bacd"
>     , Simply (IncompleteParse (Just (Pos 1 4)))
>     )
>   ]

Now for something a little more complicated. Here's a simple rose tree type. We'll use indentation to denote levels in the tree; child nodes are indented by two extra spaces.

> data Tree = B String [Tree]
>   deriving (Eq, Show)
> 
> p_08 = do
>   (b,(u,_)) <- ignore spaces *> consume (some lowerLatin) <* newline
>   ts <- many $ indent (wrtPos u Start Column (Add 2)) p_08
>   return $ B b ts
> 
> ok_08 =
>   [ ( concat
>         [ "a\n"
>         ]
>     , B "a" []
>     )
>   , ( concat
>         [ "a\n"
>         , "  b\n"
>         , "  c\n"
>         ]
>     , B "a" [B "b" [], B "c" []]
>     )
>   ]
> 
> fail_08 =
>   [ ( concat
>         [ "a\n"
>         , " b\n"
>         ]
>     , Simply (IncompleteParse (Just (Pos 2 1)))
>     )
>   ]





Main
----

> main :: IO ()
> main = do
>   putStrLn ""
> 
>   test_parser "char"
>     p_00 ok_00 fail_00
> 
>   test_parser "anyChar"
>     p_01 ok_01 fail_01
> 
>   test_parser "many decimalDigit <* eof"
>     p_02 ok_02 fail_02
> 
>   test_parser "little or big word"
>     p_03 ok_03 fail_03
> 
>   test_parser "ternary phone numbers"
>     p_04 ok_04 fail_04
> 
>   test_parser "speculation"
>     p_05 ok_05 fail_05
> 
>   test_parser "simple indentation"
>     p_06 ok_06 fail_06
> 
>   test_parser "simple permutation"
>     p_07 ok_07 fail_07
>
>   test_parser "indented trees"
>     p_08 ok_08 fail_08
