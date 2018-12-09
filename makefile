all: src test

src: src/Text/ParserCombinators/Munch.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/munch.html $<

test: test/Test.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/munch-test.html $<


.PHONY: all src test