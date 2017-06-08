Title: A game of tokens: write an interpreter in Python with TDD - Part 2
Date: 2017-06-07 23:00:00 +0100
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2
Summary: 

My solution to the [first part]() can be found [here](https://github.com/lgiordani/smallcalc/tree/part1). Remember that this is only one possible solution, and that in giving you the tests I already biased the whole project towards my implementation of choice.

If you review the whole `part1` branch commit by commit (`git log --oneline`), you can see how I developed the solution following the previous post

``` txt
5d842de Visitor: multiple operations are correctly handled
040f199 Parser: multiple operations are correctly handled
021c770 Lexer: added peek_token() method
2eb2045 Lexer: added stash() and pop() methods
85e9941 Lexer: multiple operations are correctly handled
3eb500f Visitor: minus sign accepted
3755fdb Parser: minus sign accepted
7b56f7b Lexer: minus sign accepted
01f6573 Lexer: added whitespace processing
68efbe2 Lexer: added support for multidigit integers
4078ae7 CLI: parse_expression() is the new entry point
5dae60f Visitor: added sum of unspaced integers
92cefa2 Parser: added sum of unspaced integers
7b79ca9 Lexer: added sum of unspaced integers
3a295f0 CLI: parse_integer() is the new entry point
16a5965 Visitor: added single-digit integer support
f4ef1ec Parser: added single-digit integer support
2f19b09 Lexer: added single-digit integer support
1c6d00e Lexer: process EOF
819ec02 Initial version of the CLI added
```

Remember that you can get the difference between two commits with `git diff [oldcommit]..[newcommit]` (e.g. `git diff 819ec02..1c6d00e`) or that you can just `checkout` the commit to play around with the code.

