Title: A game of tokens: write an interpreter in Python with TDD - Part 5
Date: 2020-08-09 18:00:00 +0100
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-5
Series: A game of tokens
Image: a-game-of-tokens-5
Summary:

## Introduction

This is part 5 of [A game of tokens]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1.markdown), a series of posts where I build an interpreter in Python following a pure TDD methodology and engaging you in a sort of a game: I give you the tests and you have to write the code that passes them. After part 4 I had a long hiatus because I focused on other projects, but now I resurrected this series and I'm moving on.

First of all I reviewed the first 4 posts, merging the posts that contained the solutions. While this is definitely better for me, I think it might be better for the reader as well, this way it should be easier to follow along. Remember however that you learn if you do, not if you read!

Secondly, I was wondering in which direction to go, and I decided to shamelessly follow the steps of Ruslan Spivak, who first inspired this set of posts and who set off to build an Pascal interpreter; you can find the impressive series of posts Ruslan wrote on [his website](https://ruslanspivak.com). Thank you Ruslan for the great posts!

So, let's go Pascal!

## Tools update

I introduced black into my development toolset, so I used it to reformat the code

``` sh
black smallcalc/*.py tests/*.py
```

And added a configuration file `.flake8` for Flake8 to avoid the two tools to clash

``` ini

[flake8]
# Recommend matching the black line length (default 88),
# rather than using the flake8 default of 79:
max-line-length = 100
ignore = E231 E741
```

## Level 17 - Reserved keywords and new assignment

Since Pascal has reserved keywords, I need tokens that have the keyword itself as value (something similar to Erlang's atoms). For this reason I changed `test_empty_token_has_length_zero` into

``` python
def test_empty_token_has_the_length_of_the_type_itself():
    t = token.Token("sometype")

    assert len(t) == len("sometype")
    assert bool(t) is True
```

and modified the code in the class `Token` to pass it

``` python
   def __len__(self):
        return len(self.value) if self.value else len(self.type)
```

The keywords I will introduce in this post are `BEGIN` and `END`, so I need a test that shows they are supported

``` python
def test_get_tokens_understands_begin_and_end():
    l = clex.CalcLexer()

    l.load("BEGIN END")

    assert l.get_tokens() == [
        token.Token(clex.BEGIN),
        token.Token(clex.END),
        token.Token(clex.EOL),
        token.Token(clex.EOF),
    ]
```

The block `BEGIN ... END` is a generic compound block in Pascal (more on this later), and a Pascal program is made of that plus a final dot. Since the dot is already used for floats I need a test that shows it is correctly lexed.

``` python
def test_get_tokens_understands_final_dot():
    l = clex.CalcLexer()

    l.load("BEGIN END.")

    assert l.get_tokens() == [
        token.Token(clex.BEGIN),
        token.Token(clex.END),
        token.Token(clex.DOT),
        token.Token(clex.EOL),
        token.Token(clex.EOF),
    ]
```

Last, Pascal assignments are sligthly different from what we already implemented, as they use the symbol `:=` instead of just `=`. We face a choice here, as we have to decide where to put the logic of our programming language: shall the lexer identify `:` and `=` separately, and let the parser deal with the two tokens in sequence, or shall we make the lexer emit an `ASSIGNMENT` token directly? I went for the first one, so that the lexer can be kept simple (no lookahead in it), but you are obviously free to try something different. For me the test that checks the assignment is

``` python
def test_get_tokens_understands_assignment_and_semicolon():
    l = clex.CalcLexer()

    l.load("a := 5;")

    assert l.get_tokens() == [
        token.Token(clex.NAME, "a"),
        token.Token(clex.LITERAL, ":"),
        token.Token(clex.LITERAL, "="),
        token.Token(clex.INTEGER, "5"),
        token.Token(clex.LITERAL, ";"),
        token.Token(clex.EOL),
        token.Token(clex.EOF),
    ]
```

You may have noticed I also decided to check for the semicolon in this test. Even here, we might discuss if it's meaningful to test two different things together, and generally speaking I'm in favour of a high granularity in tests, which however means that I try to avoid testing _unrelated_ and _complicated_ features together. In Pascal, the semicolon is used to separate statements, so it is likely be found at the end of something like an assignment. For this reason, and considering that it's a small feature, I put it in a context inside this test, and will extract it if more complex requirements arise in the future.

The parser has to be changed to support the new assignment, and to do that we first need to change the tests. The symbol `=` has to be replaced with `:=` in the following tests: `test_parse_assignment`, `test_parse_assignment_with_expression`, `test_parse_assignment_expression_with_variables`, and `test_parse_line_supports_assigment`.

### Solution

Supporting reserved keywords is just a matter of defining specific token types for them

``` python
BEGIN = "BEGIN"
DOT = "DOT"

RESERVED_KEYWORDS = [BEGIN, END]
```

and changing the method `_process_name` in order to detect them

``` python
def _process_name(self):
	regexp = re.compile(r"[a-zA-Z_]+")

	match = regexp.match(self._text_storage.tail)
	
	if not match:
		return None

	token_string = match.group()

	if token_string in RESERVED_KEYWORDS:
		tok = token.Token(token_string)
	else:
		tok = token.Token(NAME, token_string)
	
	return self._set_current_token_and_skip(tok)
```

I decided to put the logic in this method because after all reserved keywords are exactly names with a specific meaning. I might have created a dedicated method `_process_keyword` but it would basically have been a copy of `_process_name` so this solution makes sense to me.

To support the final dot I added a token for it

``` python
DOT = "DOT"
```

and a processing method

``` python
   def _process_dot(self):
        regexp = re.compile(r"\.$")

        match = regexp.match(self._text_storage.tail)

        if match:
            return self._set_current_token_and_skip(token.Token(DOT))
```

which is then introduced with a high priority in `get_token`

``` python
    def get_token(self):
        eof = self._process_eof()
        if eof:
            return eof

        eol = self._process_eol()
        if eol:
            return eol

        dot = self._process_dot()
        if dot:
            return dot

        self._process_whitespace()

        name = self._process_name()
        if name:
            return name

        number = self._process_number()
        if number:
            return number

        literal = self._process_literal()
        if literal:
            return literal
```

To pass the parser tests I just need to change the implementation of `parse_assignment`

``` python
def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard(token.Token(clex.LITERAL, ":"))
        self.lexer.discard(token.Token(clex.LITERAL, "="))
        value = self.parse_expression()
```

## Level 18 - Statements and compound statements

In Pascal a compound statement is a list of statements enclosed between `BEGIN` and `END`, so the final grammar we want to have in this post is

``` text
compound_statement : BEGIN statement_list END

statement_list : statement | statement SEMI statement_list

statement : compound_statement | assignment_statement | empty

assignment_statement : variable ASSIGN expr
```

As you can see this is a recursive definition, as the `statement_list` contains one or more `statement`, and each of them can be a `compound_statement`. The following is indeed a valid Pascal program

``` pascal
BEGIN
	BEGIN
		BEGIN
			writeln("Valid!")
		END
	END
END.
```

Recursive algorithms are not simple, and it takes some time to tackle them properly. Let's try to implement one small feature at a time. The first test is that `parse_statement` should be able to parse assignments

``` python
def test_parse_statement_assignment():
    p = cpar.CalcParser()
    p.lexer.load("x := 5")

    node = p.parse_statement()

    assert node.asdict() == {
        "type": "assignment",
        "variable": "x",
        "value": {"type": "integer", "value": 5},
    }
```

In future, statements will be more than just assignments, so this test is the first of many others that we will eventually have for `parse_statement`. The second test we need is that a compound statement can contain an empty list of statements.

``` python
def test_parse_empty_compound_statement():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN END")

    node = p.parse_compound_statement()

    assert node.asdict() == {"type": "compound_statement", "statements": []}
```

After this is done, I want to test that the compound statement can contains one single statement

``` python
def test_parse_compound_statement_one_statement():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN x:= 5 END")

    node = p.parse_compound_statement()

    assert node.asdict() == {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            }
        ],
    }
```

and multiple statements separated by semicolon

``` python
def test_parse_compound_statement_multiple_statements():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN x:= 5; y:=6; z:=7 END")

    node = p.parse_compound_statement()

    assert node.asdict() == {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            },
            {
                "type": "assignment",
                "variable": "y",
                "value": {"type": "integer", "value": 6},
            },
            {
                "type": "assignment",
                "variable": "z",
                "value": {"type": "integer", "value": 7},
            },
        ],
    }
```

### Solution

To pass the first test it is sufficient to add a method `parse_statement` that calls `parse_assignment`

``` python
    def parse_statement(self):
        with self.lexer:
            return self.parse_assignment()
```

The second test requires a bit more code. I need to define a method `parse_compound_statement` and this has to return a specific new type of node. A compound statement is s list of statements that have to be executed in order, so it's time to define a class `CompoundStatementNode`

``` python
class CompoundStatementNode(Node):

    node_type = "compound_statement"

    def __init__(self, statements=None):
        self.statements = statements if statements else []

    def asdict(self):
        return {
            "type": self.node_type,
            "statements": [statement.asdict() for statement in self.statements],
        }
```

and at this point `parse_compound_statement` is trivial, at least for now

``` python
    def parse_compound_statement(self):
        self.lexer.discard(token.Token(clex.BEGIN))
        self.lexer.discard(token.Token(clex.END))

        return CompoundStatementNode()
```

With the third test we have to add the processing of a single statement. As this is optional, it's a good use case for our lexer as a context manager

``` python
    def parse_compound_statement(self):
        nodes = []

        self.lexer.discard(token.Token(clex.BEGIN))

        with self.lexer:
            statement_node = self.parse_statement()
            if statement_node:
                nodes.append(statement_node)

        self.lexer.discard(token.Token(clex.END))

        return CompoundStatementNode(nodes)
```

And finally, for the fourth test, I have to process optional further statements separated by semicolons. For this, I make use of the method `peek_token` to look ahead and see if there is another statement to process

``` python
    def parse_compound_statement(self):
        nodes = []

        self.lexer.discard(token.Token(clex.BEGIN))

        with self.lexer:
            statement_node = self.parse_statement()
            if statement_node:
                nodes.append(statement_node)

            while self.lexer.peek_token() == token.Token(clex.LITERAL, ";"):
                self.lexer.discard(token.Token(clex.LITERAL, ";"))

                statement_node = self.parse_statement()

                if statement_node:
                    nodes.append(statement_node)

        self.lexer.discard(token.Token(clex.END))

        return CompoundStatementNode(nodes)
```

## Level 19 - Recursive compound statements

To verify that compound statements are actually recursive, we can add this test

``` python
def test_parse_compound_statement_multiple_statements_with_compund_statement():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN x:= 5; BEGIN y := 6 END ; z:=7 END")

    node = p.parse_compound_statement()

    assert node.asdict() == {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            },
            {
                "type": "compound_statement",
                "statements": [
                    {
                        "type": "assignment",
                        "variable": "y",
                        "value": {"type": "integer", "value": 6},
                    }
                ],
            },
            {
                "type": "assignment",
                "variable": "z",
                "value": {"type": "integer", "value": 7},
            },
        ],
    }
```

where the second statement is a compound statement itself. After this is done we can test the visitor (`tests/test_calc_visitor.py`) and see if we can process single statements

``` python
def test_visitor_compound_statement_one_statement():
    ast = {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            }
        ],
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) is None
    assert v.isvariable("x") is True
    assert v.valueof("x") == 5
    assert v.typeof("x") == "integer"
```

Multiple statements

``` python
def test_visitor_compound_statement_multiple_statements():
    ast = {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            },
            {
                "type": "assignment",
                "variable": "y",
                "value": {"type": "integer", "value": 6},
            },
            {
                "type": "assignment",
                "variable": "z",
                "value": {"type": "integer", "value": 7},
            },
        ],
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) is None

    assert v.isvariable("x") is True
    assert v.valueof("x") == 5
    assert v.typeof("x") == "integer"

    assert v.isvariable("y") is True
    assert v.valueof("y") == 6
    assert v.typeof("y") == "integer"

    assert v.isvariable("z") is True
    assert v.valueof("z") == 7
    assert v.typeof("z") == "integer"
```

and recursive compound statements

``` python
def test_visitor_compound_statement_multiple_statements_with_compund_statement():
    ast = {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            },
            {
                "type": "compound_statement",
                "statements": [
                    {
                        "type": "assignment",
                        "variable": "y",
                        "value": {"type": "integer", "value": 6},
                    }
                ],
            },
            {
                "type": "assignment",
                "variable": "z",
                "value": {"type": "integer", "value": 7},
            },
        ],
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) is None

    assert v.isvariable("x") is True
    assert v.valueof("x") == 5
    assert v.typeof("x") == "integer"

    assert v.isvariable("y") is True
    assert v.valueof("y") == 6
    assert v.typeof("y") == "integer"

    assert v.isvariable("z") is True
    assert v.valueof("z") == 7
    assert v.typeof("z") == "integer"
```

### Solution

Before I added the first test I quickly refactored the code to follow the grammar a bit more closely, introducing `parse_statement_list` and calling it from `parse_compound_statement`. This is just a matter of isolating the part of the code that deals with the list of statements in its own method

``` python
    def parse_statement_list(self):
        nodes = []

        statement_node = self.parse_statement()
        if statement_node:
            nodes.append(statement_node)

        while self.lexer.peek_token() == token.Token(clex.LITERAL, ";"):
            self.lexer.discard(token.Token(clex.LITERAL, ";"))

            statement_node = self.parse_statement()

            if statement_node:
                nodes.append(statement_node)

        return nodes

    def parse_compound_statement(self):
        nodes = []

        self.lexer.discard(token.Token(clex.BEGIN))

        with self.lexer:
            nodes = self.parse_statement_list()

        self.lexer.discard(token.Token(clex.END))

        return CompoundStatementNode(nodes)
```

after this I introduce the new test, and to pass it I need to change `parse_statement` so that it parses either an assignment or a compound statement

``` python
    def parse_statement(self):
        with self.lexer:
            return self.parse_assignment()

        return self.parse_compound_statement()
```

Before I move to the visitor, I want to discuss a choice that I have here. The current version of the method `parse_statement_list`

``` python
    def parse_statement_list(self):
        nodes = []

        statement_node = self.parse_statement()
        if statement_node:
            nodes.append(statement_node)

        while self.lexer.peek_token() == token.Token(clex.LITERAL, ";"):
            self.lexer.discard(token.Token(clex.LITERAL, ";"))

            statement_node = self.parse_statement()

            if statement_node:
                nodes.append(statement_node)

        return nodes
```

might be easily written in a recursive way, to better match the grammar, becoming

``` python
    def parse_statement_list(self):
        nodes = []

        statement_node = self.parse_statement()
        if statement_node:
            nodes.append(statement_node)

        with self.lexer:
            self.lexer.discard(token.Token(clex.LITERAL, ";"))
            nodes.extend(self.parse_statement_list())

        return nodes
```

As you can see if you replace the code all the test pass, so the solution is technically correct. While recursive algorithms are elegant and compact, however, in this case I will stick to the first version. Using a recursive approach introduces a limit to the number of calls, and while in this little project we won't probably have this issue, I think it is worth mentioning it. Both solutions are correct, though, so feel free to choose the recursive path if you happen to like it more.

The tests for the visitor can be passed with a minimal change, as the visitor itself just needs to be aware of `compound_statement` nodes and to know how to process them. So, I added a new condition to the method `visit`

``` python
        if node["type"] == "compound_statement":
            [self.visit(node) for node in node["statements"]]
```

which passes all the three new tests added for the visitor.

## Level 20 - Pascal programs and case insensitive names

A Pascal program ends with a dot, so we should introduce a new endpoint `parse_program` and test that it works. The first test verifies that we can parse an empty program

``` python
def test_parse_empty_program():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN END.")

    node = p.parse_program()

    assert node.asdict() == {"type": "compound_statement", "statements": []}
```

and the second tests that the final dot can't be missing

``` python
import pytest

from smallcalc.calc_lexer import TokenError


def test_parse_program_requires_the_final_dot():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN END")

    with pytest.raises(TokenError):
        p.parse_program()
```

Notice that I imported `pytest` and the `TokenError` exception to build a negative test (i.e. to test something that fails). The last test verifies a non-empty program can be parsed

``` python
def test_parse_program_with_nested_statements():
    p = cpar.CalcParser()
    p.lexer.load("BEGIN x:= 5; BEGIN y := 6 END ; z:=7 END.")

    node = p.parse_program()

    assert node.asdict() == {
        "type": "compound_statement",
        "statements": [
            {
                "type": "assignment",
                "variable": "x",
                "value": {"type": "integer", "value": 5},
            },
            {
                "type": "compound_statement",
                "statements": [
                    {
                        "type": "assignment",
                        "variable": "y",
                        "value": {"type": "integer", "value": 6},
                    }
                ],
            },
            {
                "type": "assignment",
                "variable": "z",
                "value": {"type": "integer", "value": 7},
            },
        ],
    }
```

When all these tests pass we are almost done for this post, and we just need to make the parser treat names in a case insensitive way. In Pascal, both variables and keywords are case-insensitive, so `BEGIN` and `begin` are the same keyword (or `BeGiN`, though I think this might be a misinterpretation of the concept of "snake case" =) ), and the same is valid for variables: you can define `MYVAR` and use `myvar`.

To test this behaviour I changed the test `test_get_tokens_understands_uppercase_letters` into `test_get_tokens_is_case_insensitive`

``` python
def test_get_tokens_is_case_insensitive():
    l = clex.CalcLexer()

    l.load("SomeVar")

    assert l.get_tokens() == [
        token.Token(clex.NAME, "somevar"),
        token.Token(clex.EOL),
        token.Token(clex.EOF),
    ]
```

and added the test for the two keywords we defined so far

``` python
def test_get_tokens_understands_begin_and_end_case_insensitive():
    l = clex.CalcLexer()

    l.load("begin end")

    assert l.get_tokens() == [
        token.Token(clex.BEGIN),
        token.Token(clex.END),
        token.Token(clex.EOL),
        token.Token(clex.EOF),
    ]
```

### Solution

To parse a program we need to introduce the aptly named endpoint `parse_program`, which just parses a compound statement (the program) and the final dot.


``` python
    def parse_program(self):
        compound_statement = self.parse_compound_statement()
        self.lexer.discard(token.Token(clex.DOT))

        return compound_statement
```

As for the case insensitive names, it's just a matter of changing the method `_process_name`

``` python
    def _process_name(self):
        regexp = re.compile(r"[a-zA-Z_]+")

        match = regexp.match(self._text_storage.tail)

        if not match:
            return None

        token_string = match.group()

        if token_string.upper() in RESERVED_KEYWORDS:
            tok = token.Token(token_string.upper())
        else:
            tok = token.Token(NAME, token_string.lower())

        return self._set_current_token_and_skip(tok)
```

Note that I decided to keep internally keywords with uppercase names and variables with lowercase ones. This is really just a matter of personal taste at this point of the project (and probably will always be), so feel free to follow the structure you like the most.

## Final words

That was something! I was honestly impressed by how easily I could introduce changes in the language and add new feature, a testimony that the TDD methodology is a really powerful tool to have in your belt. Thanks again to [Ruslan Spivak](https://ruslanspivak.com/pages/about/) for his work and his inspiring posts!

The code I developed in this post is available on the GitHub repository tagged with `part5` ([link](https://github.com/lgiordani/smallcalc/tree/part5)).

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

