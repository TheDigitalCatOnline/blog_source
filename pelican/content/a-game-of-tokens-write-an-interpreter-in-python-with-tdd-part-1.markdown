Title: A game of tokens: write an interpreter in Python with TDD - Part 1
Date: 2017-05-09 23:00:00 +0100
Modified: 2020-08-05 11:00:00 +0000
Category: Programming
Tags: pytest, Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1
Series: A game of tokens
Series_info: Python, Ruby, Javascript, C, Erlang, how many different languages. But how does a compiler work? How is the source code converted into something that works? To understand that there's nothing better than actually write a language. Let's write a simple language interpreter in Python using TDD!
Image: a-game-of-tokens-1
Summary: How to write a programming language in Python, a TDD game

## Introduction

Writing an interpreter or a compiler is usually considered one of the greatest goals that a programmer can achieve, and with good reason. I do not believe the importance of going through this experience is primarily due to its difficulty, though. After all, writing an efficient compiler is difficult, but the same is true for a good web framework, or a feature-rich editor.

Being able to write an interpreter is a significant skill mainly because of its recursive (or self-referring) nature. Think about it: you use a language to write a new language. And this new language, if it becomes sufficiently rich, can eventually be used to create its own compiler.

**A language can be used to write the program that executes that same language.**

Didn't this last sentence fire you with enthusiasm? It makes me eager to start!

Compilers have been the subject of academic research since the 50s, with the works of [Hopper](https://en.wikipedia.org/wiki/Grace_Hopper) and [Glennie](https://en.wikipedia.org/wiki/Alick_Glennie), so trying to provide an overview in a few lines is basically impossible. I highly recommend you to check the online resources listed at the bottom of the post if you are seriously interested in the matter.

In this series of posts I want to try an experiment. I want to guide you through the creation of a simple interpreter in Python using a pure TDD (Test-Driven Development) approach. The posts will be structured like a game, where every level is represented by a new test that I will add to the suite. If you are not confident with TDD, you will find more on it in the specific section.

Following this series you will learn about Python, compilers, interpreters, parsers, lexers, test-driven development, refactoring, coverage, regular expressions, classes, context managers. Wow, that's a lot!

Are you ready to start?

## On the TDD game

This series of posts will introduce you to TDD with a sort of game. I'll give you the test, and you are supposed to write something that passes that test, finishing the level. **Update**: I decided to move solutions into the same post where the challenge is given, you will find them in specific sections named "Solution" after each level.

My best advice for the TDD game is: remember that the easiest solution for a test that requires the output `A` is to write a function that returns exactly `A`.

**Beautiful is better than ugly, but ugly and tested is better than beautiful and untested.**

## About the language

At the time of writing the language we are going to implement is a simple calculator with support for **integer** and **floats**, **binary operators** (addition, subtraction, multiplication, division, and power), **unary operators** (negation), **nested expressions** (parentheses) and **variables**.

The name **smallcalc** is a homage to one of the most innovative and influential languages ever conceived: [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk).

I do not know if the final version will be something richer, it depends on how much fun you will find in the series. So, if you are interested, just ask! You can drop a line of appreciation [on Twitter](https://twitter.com/thedigicat).

At the time of writing, then, the language grammar is

``` text
factor : ('+' | '-') factor | '(' expression ')' | variable | number
power : factor [ '^' power ]*
term : power [ ('*' | '/') term ]*
expression : term [ ('*' | '/') expression ]*
assignment : variable '=' expression
line : assignment | expression
```

The syntax of the grammar is pretty self-explanatory if you have some programming background. If you want to know more about grammars like the one above start from the links in the resources section.

## TDD and refactoring

If you already know what TDD is feel free to skip this section.

**TDD** means **Test-Driven Development**, and in short it is a programming methodology that requires you to write a test for a feature before implementing the feature itself. Much has been said on the benefits of TDD elsewhere. I personally think it is one of the most effective ways to work on a programming task, and something that every programmer should know. I wrote a post on TDD with Python that you can find [here]({filename}python-oop-tdd-example-part1.markdown).

A **test**, in TDD, is code that uses the code you are going to develop. You will start with a project skeleton and add the tests I will present in the posts one at a time. Once you add the test, you have to write the code that passes the test. Your code doesn't need to be beautiful or smart, it just needs to pass the test. Then you can move to the following test and start the cycle again.

After adding some tests you can start considering **refactoring**, which means changing the existing code in order to make it more beautiful, simpler or better organised. Every change has to be tested against the existing battery of tests. If the tests do not fail your change is correct, at least in terms of the behaviour that the tests are checking.

**Coverage** is the check of how much of your code is covered by your tests. We call some code "covered" by a test if executing the test makes that code run. So, for example, if you have a test (an `if` block) you should write two tests. One to cover the first option, and another to cover the second one. If you work with a strict TDD methodology your coverage is going to be always 100%, because you wrote just the code that makes the tests pass.

You can find more on TDD on this blog [here](/categories/tdd/).

## About the project

The main components of our interpreter are the following:

* **Token**: a token is the minimal element of the language syntax, like an integer (not a digit, but a group of them), a name (not a letter but a group of them), or a symbol (like the mathematical operations).
* **Buffer**: the input text (the program) has to be managed by a specific component. Parsing the input text has many requirements, among them being able to read upcoming parts of the text and to move back, or to move to specific locations.
* **Lexer**: this is the first component of standard interpreters. Its job is to divide the stream of input characters into meaningful chunks called tokens. It will process a string like "123 + x" and output three tokens: an integer, a symbol and a variable name.
* **Parser**: the second component of standard interpreters. It analyses the stream of tokens produced by the lexer and produces a data structure that represents the whole program.
* **Visitor**: the output of the parser is processed by a component that will either write the equivalent in another language or execute it.
* **Command Line Interface (CLI)**: the whole stack can be directly used by a REPL (Read, Evaluate, Print Loop), a command line interface similar to the one Python provides. There each line is lexed, parsed, and visited, and the result is printed immediately.

I will provide two classes: `Token` and `TextBuffer`. These will avoid you spending too much time to create the basic tools, and allow you to get straight into the game. Since those classes come obviously with their own test suite you are free to develop them on your own. You should however start from the same tests that I used, otherwise your interface might end up being incompatible witht he rest of the project.

## Initial setup

I prepared [this repository](https://github.com/lgiordani/smallcalc), which contains everything you need to start the project.

``` sh
$ git clone https://github.com/lgiordani/smallcalc.git
```

Once you cloned the repository, set up a Python virtual environment using your favourite method/tool and install the testing requirements

``` sh
pip install -r requirements/test.txt
```

At this point you should be able to run the test suite. For this project we are going to use [pytest](http://www.pytest.org), so the command line is

``` sh
pytest -svv
```

or, if you want to check your code coverage,

``` sh
pytest -svv  --cov-report term-missing --cov=smallcalc 
```

## Tokens

The first class that I provide to start working on our interpreter is `Token`.

``` python
class Token:

    def __init__(self, _type, value=None, position=None):
        self.type = _type
        self.value = str(value) if value is not None else None
        self.position = position

    def __str__(self):
        if not self.position:
            return "Token({}, '{}')".format(
                self.type,
                self.value,
            )

        return "Token({}, '{}', line={}, col={})".format(
            self.type,
            self.value,
            self.position[0],
            self.position[1]
        )

    __repr__ = __str__

    def __eq__(self, other):
        return (self.type, self.value) == (other.type, other.value)

    def __len__(self):
        if self.value:
            return len(self.value)

        return 0

    def __bool__(self):
        return True
```

This represents one syntax unit in which we divide the input text. The token can contain information about its original position, which can be useful in case of syntax errors to print meaningful messages for the user. The class implements the method `__eq__` to provide comparison between tokens.

The value of a token is always a string, and shall be converted into a different type by an external object according to the value that the token assumes. For example the string `'123'` can be interpreted as an integer, but could also be the name of a variable if our language supports such a feature.

Remember that everything you find in this class has been introduced to make one or more tests pass, so check the test suite to understand how the object can be used.

## Buffer

The second element that you will find in the initial setup is the class `TextBuffer`, that provides a very basic manager for an input text file

``` python
class EOLError(ValueError):

    """ Signals that the buffer is reading after the end of a line."""


class EOFError(ValueError):

    """ Signals that the buffer is reading after the end of the text."""


class TextBuffer:

    def __init__(self, text=None):
        self.load(text)

    def reset(self):
        self.line = 0
        self.column = 0

    def load(self, text):
        self.text = text
        self.lines = text.split('\n') if text else []
        self.reset()

    @property
    def current_line(self):
        try:
            return self.lines[self.line]
        except IndexError:
            raise EOFError(
                "EOF reading line {}".format(self.line)
            )

    @property
    def current_char(self):
        try:
            return self.current_line[self.column]
        except IndexError:
            raise EOLError(
                "EOL reading column {} at line {}".format(
                    self.column, self.line
                )
            )

    @property
    def next_char(self):
        try:
            return self.current_line[self.column + 1]
        except IndexError:
            raise EOLError(
                "EOL reading column {} at line {}".format(
                    self.column, self.line
                )
            )

    @property
    def tail(self):
        return self.current_line[self.column:]

    @property
    def position(self):
        return (self.line, self.column)

    def newline(self):
        self.line += 1
        self.column = 0

    def skip(self, steps=1):
        self.column += steps

    def goto(self, line, column=0):
        self.line, self.column = line, column
```

As happened for the `Token` class, you can read the tests to understand how to use the class. Basically, however, the class can `load` an input text and extract the `current_line`, the `current_char`, and the `next_char`. You can also `skip` a given number of characters, `goto` a given position, extract the current `position` and read the `tail`, which is the remaining text from the current position to the end of the line.

This class has not been optimized or designed to manage big files or continuous streams of text. This is perfectly fine for our current project, but be aware that for a real compiler you might want to implement something more powerful.

## CLI

The third element I provide is a simple REPL ([Read–eval–print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)) that at the moment just echoes any text you will input and gracefully exit when we press Ctrl+D. There are and there will be no tests for the CLI. Testing endpoints like this is complex and not always worth the effort, as in this case.

The command line can be run from the project main directory with

``` sh
python cli.py
```

## Level 1 - End of file

*"End? No, the journey doesn't end here."* - The Lord of the Rings: The Return of the King (2003)

The first thing a Lexer shall be able to do is to load and process an empty text. This should return an `EOF` (`End Of File`) token. `EOF` is used to signal that the input buffer has ended and that there is no more text to process.

The method `get_tokens` returns all the tokens of the input stream in a single list.

Add this code to `tests/test_calc_lexer.py`

``` python
from smallcalc import tok as token
from smallcalc import calc_lexer as clex


def test_get_tokens_understands_eof():
    l = clex.CalcLexer()

    l.load('')

    assert l.get_tokens() == [
        token.Token(clex.EOF)
    ]
```

To avoid misleading errors you should also create the empty file `smallcalc/calc_lexer.py`, as without that file pytest will raise an `ImportError`.

This is our first test, and if you run the test suite now you will see that it fails. This is expected, as there is no code to pass the test.

``` sh
$ pytest -svv  --cov-report term-missing --cov=smallcalc
================================== FAILURES ===================================
_______________________ test_get_tokens_understands_eof _______________________

    def test_get_tokens_understands_eof():
>       l = clex.CalcLexer()
E       AttributeError: module 'smallcalc.calc_lexer' has no attribute 'CalcLexer'

tests/test_calc_lexer.py:6: AttributeError
===================== 1 failed, 29 passed in 0.08 seconds =====================
```

Implement now a class `CalcLexer` in the file `smallcalc/calc_lexer.py` that makes the test pass. Remember that you just need the code to pass this test. So do not implement complex systems now and go for the simplest solution (hint: the test expects that specific output).

The `EOF` constant can be a simple string with the value `'EOF'`.

It is worth executing the test suite with coverage (check the command line above), which will tell you if you over-engineered your code. You should aim for 100% coverage, always.

---

### Solution

To pass the test, the class `CalcLexer` can use the provided `text_buffer.TextBuffer` class, that exposes a method `load` and wrap it in `CalcLexer.load`. The test is not providing any input so the easiest solution is just to return the required token. The test requires us to implement the method `get_tokens`, but I preferred to isolate the code in a method called `get_token` and to call the latter from `get_tokens`. The file `smallcalc/calc_lexer.py` is then

``` python
from smallcalc import text_buffer
from smallcalc import tok as token

EOF = 'EOF'


class CalcLexer:
    def __init__(self, text=''):

        self._text_storage = text_buffer.TextBuffer(text)

    def load(self, text):
        self._text_storage.load(text)

    def get_token(self):
        self._current_token = token.Token(EOF)
        return self._current_token

    def get_tokens(self):
        return [self.get_token()]
```

---

You can see here in practice what I mentioned in the introduction about TDD. The method `get_token` returns a hardcoded `token.Token(EOF)`, because _that is enough to pass the test_. It is not enough to be a good Lexer, but if we write and pass the right tests, this will happen in time. Be smart, be strict: write the minimal code needed to pass the test.

Being really strict, however, this solution is already over-engineered. The code

``` python
    def get_tokens(self):
        return [token.Token(EOF)]
```

would be enough to pass the test. It would also be the first thing we change as soon as we add another test. So, let me amend the previous advice: be strict, with a pinch of salt.

## Level 2 - Single digit integers

*"You're missing just a couple of digits there."* - Iron Man (2008)

The requirement for this section is

``` text
# The only accepted value for the input is one single digit between 0 and 9
integer: [0-9]
```

### Lexer

Since a calculator has to deal with numbers let us implement support for integers (we will add floating point numbers later). The first thing that we need is to recognise single-digit integers. This is the test that you have to add to `tests/test_calc_lexer.py`

``` python
def test_get_token_understands_integers():
    l = clex.CalcLexer()

    l.load('3')

    assert l.get_token() == token.Token(clex.INTEGER, '3')
```

Note that here we are testing `get_token` and not `get_tokens`. This method will come handy later, so it is worth testing it here. As soon as that works you can test the behaviour of `get_tokens`

``` python
def test_get_tokens_understands_integers():
    l = clex.CalcLexer()

    l.load('3')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

Please note that now the lexer shall output both an `EOF` and an `EOL` token, as the current line of code ends. The biggest issue you have to face here is that when you recognise a token then you have to skip it in the source text.

After this test you may end up with some code duplication, as `get_token` and `get_tokens` perform similar tasks. If you haven't already, please call the former from the latter. It could also be worth doing some refactoring. Remember: you can confidently change your code, because as long as the tests pass your changes are correct! This is the true power of TDD.

If you refactor the code creating helper methods you should make them "private" by prefixing their name with an underscore. This also means that you do not need to test them, in principle (watch [this talk](https://www.youtube.com/watch?v=URSWYvyc42M) by Sandy Metz on this subject).

### Parser

Now that we have a working lexer that recognises integers let us work on the parser. This has to use the lexer to process a text and produce a tree of nodes that represent the syntactic structure of the processed code. Don't worry if it seems extremely complex, it is actually pretty simple if you follow the tests.

Edit the `tests/test_calc_parser.py` file and insert this code

``` python
from smallcalc import calc_parser as cpar


def test_parse_integer():
    p = cpar.CalcParser()
    p.lexer.load("5")

    node = p.parse_integer()

    assert node.asdict() == {
        'type': 'integer',
        'value': 5
    }
```

The `node` variable is an instance of a specific class that contains integers, `IntegerNode` (but you are free to name it as you want, as this is not tested). Please note that this class doesn't consider the value as a string any more, but as a proper (Python) integer (`'value': 5`). Now edit the file `smallcalc/calc_parser.py` and write some code that passes the test.

Does it work? Well, you just wrote your first parser! Congratulations! From here to something that understands C++ or Python the journey is pretty long, but the initial steps are promising.

### Visitor

Let us consider the visitor, now. This is the run-time component of our language, the part that actually runs through the tree of nodes and executes it. This part, thus, is where most of the actual behaviour of the language happens. For instance, the fact that the symbol "+" actually sums integers is because the visitor implements that operation.

This can seem a trivial consideration, but if you think about the division between integers you immediately understand that the visitor has a great responsibility. Does the symbol `/` divide integers with or without floating point math? Python 3, for instance, opted for a floating point division, and introduced the `//` operator for the integer version of the operation, but other languages behave differently.

I'll discuss this in more detail later, when we will implement mathematical operations. For the time being, let us create the `tests/test_calc_visitor.py` file and introduce the following test

``` python
from smallcalc import calc_visitor as cvis


def test_visitor_integer():
    ast = {
        'type': 'integer',
        'value': 12
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (12, 'integer')
```

As you can see, at this stage the visitor has a trivial job, which is to just return the value and the type of the number that it finds in the tree. Note that the visitor provides a method `visit` which is type agnostic (i.e. it doesn't care about the type of the node). This is correct, as the visitor has to traverse the whole tree recursively and to react to the different nodes without a previous knowledge of what it should expect.

As simple as the visitor can be, now we can make our CLI interface use the parser and the visitor to understand and execute one simple command, which is to parse a single-digit integer and print it with its type. Change the `cli.py` file to 

``` python
from smallcalc import calc_parser as cpar
from smallcalc import calc_visitor as cvis


def main():
    p = cpar.CalcParser()
    v = cvis.CalcVisitor()

    while True:
        try:
            text = input('smallcalc :> ')
            p.lexer.load(text)

            node = p.parse_integer()
            res = v.visit(node.asdict())

            print(res)

        except EOFError:
            print("Bye!")
            break

        if not text:
            continue


if __name__ == '__main__':
    main()
```

Test it to check that everything works. If your code passes the tests I gave you, the result is guaranteed.

``` sh
$ python cli.py 
smallcalc :> 3
(3, 'integer')
```

Let me recap what we just created. We wrote a lexer, which is a component that splits the input text in different tokens with a meaning, and we instructed it to react to single-digits integers. Then, we created a parser, which is the component that tries to make sense of several tokens put together, applying syntactical rules. Last, the visitor runs through the output of the parser and actually performs the actions that the grammar describes. All this to just print out an integer? Seems overkill! It is, actually, but there is a lot to come, and this separation of levels will come handy.

---

### Solution

The two functions `get_token` and `get_tokens` have to evolve to deal with the new requirements, and to avoid having too much code in a single function I created some private helpers (where "private" has the Python meaning of "please don't use them").

The idea behind `get_tokens` is to call `get_token` until the `EOF` token is returned, even though we want the latter to be present in the final result.

``` python
    def get_tokens(self):
        t = self.get_token()
        tokens = []

        while t != token.Token('EOF'):
            tokens.append(t)
            t = self.get_token()

        tokens.append(token.Token('EOF'))

        return tokens
```

Then I decided to make `get_token` the central hub of my process with the following paradigm: the function tries to extract a specific token (`_process_integer`, in this case) and to return it; if the token cannot be extracted, the function tries the following one. At the moment I don't have any other type of token, but I will have them soon.

``` python
    def get_token(self):
        eof = self._process_eof()
        if eof:
            return eof

        eol = self._process_eol()
        if eol:
            return eol

        integer = self._process_integer()
        if integer:
            return integer
```

The three helpers shall just try to extract and return the token they have been assigned or None. After some refactoring I came up with three functions (two of them as properties) that simplify common tasks. `_current_char` and `_current_line` are just wrappers around two attributes of `self._text_storage`, while `_set_current_token_and_skip` is a bit more complex and ensures that the `_current_token` is always up to date.

``` python
    @property
    def _current_char(self):
        return self._text_storage.current_char

    @property
    def _current_line(self):
        return self._text_storage.current_line

    def _set_current_token_and_skip(self, token):
        self._text_storage.skip(len(token))

        self._current_token = token
        return token
```

Once this functions are in place I can write the actual helpers for the token extraction. The method `_process_eol` leverages `self._text_storage`, which raises an `EOLError` when the end of line has been reached. So all I need to do is to try to get the current char and return `None` if nothing happens. In case an `EOLError` exception is raised I run `_set_current_token_and_skip` with the end of line token.

``` python
    def _process_eol(self):
        try:
            self._current_char
            return None
        except text_buffer.EOLError:
            self._text_storage.newline()

            return self._set_current_token_and_skip(
                token.Token(EOL)
            )
```

The helper to process the end of file (`_process_eof`) is exactly like `_process_eol`, using `self._current_line` and `text_buffer.EOFError`.

``` python
    def _process_eof(self):
        try:
            self._current_line
            return None
        except text_buffer.EOFError:
            return self._set_current_token_and_skip(
                token.Token(EOF)
            )
```

At this point of the development the incoming token can only be `EOL`, `EOF`, or an integer, so the `_process_integer` function doesn't need to return `None`. Therefore, it is sufficient to create an integer token with the current char and return it.

``` python    
    def _process_integer(self):
        return self._set_current_token_and_skip(
            token.Token(INTEGER, self._current_char)
        )
```

The above methods use two new global variables `EOL` and `INTEGER` that are defined at the beginning of the file along with `EOF`

``` python
EOL = 'EOL'
INTEGER = 'INTEGER'
```

`CalcParser` is the only class that is tested, but forecasting (actually, knowing) that we are going to manage multiple types of nodes, I isolated the code for the `IntegerNode` in its own class. There is no need to abstract things further for the time being, so `IntegerNode` doesn't inherit from any other class.

From a pure TDD point of view this is wrong, because I should have written some tests for the `IntegerNode` class before writing it. The purpose of this exercise, however is to guide you through the creation of a simple compiler, so tests are already given, and I will turn a blind eye on my own exception to the rule (how convenient!).

``` python
from smallcalc import calc_lexer as clex


class IntegerNode:
    node_type = 'integer'

    def __init__(self, value):
        self.value = int(value)

    def asdict(self):
        return {
            'type': self.node_type,
            'value': self.value
        }


class CalcParser:

    def __init__(self):
        self.lexer = clex.CalcLexer()

    def parse_integer(self):
        t = self.lexer.get_token()
        return IntegerNode(t.value)
```

`CalcVisitor` is by far the simplest class at the moment, as the only node we are managing is the one with an `integer` type.

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']
```

---

## Level 3 - Binary operations: addition

*"You're about to become a permanent addition to this archaeological find."* - Raiders of the Lost Ark (1981)

Let's update the grammar of the language with `addsymbol` and `expression`

``` text
integer: [0-9]

# Label the symbol '+'' with the name 'addsymbol'
addsymbol: '+'

# An expression is an integer followed by an addsymbol followed by another integer
expression: integer addsymbol integer
```

At the moment, our parser doesn't sound like an important component, as its output is just a refurbished version of the lexer one. The visitor, in turn, doesn't really perform any action but to print in a different format what the parser produces.

Let us try to introduce a simple mathematical operation, then, that should spice up our components. The new test for the lexer component (`tests/test_calc_lexer.py`) is

``` python
def test_get_tokens_understands_unspaced_sum_of_integers():
    l = clex.CalcLexer()

    l.load('3+5')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '+'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

Please note that there are no spaces, as our lexer doesn't know how to deal with them yet. As you can see the output is straightforward, so go and change the `CalcLexer` class to make this tests pass without breaking any of the ones you already wrote. Check for coverage, to spot possible overengineered parts, and if necessary refactor the class to keep methods as simple as possible.

The parser now has a more complex job than before, though not yet really challenging. The test for the parser is

``` python
def test_parse_expression():
    p = cpar.CalcParser()
    p.lexer.load("2+3")

    node = p.parse_expression()

    assert node.asdict() == {
        'type': 'binary',
        'left': {
            'type': 'integer',
            'value': 2
        },
        'right': {
            'type': 'integer',
            'value': 3
        },
        'operator': {
            'type': 'literal',
            'value': '+'
        }
    }
```

I want to resume here the discussion about mathematical operators and the role of the visitor that I started in the previous section. As you can see the expression is a generic binary operator, with a `left` term, a `right` term, and an `operator`. The operator, furthermore, is just a literal which value is the symbol we use for that binary operation.

This parser, thus, is pretty ignorant of the different operations we can perform, giving the whole responsibility to the visitor. We could, however, implement the parser to make it produce something more specific, like for example a `binary_sum` or `addition` node, which represents only the addition, and which wouldn't need the `'operator'` key, as it is implicit in the node type.

The amount of work done by the parser and by the visitor is a peculiarity of the specific language or program, so feel free to experiment. For the moment you have to stick to one solution as you are guided by the tests that I wrote, but as soon as you grasped the concepts and start writing a new language, you will be free to implement each component as you prefer.

Finally, the visitor shall implement the actual mathematical operation. The test is

``` python
def test_visitor_expression_sum():
    ast = {
        'type': 'binary',
        'left': {
                'type': 'integer',
                'value': 5
        },
        'right': {
            'type': 'integer',
            'value': 4
        },
        'operator': {
            'type': 'literal',
            'value': '+'
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (9, 'integer')
```

As soon as you changed the method `visit` to deal with `'expression'` nodes, you can test the new syntax in the CLI. Since we changed `visit` internally, that part of the CLI doesn't require any modification. We have, however, to change the parser entry point from `parse_integer` to `parse_expression`, so the new `cli.py` file will be

``` python
from smallcalc import calc_parser as cpar
from smallcalc import calc_visitor as cvis


def main():
    p = cpar.CalcParser()
    v = cvis.CalcVisitor()

    while True:
        try:
            text = input('smallcalc :> ')
            p.lexer.load(text)

            node = p.parse_expression()
            res = v.visit(node.asdict())

            print(res)

        except EOFError:
            print("Bye!")
            break

        if not text:
            continue


if __name__ == '__main__':
    main()
```

And a quick test of the CLI confirms that everything works fine

``` sh
$ python cli.py 
smallcalc :> 2+4
(6, 'integer')
```

Everything? Well, not exactly. If I type just a single integer in the CLI the whole program crashes with an exception. If your solution doesn't blow up with a single integer, it means that you (probably) overengineered it a little. This is fine, but if you had implemented just what was needed to pass the tests the result would have been an error in that case.

Why do we have an error? Because we now parse the input with `parse_expression` and this method expects its input to be a full-formed expression, not a single integer. Generally speaking, our parser's entry point should be able to parse different syntax structures. We will improve this behaviour later, when we will address the problem of nested expressions.

---

### Solution

The helper `_process_literal` does what `_process_integer` did before, which is to blindly return a token, this time with the `LITERAL` type.

``` python
LITERAL = 'LITERAL'
```

``` python
    def _process_literal(self):
        return self._set_current_token_and_skip(
            token.Token(LITERAL, self._current_char)
        )
```

The helper `_process_integer`, on the other hand, changes to return `None` when no integer can be parsed, which is easily checked with `isdigit`.

``` python
    def _process_integer(self):
        if not self._current_char.isdigit():
            return None

        return self._set_current_token_and_skip(
            token.Token(INTEGER, self._current_char)
        )
```

Last, the method `get_token` receives `_process_literal` as an additional case.

``` python
    def get_token(self):
        eof = self._process_eof()
        if eof:
            return eof

        eol = self._process_eol()
        if eol:
            return eol

        integer = self._process_integer()
        if integer:
            return integer

        literal = self._process_literal()
        if literal:
            return literal
```

The parser needs a node that represents the literal, namely `LiteralNode`, and a node to represent a binary operation, called `BinaryNode`. To avoid duplicating methods I created the `ValueNode` class and made both `IntegerNode` and `LiteralNode` inherit from that.

``` python
from smallcalc import calc_lexer as clex


class Node:

    def asdict(self):
        return {}  # pragma: no cover


class ValueNode(Node):

    node_type = 'value_node'

    def __init__(self, value):
        self.value = value

    def asdict(self):
        return {
            'type': self.node_type,
            'value': self.value
        }


class IntegerNode(ValueNode):
    node_type = 'integer'

    def __init__(self, value):
        self.value = int(value)


class LiteralNode(ValueNode):

    node_type = 'literal'


class BinaryNode(Node):

    node_type = 'binary'

    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def asdict(self):
        result = {
            'type': self.node_type,
            'left': self.left.asdict()
        }

        result['right'] = None
        if self.right:
            result['right'] = self.right.asdict()

        result['operator'] = None
        if self.operator:
            result['operator'] = self.operator.asdict()

        return result
```

The most important change, however, is in `CalcParser`, where I added the methods `parse_addsymbol` and `parse_expression`.

``` python
class CalcParser:

    def __init__(self):
        self.lexer = clex.CalcLexer()

    def parse_addsymbol(self):
        t = self.lexer.get_token()
        return LiteralNode(t.value)

    def parse_integer(self):
        t = self.lexer.get_token()
        return IntegerNode(t.value)

    def parse_expression(self):
        left = self.parse_integer()
        operator = self.parse_addsymbol()
        right = self.parse_integer()

        return BinaryNode(left, operator, right)
```

The visitor has to add the processing code for `binary` nodes, which assumes the operation is a sum, so it just needs to visit the left and right nodes.

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'binary':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            return lvalue + rvalue, rtype
```

---

## Level 4 - Multi-digit integers

*"So many."* - Braveheart (1995)

Let's move allowing integers to be made of multiple digits.

``` text
# An integer is a sequence of digits, + here means `one or more`
integer: [0-9]+
addsymbol: '+'
expression: integer addsymbol integer
```

Up to now our language can handle only single-digit integers, so this part shall be enhanced before moving to more complex syntax structures. The only component that requires a change is the lexer, as it should emit one single token containing all the digits. The test, consequently, goes in `tests/test_calc_lexer.py`

``` python
def test_get_tokens_understands_multidigit_integers():
    l = clex.CalcLexer()

    l.load('356')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '356'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

There are many ways to solve this problem, one of the simplest (and a perfectly valid one) is using regular expressions (which are, if you think about it, another language).

If you do not know how to use regular expressions do yourself a favour and learn them! You can find a nice tutorial on them at [RegexOne](https://regexone.com/). If you already know the syntax but don't know how to use them in Python [this Google for Education page](https://developers.google.com/edu/python/regular-expressions) and the [official documentation](https://docs.python.org/3/howto/regex.html) are your friends.

After this test the CLI should be able to handle expressions like `123+456`. We don't need any change in the parser and in the visitor, can you tell why?

---

### Solution

To provide support for multi-digit integers we just need to change the method `_process_integer` of the lexer. The new version makes use of a very simple regular expressions.

``` python
import re
```

``` python
    def _process_integer(self):
        regexp = re.compile('\d+')

        match = regexp.match(
            self._text_storage.tail
        )

        if not match:
            return None

        token_string = match.group()

        return self._set_current_token_and_skip(
            token.Token(INTEGER, int(token_string))
        )
```

The reason why we don't need to change the parser and the visitor is that nothing changed at that level. We altered the way the lexer identifies an integer token, but once that has been isolated the following steps are exactly the same as before.

---

## Level 5 - Whitespaces

*"Follow the white rabbit."* - The Matrix (1999)

The second limitation that our language has at the moment is that it cannot handle whitespaces. If you try to input an expression like `3 + 4` in the CLI the program will crash with an exception (why?). Traditionally, whitespaces are completely ignored by programming languages: in Python, as well as in C and many other languages, writing `3+4`, `3 + 4`, `3+ 4`, or `3   +   4` doesn't change the meaning at all. In Python, however, whitespaces matter at the beginning of the line, as indentation is used in lieu of parentheses.

How can we put such a behaviour in our language? Again, the lexer is the component in charge, as it should just skip whitespaces. So add this test to `tests/test_calc_lexer.py`

``` python
def test_get_tokens_ignores_spaces():
    l = clex.CalcLexer()

    l.load('3 + 5')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '+'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

While you change the `CalcLexer` class to make it pass this test, ask yourself if the current structure of the class satisfies you or if it is the right time to refactor it, possibly even heavily rewriting some parts of it. You have a good test suite, now, so you can be sure that what you implemented is correct, at least according to the current requirements.

Note that we are hitting a limitation of unit testing here, which is that we should test that the language skips _any_ amount of whitespaces, but it is impossible to write a test for this. We can test 1, 2, 100 whitespaces, but never _any_ amount. The pragmatic solution, here is that of testing that the language skips one whitespace and leave further tests to be written only if specific errors arise in the future. The code should however try to provide a generic solution.

---

### Solution

To process whitespaces I needed to add a helper called `_process_whitespace` with the same structure of the new `_process_integer`.

``` python
    def _process_whitespace(self):
        regexp = re.compile('\ +')

        match = regexp.match(
            self._text_storage.tail
        )

        if not match:
            return None

        self._text_storage.skip(len(match.group()))
```

Note that the solution here is `'\ +'` which skips any amount of whitespaces, even though `'\ '` would have been enough to pass the test. As I said before, every time we have to test cases with "any" in them we have to be a bit less strict. TDD is not perfect, and remember that at the end of the day it's more important to have something that works and is not perfect than something that is perfect and doesn't work at all.

As this time I am not interested in returning whitespace tokens, I just want to skip them. The helper is therefore added to `get_token` without a `return` statement.

``` python
    def get_token(self):
        eof = self._process_eof()
        if eof:
            return eof

        eol = self._process_eol()
        if eol:
            return eol

        self._process_whitespace()

        integer = self._process_integer()
        if integer:
            return integer

        literal = self._process_literal()
        if literal:
            return literal
```

---

## Level 6 - Subtraction

*"I can add, subtract. I can make coffee. I can shuffle cards."* - The Bourne Identity (2002)

``` text
integer: [0-9]+

# An addsymbol can be the symbol '+' or the symbol '-'
addsymbol: '+' | '-'

expression: integer addsymbol integer
```

Now that we addressed two basic issues of our language we can start enhancing the higher level syntactical structures. Since we implemented the addition operation, the most natural step forward is to implement subtraction. As for the addition, this change will involve all the three layers of the language, lexer, parser, and visitor.

Let us start teaching the lexer to understand the minus sign. The test that we need is the following (in `tests/test_calc_lexer.py`)

``` python
def test_get_tokens_understands_subtraction():
    l = clex.CalcLexer()

    l.load('3 - 5')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '-'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

Does the lexer require any change? Why?

As you can see the decision to handle operators and symbols as `LITERAL` tokens allows us to introduce new symbols without the need to change the lexer. This obviously means that we will need to tell the symbols apart in a later stage, as nothing happens automatically. We could have decided to represent each symbol with a specific token, like `PLUS` and `MINUS`, but if you think about it, this would not have really changed the code in later stages, as `PLUS` is just another symbol, exactly like `+` is.

Using specific tokens, however, can simplify things if we want to handle multi-character literals. If we have an operator like `->` (as in C) or `//` (like in Python), or something more complex, we could prefer to handle those in the lexer, emitting a single token with a specific name.

We could introduce in the lexer a table of accepted values for literals, which would lead to an earlier and better error reporting. At the moment our language accepts every literal between two integers (try with `$`, for example), but fails to process them in the parser, interpreting any literal as `+`. Feel free to expand the project in such a direction if you want.

The test for the parser is the following

``` python
def test_parse_expression_understands_subtraction():
    p = cpar.CalcParser()
    p.lexer.load("2-3")

    node = p.parse_expression()

    assert node.asdict() == {
        'type': 'binary',
        'left': {
            'type': 'integer',
            'value': 2
        },
        'right': {
            'type': 'integer',
            'value': 3
        },
        'operator': {
            'type': 'literal',
            'value': '-'
        }
    }
```

which is a small variation of the previously implemented `test_parse_expression`.

The considerations made for the lexer are perfectly valid for the parser as well, so you should need no node change at this point. The last test we have to add is that of the visitor, which is again very similar to the previous one

``` python
def test_visitor_expression_subtraction():
    ast = {
        'type': 'binary',
        'left': {
                'type': 'integer',
                'value': 5
        },
        'right': {
            'type': 'integer',
            'value': 4
        },
        'operator': {
            'type': 'literal',
            'value': '-'
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (1, 'integer')
```

This time, however, we are at the end of the processing chain, and we have to deal with the difference symbol, namely to actually subtract numbers. To make this test pass, thus, you will need to change something in your `CalcVisitor` class.

Once your code passes this test, a quick test of the CLI shows that everything works as intended

``` sh
$ python cli.py 
smallcalc :> 4 - 6
(-2, 'integer')
```

and since we rely on Python to perform the actual subtraction we get negative numbers for free. Pay attention: we can have negative numbers in the results, but we cannot input negative numbers. This is something that we will have to add later.

---

### Solution

Adding the addition binary operation changed code in the lexer, the parser, and in the visitor. That operation was however considered a generic binary operation, and only the visitor implements the actual `+` operation. So adding the subtraction works out of the box for the first two stages and requires me to change the visitor only, with a simple `if` condition on the value of the operator.

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'binary':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            operator = node['operator']['value']

            if operator == '+':
                return lvalue + rvalue, rtype
            else:
                return lvalue - rvalue, rtype
```

---

## Level 7 - Multiple operations

*"The machine simply does not operate as expected."* - The Prestige (2006)

``` text
integer: [0-9]+
addsymbol: '+' | '-'

# A expression starts with a single integer and optionally
# contains an addsymbol and another expression
# (this is a recursive definition) 
expression: integer (addsymbol expression)
```

Before we dive into the fascinating but complex topic of nested operations, let us take a look and implement multiple operations, that is the application of a chain of "similar" operators with the same priority.

Since this tutorial is a practical approach to the construction of an interpreter, I will not go too deep into the subject matter. Feel free to check the references if you are interested in such topics. For the moment, it is sufficient to understand that addition and subtraction are two operations that have the same precedence, which means that their order can be changed without affecting the result.

For instance: the expression `3 + 4 - 5` gives `2` as a result. The result is the same if we perform `(3 + 4) - 5 = 7 - 5 = 2` or `3 + (4 - 5) = 3 - 1 = 2`, where the expressions between parentheses are executed first.

From the interpreter's point of view, then, we can process a chain of additions and subtractions without being concerned about precedence, which greatly simplifies our job. As the output of the parser is a tree, however, we need to find a way to represent such a chain of operations in that form. One way is to nest expressions, which means that each operation is a single `binary` node, with the left term containing an integer and the right one the rest of the expression. In the previous example `3 + 4 - 5` is represented by an addition between `3` and `4 - 5`. `4 - 5`, in turn, is another binary node, a subtraction between `4` and `5`.

Let us start checking if the lexer understand multiple operations

``` python
def test_get_tokens_understands_multiple_operations():
    l = clex.CalcLexer()

    l.load('3 + 5 - 7')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '+'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.LITERAL, '-'),
        token.Token(clex.INTEGER, '7'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

If the current version of your lexer doesn't pass this test make the necessary changes to the code.

Now we should modify the parser. While expressing the test is very simple, actually creating the code that makes it pass is not that trivial. So, as an intermediate step, I will make you implement the code that allows the parser to check upcoming tokens.

One solution to this problem is to save the state of the parser, get as many tokens as we need, and then restore the status. Inspired by Git, I called those methods `stash` and `pop`. Put the following test in `tests/test_calc_lexer.py`

``` python
def test_lexer_can_stash_and_pop_status():
    l = clex.CalcLexer()
    l.load('3 5')

    l.stash()
    l.get_token()
    l.pop()

    assert l.get_token() == token.Token(clex.INTEGER, '3')
```

As you can see the `get_token` call between `stash` and `pop` doesn't leave any trace.

Once your code is working implement the second test. Create a method `peek_token` that performs all the previous actions together

``` python
def test_lexer_can_peek_token():
    l = clex.CalcLexer()
    l.load('3 + 5')

    l.get_token()
    assert l.peek_token() == token.Token(clex.LITERAL, '+')
```

You can implement `peek_token` very easily leveraging `stash` and `pop`.

Now we are ready to face the test that covers nested operations, which goes in `tests/test_calc_parser.py`

``` python
def test_parse_expression_with_multiple_operations():
    p = cpar.CalcParser()
    p.lexer.load("2 + 3 - 4")

    node = p.parse_expression()

    assert node.asdict() == {
        'type': 'binary',
        'left': {
            'type': 'binary',
            'left': {
                'type': 'integer',
                'value': 2
            },
            'right': {
                'type': 'integer',
                'value': 3
            },
            'operator': {
                'type': 'literal',
                'value': '+'
            }
        },
        'right': {
            'type': 'integer',
            'value': 4
        },
        'operator': {
            'type': 'literal',
            'value': '-'
        }
    }
```

A note of warning: probably the first version of the code that makes this test pass will be horrible, as the logic involved is not trivial. Remember that your **first goal is to make the test pass and then, with the battery of tests in your arsenal, to tidy up the code**.

As usual, the last test involves the visitor

``` python
def test_visitor_expression_with_multiple_operations():
    ast = {
        'type': 'binary',
        'left': {
            'type': 'binary',
            'left': {
                'type': 'integer',
                'value': 3
            },
            'right': {
                'type': 'integer',
                'value': 4
            },
            'operator': {
                'type': 'literal',
                'value': '-'
            }
        },
        'right': {
            'type': 'integer',
            'value': 200
        },
        'operator': {
            'type': 'literal',
            'value': '+'
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (199, 'integer')
```

What changes do you need to make to the `CalcVisitor` class? Why?

---

### Solution

I made no assumptions on the length of the tokens stream in `get_tokens`, so processing multiple tokens comes out of the box in the lexer.

Adding `stash` and `pop` is not very complex, as the tests show exactly what we need to save and retrieve. Here I leverage the `position` attribute and the `goto` functions of the `TextBuffer` class.

``` python
class CalcLexer:

    def __init__(self, text=''):
        self._text_storage = text_buffer.TextBuffer(text)
        self._status = []
        self._current_token = None

    [...]

    @property
    def _current_status(self):
        status = {}
        status['text_storage'] = self._text_storage.position
        status['current_token'] = self._current_token
        return status

    def stash(self):
        self._status.append(self._current_status)

    def pop(self):
        status = self._status.pop()
        self._text_storage.goto(*status['text_storage'])
        self._current_token = status['current_token']
```

Once `stash` and `pop` are in place implementing `peek_token` is trivial

``` python
    def peek_token(self):
        self.stash()
        token = self.get_token()
        self.pop()

        return token
```

Finally, `peek_token` allows me to add support for multiple expressions in the parser.

``` python
    def parse_expression(self):
        left = self.parse_integer()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL:
            operator = self.parse_addsymbol()
            right = self.parse_integer()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

---

## Final words

Phew! That was something, wasn't it? I think so, we went from nothing to a trivial calculator, but the engine we have under the bonnet is clearly powerful, so I'm already looking forward to implementing more complex syntax elements, like round brackets, multiplication, division, not to mention that sooner or later this should become a language, so we will need variables, functions, scopes, and so on.

The code I developed in this post is available on the GitHub repository tagged with `part1` ([link](https://github.com/lgiordani/smallcalc/tree/part1)).

Well, See you in the next post of the series, then!

## Resources

Some links on compilers history

* [GCC history](http://gcc.gnu.org/wiki/History)
* [History of compiler construction on Wikipedia](https://en.wikipedia.org/wiki/History_of_compiler_construction)

Tutorials and analysis of compilers and parsers

* The beautiful ["Let’s Build A Simple Interpreter"](https://ruslanspivak.com/lsbasi-part1/) series by Ruslan Spivak. Thanks Ruslan!
* How to implement a programming language in JavaScript [on Lisperator.net](http://lisperator.net/pltut/) by Mihai Bazon.
* [Build Your Own Lisp](http://www.buildyourownlisp.com/)
* [LL and LR Parsing Demystified](http://blog.reverberate.org/2013/07/ll-and-lr-parsing-demystified.html) by Josh Haberman.

Grammars

* [Backus-Naur form on Wikipedia](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
* [Extended Backus-Naur form on Wikipedia](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
* [The language of languages](http://matt.might.net/articles/grammars-bnf-ebnf/)

## Updates

2017-12-24: Victor Uriarte ([vmuriart](https://github.com/vmuriart)) spotted an important issue in a previous version of the post. The last two tests (`test_parse_expression_with_multiple_operations` and `test_visitor_expression_with_multiple_operations`) used a right-growing tree instead of a left-growing one. The problem with a right-growing tree is that an operator affects _everything_ is on the right side, that is the whole rest of the operation. Thus, an operation like `10 - 1 + 1` would become `10 - (1 + 1)`, and the result is obviously different. I fixed the tests and the solution I give in the next posts. You can read Victor's issue [here](https://github.com/lgiordani/smallcalc/issues/4). Thanks Victor for spotting it!

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/blog_source/issues) page is the best place to submit corrections.

