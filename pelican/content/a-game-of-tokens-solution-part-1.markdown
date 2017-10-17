Title: A game of tokens: solution - Part 1
Date: 2017-07-12 10:00:00 +0100
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-solution-part-1
Series: "A game of tokens"
Summary:

This is my solution of the challenge posted [here](/blog/2017/05/09/a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1/). As I stressed in that post, this is just **one** possible solution, and not even necessarily the best one. I provide it to show how I managed to solve the tests and how I worked in a TDD way.

Speaking of TDD I realised that I hadn't followed it very strictly, as sometimes I wrote more code than needed, usually forecasting future changes. I do not believe in a inflexible and uncompromising application of rules, so I do not consider this a big issue, as long as the result is a working code that is not blatantly overengineered.

You can find the code for this part in [this repository](https://github.com/lgiordani/smallcalc). The branch called `part1` contains all the commits explained in this post, and every commit contains both the test(s) and the code that makes the test(s) pass.

# Level 1 - End of file

The base class to pass the test leverages the provided `text_buffer.TextBuffer` class, that exposes a `load()` method, directly composed here to `CalcLexer.load()`. As the test is not providing a text the easiest solution is just to return the tested token. I extracted `get_token()` from `get_tokens()` to have a method that is specifically focused on dealing with the current token.

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

# Level 2 - Single digit integers

## Lexer

The two functions `get_token()` and `get_tokens()` have to evolve to deal with the new requirements, and to avoid having too much code in a single function I created some private helpers (where "private" has the Python meaning of "please don't use them").

The idea behind `get_tokens()` is to call `get_token()` until the `EOF` token is returned, even though we want the latter to be present in the final result.

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

Then I decided to make `get_token()` the central hub of my process with the following paradigm: the function tries to extract a specific token and to return it; if the token cannot be extracted, the function tries with the following.

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

The three helpers shall just try to extract and return the token they have been assigned or None. After some refactoring I came up with three functions (two of them as properties) that simplify common tasks. `_current_char` and `_current_line` are just wrappers around two attributes of `self._text_storage`, while `_set_current_token_and_skip()` is a bit more complex and ensures that the `_current_token` is always up to date.

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

Once this functions are in place I can write the actual helpers for the token extraction. `_process_eol()` leverages `self._text_storage`, which raises an `EOLError` when the end of line has been reached. So all I need to do is to try to get the current char and return None if nothing happens. In case of `EOLError` exception I run `_set_current_token_and_skip()` with the end of line token.

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

The helper to process the end of file, `_process_eof()` is exactly like `_process_eol()`, using `self._current_line` and `text_buffer.EOFError`.

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

At this point of the development the incoming token can only be `EOL`, `EOF`, or an integer. So the `_process_integer()` function doesn't need to return `None`. So I just create an integer token with the current char and return it.

    def _process_integer(self):
        return self._set_current_token_and_skip(
            token.Token(INTEGER, self._current_char)
        )

## Parser

`CalcParser` is the only class that is tested, but forecasting (actually, knowing) that we are going to manage multiple types of nodes, I isolated the code for the `IntegerNode` in its own class. There is no need yet to abstract things further, however, so `IntegerNode` doesn't inherit from any other class.

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

## Visitor

`CalcVisitor` is by far the simplest class at the moment, as the only node we are managing is the one with an `integer` type.

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']
```

# Level 3 - Binary operations: addition

## Lexer

The `_process_literal()` helper does what `_process_integer()` did before, which is to blindly return a token, this time with the `LITERAL` type.

``` python
    def _process_literal(self):
        return self._set_current_token_and_skip(
            token.Token(LITERAL, self._current_char)
        )
```

The `_process_integer()` helper, on the other hand, changes to return `None` when no integer can be parsed, which is easily checked with `isdigit()`.

``` python
    def _process_integer(self):
        if not self._current_char.isdigit():
            return None

        return self._set_current_token_and_skip(
            token.Token(INTEGER, self._current_char)
        )
```

Last, the `get_token()` method, which adds `_process_literal` as an additional case.

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

## Parser

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

The most important change, however, is in `CalcParser`, where I added the `parse_addsymbol()` and `parse_expression()` methods.

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

## Visitor

The visitor has to add the processing code for `binary` nodes, which assumes the operation can only be a sum, so just needs to visit the left and right nodes.

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'binary':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            return lvalue + rvalue, ltype
```

# Level 4 - Multi-digit integers

To provide support for multi-digit integers we just need to change the `_process_integer()` method of the lexer. The new version makes use of a very simple regular expressions.

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

# Level 5 - Whitespaces

To process whitespaces I needed to add a helper called `_process_whitespace()` with the same structure of the new `_process_integer()`.

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

As this time I am not interested in returning whitespace tokens, but I just want to skip them, the helper is added to `get_token()` without a `return` statement.

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

# Level 6 - Subtraction

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
                return lvalue + rvalue, ltype
            else:
                return lvalue - rvalue, ltype
```

# Level 7 - Multiple operations

I made no assumptions on the length of the tokens stream in `get_tokens()`, so processing multiple tokens comes out of the box inn the lexer.

Adding `stash()` and `pop()` is not very complex, as the tests show exactly what we need to save and retrieve. Here I leverage the `position` attribute and the `goto` functions of the `TextBuffer` class.

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

Once `stash()` and `pop()` are in place implementing `peek_token()` is trivial

``` python
    def peek_token(self):
        self.stash()
        token = self.get_token()
        self.pop()

        return token
```

Finally, `peek_token()` allows me to add support for multiple expressions in the parser.

``` python
    def parse_expression(self):
        left = self.parse_integer()

        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL:
            operator = self.parse_addsymbol()
            right = self.parse_expression()

            return BinaryNode(left, operator, right)
        else:
            return IntegerNode(left.value)
```

## Final words

Again, it is worth mentioning that this solution of mine is just one of the possible ones, not the **correct** one. If your code passes the tests it is correct; it can be ugly, overengineered, slow, but definitely correct. I hope that reading my solution helped you better understand the underlying concepts of lexer, parser, and visitor. Feel free to get in touch if you want to discuss your solution or if you have questions about the code I posted here.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
