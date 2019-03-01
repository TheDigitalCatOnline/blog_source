Title: A game of tokens: solution - Part 3
Date: 2017-10-31 12:00:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-solution-part-3
Series: A game of tokens
Image: a-game-of-tokens-solutions
Summary: 

This is my solution to the third part of "A Game of Tokens", where we try to implement a programming language with Python. The description of this third part of the challenge can be found [here]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-3.markdown).

You can find the tests and the code presented here in [this repository](https://github.com/lgiordani/smallcalc) in the branch called `part3`.

# Mezzanine - Refactoring

Often, after you wrote some code, you realise that some of the original choices you did are not perfect, especially when it comes to variable and function names. Furthermore, you can realise that some of your functions are too long, and you may consider splitting them in mutiple functions to make the code easier to understand and to use.

It is time, then to reconsider the code of smallcalc and see if we can improve it. Luckily, having all our tests in place, we may refactor it, that is we can change the code with a high degree of confidence, as the tests check that the behaviour of the whole system doesn't change.

The first change is the naming of the `parse_addsymbol()` method, which now can be more aptly named `_parse_symbol()`. As Martin Fowler says in his book "Refactoring" (a recommended reading): "_Life being what it is, you won't get your names right the first time. [...] Remember your code is for a human first and a computer second. Humans need good names._" The name of the method will be prefixed with an underscore because this method is used only internally, and shouldn't be used by third parties.

The proper way to change the name of a method involves calling the new method from the old one, but in this case we may safely rely on tests to tell us what needs to be fixed (this is because our codebase is small). We may thus open the file `smallcalc/calc_parser.py` and change the name to `_parse_symbol()`. At this point, running the test suite, you should have 11 failures. You can fix them with a text replace action of your editor of choice, but I recommend you to make the tests fail before replacing the text. The 3 replacements are in the `parse_factor()`, `parse_term()`, and `parse_expression()` methods.

I then wanted to add two methods `discard()` and `discard_type()` to the lexer, to better control what gets discarded. At the moment the code is using `self.lexer.get_token()` which doesn't allow to explicitly check what we are dropping. These are the tests that I added to `tests/test_calc_lexer.py`

``` python
import pytest

def test_discard_tokens():
    l = clex.CalcLexer()
    l.load('3 + 5')

    l.discard(token.Token(clex.INTEGER, '3'))
    assert l.get_token() == token.Token(clex.LITERAL, '+')


def test_discard_checks_equality():
    l = clex.CalcLexer()
    l.load('3 + 5')

    with pytest.raises(clex.TokenError):
        l.discard(token.Token(clex.INTEGER, '5'))


def test_discard_tokens_by_type():
    l = clex.CalcLexer()
    l.load('3 + 5')

    l.discard_type(clex.INTEGER)
    assert l.get_token() == token.Token(clex.LITERAL, '+')


def test_discard_type_checks_equality():
    l = clex.CalcLexer()
    l.load('3 + 5')

    with pytest.raises(clex.TokenError):
        l.discard_type(clex.LITERAL)
```

As you can see the idea is for both methods to require a parameter, either the token or the type. The code that passes these tests is made by a custom exception in `smallcalc/calc_lexer.py`

``` python
class TokenError(ValueError):

    """ The expected token cannot be found """


```

and, in the `CalcLexer` class in the same file

``` python
    def discard(self, token):
        if self.get_token() != token:
            raise TokenError(
                'Expected token {}, found {}'.format(
                    token, self._current_token
                ))

    def discard_type(self, _type):
        t = self.get_token()

        if t.type != _type:
            raise TokenError(
                'Expected token of type {}, found {}'.format(
                    _type, self._current_token.type
                ))
```

# Level 13 - Variables

## Lexer

To pass the `test_get_tokens_understands_letters` test I added a `_process_name()` method to the `CalcLexer` class

``` python
    def _process_name(self):
        regexp = re.compile('[a-z]+')

        match = regexp.match(
            self._text_storage.tail
        )

        if not match:
            return None

        token_string = match.group()

        return self._set_current_token_and_skip(
            token.Token(NAME, token_string)
        )
```

and then added it to the `get_token()` method. The new version of the latter is then

``` python
    def get_token(self):
        eof = self._process_eof()
        if eof:
            return eof

        eol = self._process_eol()
        if eol:
            return eol

        self._process_whitespace()

        name = self._process_name()
        if name:
            return name

        integer = self._process_integer()
        if integer:
            return integer

        literal = self._process_literal()
        if literal:
            return literal
```

At this point to pass the remaining tests `test_get_tokens_understands_uppercase_letters` and `test_get_tokens_understands_names_with_underscores` it is sufficient to change the regular expression we use in `_process_name()`

``` python
    def _process_name(self):
        regexp = re.compile('[a-zA-Z_]+')

        match = regexp.match(
            self._text_storage.tail
        )

        if not match:
            return None

        token_string = match.group()

        return self._set_current_token_and_skip(
            token.Token(NAME, token_string)
        )
```

## Parser

The required change to `parse_factor()` is simple, but since we will be returning a new type of node we have to define it

``` python
class VariableNode(ValueNode):

    node_type = 'variable'
```

We can then add the required `if` statement in `parse_factor()`, which becomes

``` python
    def parse_factor(self):
        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value in ['-', '+']:
            operator = self._parse_symbol()
            factor = self.parse_factor()
            return UnaryNode(operator, factor)

        if next_token.type == clex.LITERAL and next_token.value == '(':
            self.lexer.discard_type(clex.LITERAL)
            expression = self.parse_expression()
            self.lexer.discard_type(clex.LITERAL)
            return expression

        if next_token.type == clex.NAME:
            t = self.lexer.get_token()
            return VariableNode(t.value)

        return self.parse_integer()
```

The second test that we have to pass checks if the parser can understand variable assignments. First of all we need to define `AssignmentNode` which is the node we will return to the visitor.

``` python
class AssignmentNode(Node):

    node_type = 'assignment'

    def __init__(self, variable, value):
        self.variable = variable
        self.value = value

    def asdict(self):
        return {
            'type': self.node_type,
            'variable': self.variable.value,
            'value': self.value.asdict(),
        }
```

At this point we need a method to parse a variable in `CalcParser`. This method is very similar to `_parse_symbol()` and `parse_integer()`

``` python
    def _parse_variable(self):
        t = self.lexer.get_token()
        return VariableNode(t.value)
```

Since the test is running `parse_assignment()` we just need to add that method. We want the assignment to have a variable as its left member and an expression as its right member

``` python
    def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard_type(clex.LITERAL)
        value = self.parse_expression()

        return AssignmentNode(variable, value)
```

This code makes both the `test_parse_assignment()` and the `test_parse_assignment_with_expression()` tests pass.

## Visitor

As discussed in the introductory text before the test code the variable storage space can be a simple dictionary. The key will be the name of the variable, and the content will be another dictionary with `value` and `type`. This is sufficient for the moment and should be also extensible when future requirements will arise.

The `CalcVisitor` class can be then changed to get the new methods, and a `__init__()` that initializes the dictionary. I also added the relevant `if` statement to the `visit()` method of the same class. The new `CalcVisitor` is then

``` python
class CalcVisitor:

    def __init__(self):
        self.variables = {}

    def isvariable(self, name):
        return name in self.variables

    def valueof(self, name):
        return self.variables[name]['value']

    def typeof(self, name):
        return self.variables[name]['type']

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'unary':
            operator = node['operator']['value']
            cvalue, ctype = self.visit(node['content'])

            if operator == '-':
                return - cvalue, ctype

            return cvalue, ctype

        if node['type'] == 'binary':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            operator = node['operator']['value']

            if operator == '+':
                return lvalue + rvalue, rtype
            elif operator == '-':
                return lvalue - rvalue, rtype
            elif operator == '*':
                return lvalue * rvalue, rtype
            elif operator == '/':
                return lvalue // rvalue, rtype

        if node['type'] == 'assignment':
            right_value, right_type = self.visit(node['value'])
            self.variables[node['variable']] = {
                'value': right_value,
                'type': right_type
            }

            return None, None
```

To pass the second test we need only to change the `visit()` method adding an `if` statement for the `variable` nodes. The new version of the method is

``` python
    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'variable':
            return self.valueof(node['value']), self.typeof(node['value'])

        if node['type'] == 'unary':
            operator = node['operator']['value']
            cvalue, ctype = self.visit(node['content'])

            if operator == '-':
                return - cvalue, ctype

            return cvalue, ctype

        if node['type'] == 'binary':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            operator = node['operator']['value']

            if operator == '+':
                return lvalue + rvalue, rtype
            elif operator == '-':
                return lvalue - rvalue, rtype
            elif operator == '*':
                return lvalue * rvalue, rtype
            elif operator == '/':
                return lvalue // rvalue, rtype

        if node['type'] == 'assignment':
            right_value, right_type = self.visit(node['value'])
            self.variables[node['variable']] = {
                'value': right_value,
                'type': right_type
            }

            return None, None
```

# Level 14 - Parsing expressions and assignments

To pass the first test, as suggested, I added the `parse_line()` method as a wrapper around `parse_expression()`

``` python
    def parse_line(self):
        return self.parse_expression()
```

The second test requires some changes to `parse_line()`. As I do not know if the next token is an expression or an assignment I decided to stash the status and try one of the two. In case of error I just pop the state and try with the second option

``` python
    def parse_line(self):
        try:
            self.lexer.stash()
            return self.parse_assignment()
        except clex.TokenError:
            self.lexer.pop()
            return self.parse_expression()
```

At the same time `parse_assignment()` has to be changed. The current code parses a variable and then discards a literal, which is too generic, as an expression like `x * 2` will not raise an error. The new code for that method is then

``` python
    def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard(token.Token(clex.LITERAL, '='))
        value = self.parse_expression()

        return AssignmentNode(variable, value)
```

where I explicitly discard a literal `=` sign.

# Final words

Amazing job! We have now a calculator with variables, which is a good first step towards a full-fledged programming language. Try something new while you wait for the next post, like adding mathematical operations or, if you dare, functions!

# Updates

2017-12-24: The tuple returned by the `Visitor` class now uses `rtype` instead of `ltype`. This has been changed to be coherent with a change made in the tree construction mechanism. See the updates section of the first post in the series for a full explanation of the issue.

# Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
