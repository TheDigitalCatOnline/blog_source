Title: A game of tokens: write an interpreter in Python with TDD - Part 3
Date: 2017-10-31 11:00:00 +0000
Modified: 2020-08-05 11:00:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-3
Series: A game of tokens
Image: a-game-of-tokens
Summary:

## Introduction

This is the third instalment of a series of posts on how to write an interpreter in Python. In the [first part]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1.markdown) we developed together a small command line calculator that could sum and subtract numbers, while in the [second part]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2.markdown) we went further adding multiplication, division and unary plus and minus.

In this third part we wil start adding variables to our calculator, moving towards a proper programming language.

## Mezzanine - Refactoring

Often, after wroting some code, you realise that some of the original choices you did are not perfect, especially when it comes to variable and function names. Furthermore, you can realise that some of your functions are too long, and you may consider splitting them in mutiple functions to make the code easier to understand and to use.

It is time, then, to reconsider the code of smallcalc and see if we can improve it. Luckily, having all our tests in place, we may refactor it, that is we can change the code with a high degree of confidence, as the tests check that the behaviour of the whole system doesn't change.

The first change is the naming of the method `parse_addsymbol`, which now can be more aptly named `_parse_symbol`. As Martin Fowler says in his book "Refactoring" (a recommended reading): "_Life being what it is, you won't get your names right the first time. [...] Remember your code is for a human first and a computer second. Humans need good names._" The name of the method will be prefixed with an underscore because this method is used only internally, and shouldn't be used by third parties.

The proper way to change the name of a method involves calling the new method from the old one, but in this case we may safely rely on tests to tell us what needs to be fixed (this is because our codebase is small). We may thus open the file `smallcalc/calc_parser.py` and change the name to `_parse_symbol`. At this point, running the test suite, you should have 11 failures. You can fix them with a text replace action of your editor of choice, but I recommend you to make the tests fail before replacing the text. The 3 replacements are in the methods `parse_factor`, `parse_term`, and `parse_expression`.

I then wanted to add two methods, `discard` and `discard_type`, to the lexer, to better control what gets discarded. At the moment the code is using `self.lexer.get_token` which doesn't allow to explicitly check what we are dropping. These are the tests that I added to `tests/test_calc_lexer.py`

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

As I am satisfied with the code that I have now, I will move on to add new features.

## Level 13 - Variables

*I have been assigned by my strength and cunning.* - Up (2009)

Variables are labels assigned to values, so what we need to add is a way for the user to make this assignment and then to use variables intead of actual values. The simplest syntax, used by many languages is `name = value` and we will stick to this. Usually languages allow only a subset of symbols in the name of a variable so we will learn how to use lower- and uppercase names that may also contain an underscore.

### Lexer

We want the lexer to recognise a new token called `NAME`, so the test we have to add to `tests/test_calc_lexer.py` is

``` python
def test_get_tokens_understands_letters():
    l = clex.CalcLexer()

    l.load('somevar')

    assert l.get_tokens() == [
        token.Token(clex.NAME, 'somevar'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

This test checks only the support for lowercase letters. Since we want to support also uppercase letters and underscores we need another pair of test

``` python
def test_get_tokens_understands_uppercase_letters():
    l = clex.CalcLexer()

    l.load('SomeVar')

    assert l.get_tokens() == [
        token.Token(clex.NAME, 'SomeVar'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]


def test_get_tokens_understands_names_with_underscores():
    l = clex.CalcLexer()

    l.load('some_var')

    assert l.get_tokens() == [
        token.Token(clex.NAME, 'some_var'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

If you wonder why I created two tests instead of just one with both uppercase letters and underscores, the reason is that I generally prefer to have tests that focus on one specific feature. This obviously depends on the level of granularity that you want, and in this case we are discussing very simple features, so I would not argue if I saw both tested at the same time.

### Parser

To support variables in expressions we need to change the behaviour of `parse_factor`, which is the method where we parse the building blocks like integers of unary operators. The test you need to add to `tests/test_calc_parser.py` is

``` python
def test_parse_factor_variable():
    p = cpar.CalcParser()
    p.lexer.load("somevar")

    node = p.parse_factor()

    assert node.asdict() == {
        'type': 'variable',
        'value': 'somevar'
    }
```

After this we want to provide support for variable assignments. Working on the parser we need only to output the correct node so the test is pretty straightforward

``` python
def test_parse_assignment():
    p = cpar.CalcParser()
    p.lexer.load("x = 5")

    node = p.parse_assignment()

    assert node.asdict() == {
        'type': 'assignment',
        'variable': 'x',
        'value': {
            'type': 'integer',
            'value': 5
        }
    }
```

This test tries to assign the value `5` to the variable `x`, but in general we want to support assignment with expressions, so we should test this behaviour as well, including the presence of variables

``` python
def test_parse_assignment_with_expression():
    p = cpar.CalcParser()
    p.lexer.load("x = 4 * (3 + 5)")

    node = p.parse_assignment()

    assert node.asdict() == {
        'type': 'assignment',
        'variable': 'x',
        'value': {
            'type': 'binary',
            'operator': {
                'type': 'literal',
                'value': '*'
            },
            'left': {
                'type': 'integer',
                'value': 4
            },
            'right': {
                'type': 'binary',
                'operator': {
                    'type': 'literal',
                    'value': '+'
                },
                'left': {
                    'type': 'integer',
                    'value': 3
                },
                'right': {
                    'type': 'integer',
                    'value': 5
                }
            }
        }
    }


def test_parse_assignment_expression_with_variables():
    p = cpar.CalcParser()
    p.lexer.load("x = y + 4")

    node = p.parse_assignment()

    assert node.asdict() == {
        "type": "assignment",
        "variable": "x",
        'value': {
            'type': 'binary',
            'operator': {
                'type': 'literal',
                'value': '+'
            },
            'left': {
                'type': 'variable',
                'value': 'y'
            },
            'right': {
                'type': 'integer',
                'value': 4
            },
        }
    }
```

### Visitor

It is now time to implement the code that actually stores and retrieves variables, which is what happens in the visitor when an `assignment` or a `variable` node are processed. For the moment we do not have specific requirements for variables and we can treat the storage space as a big global dictionary.

The test we want to pass specifies the initial API of the storage space when we assign a value to a variable

``` python
def test_visitor_assignment():
    ast = {
        'type': 'assignment',
        'variable': 'x',
        'value': {
            'type': 'integer',
            'value': 5
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (None, None)
    assert v.isvariable('x') is True
    assert v.valueof('x') == 5
    assert v.typeof('x') == 'integer'
```

We want the visitor to provide three new methods, `isvariable`, `valueof`, and `typeof`, that allow us to interact with the variables we defined.

The last change that the visitor requires is some code that allows it to read the value of variables to be able to use them when computing the result of an expression. The test is then

``` python
def test_visitor_variable():
    assignment_ast = {
        'type': 'assignment',
        'variable': 'x',
        'value': {
            'type': 'integer',
            'value': 123
        }
    }

    read_ast = {
        'type': 'variable',
        'value': 'x'
    }

    v = cvis.CalcVisitor()
    v.visit(assignment_ast)
    assert v.visit(read_ast) == (123, 'integer')
```

where two different ASTs have been created. The first one assigns a value to the variable, the second one reads it and returns its value. Note that the visitor returns both value and type of the variable, which seems reasonable to implement later checks of equality or other operations on variables.

---

### Solution

To pass the `test_get_tokens_understands_letters` test I added a method `_process_name` to the `CalcLexer` class

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

and then added it to the method `get_token`. The new version of the latter is then

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

At this point to pass the remaining tests `test_get_tokens_understands_uppercase_letters` and `test_get_tokens_understands_names_with_underscores` it is sufficient to change the regular expression we use in `_process_name`

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

The required change to `parse_factor` is simple, but since we will be returning a new type of node we have to define it

``` python
class VariableNode(ValueNode):

    node_type = 'variable'
```

We can then add the required `if` statement in `parse_factor`, which becomes

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

At this point we need a method to parse a variable in `CalcParser`. This method is very similar to `_parse_symbol` and `parse_integer`

``` python
    def _parse_variable(self):
        t = self.lexer.get_token()
        return VariableNode(t.value)
```

Since the test is running `parse_assignment` we just need to add that method. We want the assignment to have a variable as its left member and an expression as its right member

``` python
    def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard_type(clex.LITERAL)
        value = self.parse_expression()

        return AssignmentNode(variable, value)
```

This code makes both the `test_parse_assignment` and the `test_parse_assignment_with_expression` tests pass.

As discussed in the introductory text before the test code the variable storage space can be a simple dictionary. The key will be the name of the variable, and the content will be another dictionary with `value` and `type`. This is sufficient for the moment and should be also extensible when future requirements will arise.

The `CalcVisitor` class can be then changed to get the new methods, and a `__init__` that initializes the dictionary. I also added the relevant `if` statement to the method `visit` of the same class. The new `CalcVisitor` is then

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

To pass the second test we need only to change the method `visit` adding an `if` statement for the `variable` nodes. The new version of the method is

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

---

## Level 14 - Parsing expressions and assignments

*Speak words we can all understand!* - The Lord of the Rings: The Fellowship of the Ring (2001)

We are missing a final step. The CLI uses `parse_expression` as its default entry point, which means that it doesn't understand variable assignments for the time being. We need then to introduce a new entry point `parse_line` that we will use to process general language statements. The test for this goes in `tests/test_calc_parser.py`

``` python
def test_parse_line_supports_expression():
    p = cpar.CalcParser()
    p.lexer.load("2 * x + 4")

    node = p.parse_line()

    assert node.asdict() == {
        'type': 'binary',
        'left': {
            'type': 'binary',
            'left': {
                'type': 'integer',
                'value': 2
            },
            'right': {
                'type': 'variable',
                'value': 'x'
            },
            'operator': {
                'type': 'literal',
                'value': '*'
            }
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
```

and checks that `parse_line` can parse expressions (which can be solved just wrapping `parse_expression` with it). The second test checks that `parse_line` can parse variable assignments and goes in the same file

``` python
def test_parse_line_supports_assigment():
    p = cpar.CalcParser()
    p.lexer.load("x = 5")

    node = p.parse_line()

    assert node.asdict() == {
        'type': 'assignment',
        'variable': 'x',
        'value': {
            'type': 'integer',
            'value': 5
        }
    }
```

At this point we can change the entry point in the CLI, using `parse_line` instead of `parse_expression`. The new CLI is then

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

            node = p.parse_line()
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

Try to fire the CLI and enjoy a calculator with variables! Everything works, but you now know it is not magic, but the outcome of a good amount of code. And you wrote it, so you may proudly say that you created a simple but working programming language.

---

### Solution

To pass the first test, as suggested, I added the method `parse_line` as a wrapper around `parse_expression`

``` python
    def parse_line(self):
        return self.parse_expression()
```

The second test requires some changes to `parse_line`. As I do not know if the next token is an expression or an assignment I decided to stash the status and try one of the two. In case of error I just pop the state and try with the second option

``` python
    def parse_line(self):
        try:
            self.lexer.stash()
            return self.parse_assignment()
        except clex.TokenError:
            self.lexer.pop()
            return self.parse_expression()
```

At the same time `parse_assignment` has to be changed. The current code parses a variable and then discards a literal, which is too generic, as an expression like `x * 2` will not raise an error. The new code for that method is then

``` python
    def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard(token.Token(clex.LITERAL, '='))
        value = self.parse_expression()

        return AssignmentNode(variable, value)
```

where I explicitly discard a literal `=` sign.

---

## Final words

Managing variables may look like a very easy task, but as soon as we will start implementing functions and local scopes we will have to move to something richer than a simple global dictionary. Memory management is another big topic that I didn't touch here, perhaps in the future I might discuss garbage collections and related problems.

The code I developed in this post is available on the GitHub repository tagged with `part3` ([link](https://github.com/lgiordani/smallcalc/tree/part3)).

In the next issue I will face with you the task of adding the power operator, support for floating point numbers, and a big refactoring with context managers that will greatly simplify the code.

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

