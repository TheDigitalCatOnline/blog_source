Title: A game of tokens: write an interpreter in Python with TDD - Part 4
Date: 2018-06-02 13:00:00 +0000
Modified: 2020-08-05 11:00:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-4
Series: A game of tokens
Image: a-game-of-tokens-4
Summary:

## Introduction

In the first three parts of this series of posts we developed together a calculator using a pure TDD methodology. In the [previous post]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-3.markdown) we added support for variables.

In this new post we will first add the exponentiation operation. The operator will be challenging because it has a high priority, so we will need to spice up the peek functions to look at multiple tokens.

Then I will show you how I performed a refactoring of the code introducing a new version of the lexer that greatly simplifies the code of the parser.

## Level 15 - Exponentiation

*That is power.* - Conan the Barbarian (1982)

The exponentiation operation is simple, and Python uses the double star operator to represent it

``` python
>>> 2**3
8
```

The main problem that we will face implementing it is the priority of such an operation. Traditionally, this operator has precedence on the basic arithmetic operations (sum, difference, multiplication, division). So if I write

``` python
>>> 1 + 2 ** 3
9
```

Python correctly computes `1 + (2 ** 3)` and not `(1 + 2) ** 3`. As we did with multiplication and division, then, we will need to create a specific step to parse this operation.

In small calc, the exponentiation will be associated to the symbol `^`, so `2^3` will mean 2 to the power of 3 (`2**3` in Python).

### Lexer

The lexer has a simple task, that of recognising the symbol `'^'` as a `LITERAL` token. The test goes into `tests/test_calc_lexer.py`

``` python
def test_get_tokens_understands_exponentiation():
    l = clex.CalcLexer()

    l.load('2 ^ 3')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '2'),
        token.Token(clex.LITERAL, '^'),
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

Does your code already pass the test? If yes, why?

### Parser

It's time to test the proper parsing of the exponentiation operation. Add this test to `tests/test_calc_parser.py`

``` python
def test_parse_exponentiation():
    p = cpar.CalcParser()
    p.lexer.load("2 ^ 3")

    node = p.parse_exponentiation()

    assert node.asdict() == {
        'type': 'exponentiation',
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
            'value': '^'
        }
    }
```

As you can see this test checks directly the method `parse_exponentiation`, so you just need to properly implement that, at this stage.

To allow the use of the exponentiation operator `'^'` in the calculator, however, we have to integrate it with the rest of the parse functions, so we will add three tests to the same file. The first one tests that the natural priority of the exponentiation operator is higher than the multiplication

``` python
def test_parse_exponentiation_with_other_operators():
    p = cpar.CalcParser()
    p.lexer.load("3 * 2 ^ 3")

    node = p.parse_term()

    assert node.asdict() == {
        'type': 'binary',
        'operator': {
            'type': 'literal',
            'value': '*'
        },
        'left': {
            'type': 'integer',
            'value': 3
        },
        'right': {
            'type': 'exponentiation',
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
                'value': '^'
            }
        }
    }
```

The second one checks that the parentheses still change the priority

``` python
def test_parse_exponentiation_with_parenthesis():
    p = cpar.CalcParser()
    p.lexer.load("(3 + 2) ^ 3")

    node = p.parse_term()

    assert node.asdict() == {
        'type': 'exponentiation',
        'operator': {
            'type': 'literal',
            'value': '^'
        },
        'left': {
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
                'value': 2
            }
        },
        'right': {
            'type': 'integer',
            'value': 3
        }
    }
```

And the third one checks that unary operators still have a higher priority than the exponentiation operator

``` python
def test_parse_exponentiation_with_negative_base():
    p = cpar.CalcParser()
    p.lexer.load("-2 ^ 2")

    node = p.parse_exponentiation()

    assert node.asdict() == {
        'type': 'exponentiation',
        'operator': {
            'type': 'literal',
            'value': '^'
        },
        'left': {
            'type': 'unary',
            'operator': {
                'type': 'literal',
                'value': '-'
            },
            'content': {
                'type': 'integer',
                'value': 2
            }
        },
        'right': {
            'type': 'integer',
            'value': 2
        }
    }
```

My advice is to add the first test and to try and pass that one changing the code. If your change doesn't touch too much of the existing parse methods, chances are that the following two tests will pass as well.

### Visitor

Last, we need to properly expose the exponentiation operation in the CLI, which means to change the Visitor in order to support nodes of type `'exponentiation`. The test that we need to add to `tests/test_calc_visitor.py` is

``` python
def test_visitor_exponentiation():
    ast = {
        'type': 'exponentiation',
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
            'value': '^'
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (8, 'integer')
```

And the change to the `CalcVisitor` class should be very easy as we need to simply process a new type of node.

---

### Solution

The lexer can process the exponentiation operator `^` out of the box as a `LITERAL` token, so no changes to the code are needed.

The test `test_parse_exponentiation` can be passed adding a `PowerNode` class.

Note: After I wrote and committed the solution I realised that the class called `PowerNode` should have been called `ExponentiationNode`, the former being a leftover of a previous incorrect nomenclature. I will eventually fix it in one of the refactoring steps, trying to convert a mistake into a good example of TDD.

``` python
class PowerNode(BinaryNode):
    node_type = 'exponentiation'
```

and a method `parse_exponentiation` to the parser

``` python
    def parse_exponentiation(self):
        left = self.parse_factor()

        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value == '^':
            operator = self._parse_symbol()
            right = self.parse_exponentiation()

            return PowerNode(left, operator, right)

        return left
```

This allows the parser to explicitly parse the exponentiation operation, but when the operation is mixed with others the parser doesn't know how to deal with it, as `parse_exponentiation` is not called by any other method.

To pass the `test_parse_exponentiation_with_other_operators` test we need to change the method `parse_term` and call `parse_exponentiation` instead of `parse_factor`

``` python
    def parse_term(self):
        left = self.parse_exponentiation()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self._parse_symbol()
            right = self.parse_exponentiation()

            left = BinaryNode(left, operator, right)
```

the full code of the `CalcParser` class is now

``` python
class CalcParser:

    def __init__(self):
        self.lexer = clex.CalcLexer()

    def _parse_symbol(self):
        t = self.lexer.get_token()
        return LiteralNode(t.value)

    def parse_integer(self):
        t = self.lexer.get_token()
        return IntegerNode(t.value)

    def _parse_variable(self):
        t = self.lexer.get_token()
        return VariableNode(t.value)

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

    def parse_exponentiation(self):
        left = self.parse_factor()

        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value == '^':
            operator = self._parse_symbol()
            right = self.parse_exponentiation()

            return PowerNode(left, operator, right)

        return left

    def parse_term(self):
        left = self.parse_exponentiation()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self._parse_symbol()
            right = self.parse_exponentiation()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left

    def parse_expression(self):
        left = self.parse_term()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['+', '-']:
            operator = self._parse_symbol()
            right = self.parse_term()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left

    def parse_assignment(self):
        variable = self._parse_variable()
        self.lexer.discard(token.Token(clex.LITERAL, '='))
        value = self.parse_expression()

        return AssignmentNode(variable, value)

    def parse_line(self):
        try:
            self.lexer.stash()
            return self.parse_assignment()
        except clex.TokenError:
            self.lexer.pop()
            return self.parse_expression()
```

The given test `test_visitor_exponentiation` requires the `CalcVisitor` to parse nodes of type `exponentiation`. The code required to do this is

``` python
        if node['type'] == 'exponentiation':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            return lvalue ** rvalue, ltype
```

as a final case for the `CalcVisitor` class. The full code of the class is is now

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

## Intermezzo - Refactoring with tests

*See? You just had to see it in context.* - Scrooged (1988)

So our little project is growing, and the TDD methodology we are following gives us plenty of confidence in what we did. There as for sure bugs we are not aware of, but we are sure that the cases that we tested are correctly handled by our code.

As happens in many projects at a certain point it's time for refactoring. We implemented solutions to the problems that we found along the way, but are we sure we avoided duplicating code, that we chose the best solution for some algorithms, or more simply that the names we chose for the variables are clear?

Refactoring means basically to change the internal structure of something without changing its external behaviour, and tests are a priceless help in this phase. The tests we wrote are there to ensure that the previous behaviour does not change. Or, if it changes, that we are perfectly aware of it.

In this section, thus, I want to guide you through a refactoring guided by tests. If you are following this series and writing your own code this section will not add anything to the project, but I recommend that you read it anyway, as it shows why tests are so important in a software project.

If you want to follow the refactoring on the repository you can create a branch on the tag `context-manager-refactoring` and work there. From that commit I implemented the steps you will find in the next sections.

### Context managers

The main issue the current code has is that the lexer cannot automatically recover a past status, that is we cannot easily try to parse something and, when we discover that the initial guess is wrong, go back in time and start over.

Let's consider the method `parse_line`

``` python
    def parse_line(self):
        try:
            self.lexer.stash()
            return self.parse_assignment()
        except clex.TokenError:
            self.lexer.pop()
            return self.parse_expression()
```

Since a line can contain either an assignment or an expression the first thing this function does is to save the lexer status with `stash` and try to parse an assignment. If the code is not an assignment somewhere is the code the `TokenError` exception is raised, and `parse_line` restores the previous status of the lexer and tries to parse an expression.

The same thing happens in other methods like `parse_term`

``` python
    def parse_term(self):
        left = self.parse_exponentiation()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self._parse_symbol()
            right = self.parse_exponentiation()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

where the use of `lexer.peek_token` and the `while` loop show that the lexer class requires too much control from its user.

Back to `parse_line`, it's clear that the code works, but it is not immediately easy to understand what the function does and when the old status is recovered. I would really prefer something like

``` python
# PSEUDOCODE
     def parse_line(self):
        ATTEMPT:
            return self.parse_assignment()
        
        return self.parse_expression()
```

where I used a pseudo-keyword `ATTEMPT:` to signal that somehow the lexer status is automatically stored at the beginning and retrieved at the end of it.

There's a very powerful concept in Python that allows us to write code like this, and it is called _context manager_. I won't go into the theory and syntax of context managers here, please refer to the Python documentation or your favourite course/book/website to discover how context managers work.

If I can add context manager features to the lexer the code of `parse_line` might become

``` python
     def parse_line(self):
        with self.lexer:
            return self.parse_assignment()
        
        return self.parse_expression()
```

### Lexer

The first move is to transform the lexer into a context manager that does nothing. The test in `tests/test_calc_lexer.py` is

``` python
def test_lexer_as_context_manager():
    l = clex.CalcLexer()
    l.load('abcd')

    with l:
        assert l.get_token() == token.Token(clex.NAME, 'abcd')
```

When this works we have to be sure that the lexer does not restore the previous state outside the `with` statement if the code inside the statement ended without errors. The new test is

``` python
def test_lexer_as_context_manager_does_not_restore_the_status_if_no_error():
    l = clex.CalcLexer()
    l.load('3 * 5')

    with l:
        assert l.get_token() == token.Token(clex.INTEGER, 3)

    assert l.get_token() == token.Token(clex.LITERAL, '*')
```

Conversely, we need to be sure that the status is restored when the code inside the `with` statement fails, which is the whole point of the context manager. This is tested by

``` python
def test_lexer_as_context_manager_restores_the_status_if_token_error():
    l = clex.CalcLexer()
    l.load('3 * 5')

    with l:
        l.get_token()
        l.get_token()
        raise clex.TokenError

    assert l.get_token() == token.Token(clex.INTEGER, 3)
```

When these three tests pass, we have a fully working context manager lexer, that reacts to `TokenError` exceptions going back to the previously stored status.

### Parser

If the context manager lexer works as intended we should be able to replace the code of the parser without changing any test. The new code for `parse_line` is the one I showed before

``` python
    def parse_line(self):
        with self.lexer:
            return self.parse_assignment()

        return self.parse_expression()
```

and it works flawlessly.

The context manager part of the lexer, however, works if the code inside the `with` statement raises a `TokenError` exception when it fails. That exception is a signal to the context manager that the parsing path is not leading anywhere and it shall go back to the previous state.

#### Manage literals

The method `_parse_symbol` is often used after some checks like

``` python
        if next_token.type == clex.LITERAL and next_token.value in ['-', '+']:
            operator = self._parse_symbol()
```

I would prefer to include the checks in the method itself, so that it might be included in a `with` statement. First of all the method can be renamed to `_parse_literal`, and being an internal method I don't expect any test to fail

``` python
    def _parse_literal(self):
        t = self.lexer.get_token()
        return LiteralNode(t.value)
```

The method should also raise a `TokenError` when the token is not a `LITERAL`, and when the values are not the expected ones

``` python
    def _parse_literal(self, values=None):
        t = self.lexer.get_token()

        if t.type != clex.LITERAL:
            raise clex.TokenError

        if values and t.value not in values:
            raise clex.TokenError

        return LiteralNode(t.value)
```

Note that using the default value for the `values` parameter I didn't change the current behaviour. The whole battery of tests still passes without errors.

#### Parsing factors

The next method that we can start changing is `parse_factor`. The first pattern this function tries to parse is an unary node

``` python
        if next_token.type == clex.LITERAL and next_token.value in ['-', '+']:
            operator = self._parse_literal()
            factor = self.parse_factor()
            return UnaryNode(operator, factor)
```

which may be converted to

``` python
        with self.lexer:
            operator = self._parse_literal(['+', '-'])
            content = self.parse_factor()
            return UnaryNode(operator, content)
```

while still passing the whole test suite.

The second pattern are expressions surrounded by parentheses

``` python
        if next_token.type == clex.LITERAL and next_token.value == '(':
            self.lexer.discard_type(clex.LITERAL)
            expression = self.parse_expression()
            self.lexer.discard_type(clex.LITERAL)
            return expression
```

and this is easily converted to the new syntax as well

``` python
        with self.lexer:
            self._parse_literal(['('])
            expression = self.parse_expression()
            self._parse_literal([')'])
            return expression
```

#### Parsing exponentiation operations

To change the method `parse_exponentiation` we need first to make the `_parse_variable` return a `TokenError` in case of wrong token

``` python
    def _parse_variable(self):
        t = self.lexer.get_token()

        if t.type != clex.NAME:
            raise clex.TokenError

        return VariableNode(t.value)
```

then we can change the method we are interested in

``` python
    def parse_exponentiation(self):
        left = self.parse_factor()

        with self.lexer:
            operator = self._parse_literal(['^'])
            right = self.parse_exponentiation()

            return PowerNode(left, operator, right)

        return left
```

Doing this last substitution I also notice that there is some duplicated code in `parse_factor`, and I replace it with a call to `_parse_variable`. The test suite keeps passing, giving me the certainty that what I did does not change the behaviour of the code (at least the behaviour that is covered by tests).

#### Parsing terms

Now, the method `parse_term` will be problematic. To implement this function I used a `while` loop that keeps using the method `parse_exponentiation` until the separation token is a `LITERAL` with value `*` or `/`

``` python
    def parse_term(self):
        left = self.parse_exponentiation()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self._parse_literal()
            right = self.parse_exponentiation()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

This is not a pure recursive call, then, and replacing the code with the context manager lexer would result in errors, because the context manage doesn't loop. The same situation is replicated in `parse_expression`.

This is another typical situation that we face when refactoring code. We realise that the required change is made of multiple steps and that multiple tests will fail until all the steps are completed.

There is no single solution to this problem, but TDD gives you some hints to deal with it. The most important "rule" that I follow when I work in a TDD environment is that there should be maximum one failing test at a time. And when a code change makes multiple tests fail there is a simple way to reach this situation: comment out tests.

Yes, you should temporarily get rid of tests, so you can concentrate in writing code that passes the subset of active tests. Then you will add one test at a time, fixing the code or the tests according to your needs. When you refactor it might be necessary to change the tests as well, as sometimes we test part of the code that are not exactly an external boundary.

I can now change the code of the `parse_term` function introducing the context manager

``` python
    def parse_term(self):
        left = self.parse_exponentiation()

        with self.lexer:
            operator = self._parse_literal(['*', '/'])
            right = self.parse_exponentiation()

            return BinaryNode(left, operator, right)

        return left
```

and the test suite runs with one single failing test, `test_parse_term_with_multiple_operations`. I have now to work on it and try to understand why the test fails.

I decided to go for a pure recursive approach (no more `while` loops), which is what standard language parsers do. After working on it the new version of `parse_term` is

``` python
    def parse_term(self):
        left = self.parse_factor()

        with self.lexer:
            operator = self._parse_literal(['*', '/'])
            right = self.parse_term()

            return BinaryNode(left, operator, right)

        return left
```

And this change makes 3 tests fail. The same `test_parse_term_with_multiple_operations` that was failing before, plus `test_parse_exponentiation_with_other_operators`, and `test_parse_exponentiation_with_parenthesis`. The last two actually test the method `parse_exponentiation`, which uses `parse_term`. This means that I can temporarily comment them and concentrate on the first one.

What I discover is that changing the code to use the recursive approach changes the output of the functions. The previous output of `parse_term` applied to `2 * 3 / 4` was

``` python
{
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
            'value': '*'
        }
    },
    'right': {
        'type': 'integer',
        'value': 4
    },
    'operator': {
        'type': 'literal',
        'value': '/'
    }
}
```

that is, the multiplication was stored first. Moving to a recursive approach makes the `parse_term` function return this

``` python
{
    'type': 'binary',
    'left': {
        'type': 'integer',
        'value': 2
    },
    'right': {
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
            'value': '/'
        }
    },
    'operator': {
        'type': 'literal',
        'value': '*'
    }
}
```

I think it is pretty clear that this structure, once visited, will return the same value as the previous one, as multiplication and division can be swapped. We will have to pay attention to this swap when operators with different priority are involved, like sum and multiplication, but for this test we can agree the result is no different.

This means that we may change the test and make it pass. Let me stress it once more: we have to understand why the test doesn't pass, and once we understood the reason, and decided it is acceptable, we can change the test.

Tests are not immutable, they are mere safeguards that raise alarms when you change the behaviour. It's up to you to deal with the alarm and to decide what to do.

Once the test has been modified and the test suite passes, it's time to uncomment the first of the two remaining tests, `test_parse_exponentiation_with_other_operators`. This test uses `parse_term` to parse a string that contains an exponentiation, but the new code of the method `parse_term` doesn't call the `parse_exponentiation` function. So the test fails.

#### Parsing exponentiation

That tests tries to parse a string that contains a multiplication and an exponentiation, so the method that we should use to process it is `parse_term`. The current version of `parse_term`, however, doesn't consider exponentiation, so the new code is

``` python
    def parse_term(self):
        left = self.parse_exponentiation()

        with self.lexer:
            operator = self._parse_literal(['*', '/'])
            right = self.parse_term()

            return BinaryNode(left, operator, right)

        return left
```

which makes all the active tests pass.

There is still one commented test, `test_parse_exponentiation_with_parenthesis`, that now passes with the new code.

#### Parsing expressions

The new version of `parse_expression` has the same issue we found with `parse_term`, that is the recursive approach changes the output.

``` python
    def parse_expression(self):
        left = self.parse_term()

        with self.lexer:
            operator = self._parse_literal(['+', '-'])
            right = self.parse_expression()

            left = BinaryNode(left, operator, right)

        return left
```

As before, we have to decide if the change is acceptable or if it is a real error. As happened for `parse_term` the test can be safely changes to match the new code output.

## Level 16 - Float numbers

So far, our calculator can handle only integer values, so it's time to add support for float numbers. This change shouldn't be complex: floating point numbers are easy to parse as they are basically two integer numbers separated by a dot.

### Lexer

To test if the lexer understands floating point numbers it's enough to add this to `tests/test_calc_lexer.py`

``` python
def test_get_tokens_understands_floats():
    l = clex.CalcLexer()

    l.load('3.6')

    assert l.get_tokens() == [
        token.Token(clex.FLOAT, '3.6'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

### Parser

To support float numbers it's enough to add that feature the method that we use to parse integers. We can rename `parse_integer` to `parse_number`, fix the test `test_parse_integer`, and add `test_parse_float`

``` python
def test_parse_integer():
    p = cpar.CalcParser()
    p.lexer.load("5")

    node = p.parse_number()

    assert node.asdict() == {
        'type': 'integer',
        'value': 5
    }


def test_parse_float():
    p = cpar.CalcParser()
    p.lexer.load("5.8")

    node = p.parse_number()

    assert node.asdict() == {
        'type': 'float',
        'value': 5.8
    }
```

### Visitor

The same thing that we did for the parser is valid for the visitor. We just need to copy the test for the integer numbers and adapt it

``` python
def test_visitor_float():
    ast = {
        'type': 'float',
        'value': 12.345
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (12.345, 'float')
```

This however leaves a bug in the visitor. The Test-Driven Development methodology can help you writing and changing your code, but cannot completely avoid bugs in the code. Actually, if you don't test cases, TDD can't do anything to find and remove bugs.

The bug I noticed after a while is that the visitor doesn't correctly manage an operation between integers and floats, returning an integer result. For example, if you sum `4` with `5.1` you should get `9.1` with type `float`. To test this behaviour we can add this code

``` python
def test_visitor_expression_sum_with_float():
    ast = {
        'type': 'binary',
        'left': {
            'type': 'float',
            'value': 5.1
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
    assert v.visit(ast) == (9.1, 'float')
```

---

### Solution

The first thing the lexer need is a label to identify `FLOAT` tokens

``` python
FLOAT = 'FLOAT'
```

then the method `_process_integer` cna be extended to process float numbers as well. To do this the method is renamed to `_process_number`, the regular expression is modified, and the `token_type` is managed according to the presence of the dot.

``` python
    def _process_number(self):
        regexp = re.compile('[\d\.]+')

        match = regexp.match(
            self._text_storage.tail
        )

        if not match:
            return None

        token_string = match.group()

        token_type = FLOAT if '.' in token_string else INTEGER

        return self._set_current_token_and_skip(
            token.Token(token_type, token_string)
        )
```

Remember that the `get_token` function has to be modified to use the new name of the method. The new code is

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

        integer = self._process_number()
        if integer:
            return integer

        literal = self._process_literal()
        if literal:
            return literal
```

First we need to add a new type of node

``` python
class FloatNode(ValueNode):
    node_type = 'float'
```

The new version of `parse_integer`, renamed `parse_number`, shall deal with both cases but also raise the `TokenError` exception if the parsing fails

``` python
    def parse_number(self):
        t = self.lexer.get_token()

        if t.type == clex.INTEGER:
            return IntegerNode(int(t.value))
        elif t.type == clex.FLOAT:
            return FloatNode(float(t.value))

        raise clex.TokenError
```

The change to support `float` nodes is trivial, we just need to include it alongside with the `integer` case

``` python
    def visit(self, node):
        if node['type'] in ['integer', 'float']:
            return node['value'], node['type']
```

To fix the missing type promotion when dealing with integers and floats it's enough to add 

``` python
            if ltype == 'float':
                rtype = ltype
```

just before evaluating the operator in the binary nodes. The full code of the method `visit` is then

``` python
    def visit(self, node):
        if node['type'] in ['integer', 'float']:
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

            if ltype == 'float':
                rtype = ltype

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

        if node['type'] == 'exponentiation':
            lvalue, ltype = self.visit(node['left'])
            rvalue, rtype = self.visit(node['right'])

            return lvalue ** rvalue, ltype
```

---

## Final words

This post showed you how powerful the TDD methodology is when it comes to refactoring, or in general when the code has to be changed. Remember that tests can be changed if there are good reasons, and that the main point is to understand what's happening in your code and in the cases that you already tested.

The code I developed in this post is available on the GitHub repository tagged with `part4` ([link](https://github.com/lgiordani/smallcalc/tree/part4)).

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

