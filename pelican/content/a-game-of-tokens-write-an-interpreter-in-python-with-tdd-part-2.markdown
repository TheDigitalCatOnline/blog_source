Title: A game of tokens: write an interpreter in Python with TDD - Part 2
Date: 2017-10-01 15:00:00 +0100
Modified: 2020-08-05 11:00:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2
Series: A game of tokens
Image: a-game-of-tokens-2
Summary:

## Introduction

Welcome to the second part of the series of posts about writing an interpreter with Python and TDD. In the [first post]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1.markdown) we developed together a simple calculator that can handle integers, addition and subtraction. In this instalment I'll give you new tests that will guide you through the implementation of multiplication, division, parentheses, and unary operators. I will obviously reference the structure I used in my solution, but you mileage may vary, so feel free to ignore the comments or the suggested solutions in case your code is different.

## Level 8 - Multiplication and division

*"They're coming outta the walls. They're coming outta the goddamn walls."* - Aliens (1986)

As you remember from the previous post our interpreter is made of three different components, the lexer, the parser, and the visitor. So, to implement the missing basic operations, multiplication and division, we need to start with the lexer and ensure that it understands the traditional symbols `*` and `/`

### Lexer

Put the following tests in the `tests/test_calc_lexer.py` file

``` python
def test_get_tokens_understands_multiplication():
    l = clex.CalcLexer()

    l.load('3 * 5')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '*'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]


def test_get_tokens_understands_division():
    l = clex.CalcLexer()

    l.load('3 / 5')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '/'),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

Do the tests fail? Why? Please remember that when tests pass without requiring any code change you have to ask yourself "Why do they pass?", and be sure that you understood the answer before going further. Otherwise you might be adding tests that are wrong, or tests for things that have already been tested, and in either case you should act on them.

### Parser

Now that the lexer understands the symbols we can start considering the parser. The parser has to output a sensible structure that represents the new operations, which is not different from what it outputs for the sum and the difference. Add the following tests to `tests/test_calc_parser.py`

``` python
def test_parse_term():
    p = cpar.CalcParser()
    p.lexer.load("2 * 3")

    node = p.parse_term()

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
            'value': '*'
        }
    }


def test_parse_term_with_multiple_operations():
    p = cpar.CalcParser()
    p.lexer.load("2 * 3 / 4")

    node = p.parse_term()

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

This time you should have some failures, so go and edit the `CalcParser` class in order to pass the tests. As the two new binary operations are at this level the same as sum and difference you _could_ change the method `parse_expression` (try it!). This will however make things harder later when we will prioritise operations (multiplications have to be performed before sums), so my advice is to introduce a method `parse_term` in the parser, which is the method used in the tests.

### Visitor

Now it's the visitor's turn, where the syntax tree gets analysed and actually executed. Add the following tests to the `tests/test_calc_visitor.py` file and then make them pass changing the `CalcVisitor` class accordingly.

``` python
def test_visitor_term_multiplication():
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
            'value': '*'
        }
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (20, 'integer')


def test_visitor_term_division():
    ast = {
        'type': 'binary',
        'left': {
                'type': 'integer',
                'value': 11
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

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (2, 'integer')
```

---

### Solution

The tests we added for the lexer already pass. This is not surprising, as the lexer is designed to return everything it doesn't know as a `LITERAL` (`smallcalc/calc_lexer.py:119`). As we already instructed the lexer to skip spaces the new operators are happily digested. As I discussed in the previous post, I decided for this project not to assign operators a specific token, so from this point of view our lexer is pretty open and could already understand instructions like `3 $ 5` or `7 : 9`, even though they do not have any meaning in our new language (yet, maybe).

The parser is not so merciful, and the two new tests do not pass. We are explicitly calling a method `parse_term` that is not defined, so a success would have been very worrying. In these two tests `parse_term` is called explicitly and there is no relationship with the other methods named `parse_*`, so we can implement it as a stand-alone processing.

We know that a `term` is an operation between two integers, so we can follow what we did with `parse_expression`. The first thing we do is to parse the first integer, then we peek the next token and we decide what to do. If the token is a `LITERAL` we suppose it is the operation symbol, otherwise we probably hit the end of the file and we will just return the previously read integer. The second element may be a simple integer or another multiplication or division, so we recursively call `parse_term` and return a `BinaryNode` with the result.

[Note: I noticed that the `parse_addsymbol` could be now named `parse_literal` but this wasn't done when I prepared the source code. Regardless of the name, however, what this method does is to just pack a literal in a `LiteralNode` and return it.]

The whole parser is now the following

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

    def parse_term(self):
        left = self.parse_integer()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL:
            operator = self.parse_addsymbol()
            right = self.parse_integer()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left

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

The visitor was instructed only to deal with sums and subtractions, and it treats everything is not the former as the latter. This is why the new tests give as results `1` and `7`. We just need to extend the `if` statement to include the new operations

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
            elif operator == '-':
                return lvalue - rvalue, rtype
            elif operator == '*':
                return lvalue * rvalue, rtype
            elif operator == '/':
                return lvalue // rvalue, rtype
```

---

Now we have a pretty simple but fully working calculator! Enjoy the `cli.py`, as YOU did it this time! I remember I was pretty excited the first time I run a command line calculator done by me. But hold tight, because you are going to learn and implement much more!

## Level 9 - Mixing operators

*"Don't cross the streams."* - Ghostbusters (1984)

Ok, it's time to do some serious math. What happens if you mix sums and multiplications? Let's try it and see how our interpreter reacts. We already know that the lexer happily digests all the four symbols so we can head straight to the parser and add the following test to `tests/test_calc_parser.py`

``` python
def test_parse_expression_with_term():
    p = cpar.CalcParser()
    p.lexer.load("2 + 3 * 4")

    node = p.parse_expression()

    assert node.asdict() == {
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
                'value': '*'
            }
        },
        'operator': {
            'type': 'literal',
            'value': '+'
        }
    }
```

Chances are that this will fail miserably. Probably you have to rework a bit `parse_expression` as it is ignoring the new entry, `parse_term`. Please note that `2 * 3 + 4` must give `10` according to the standard math rules, and not `14`. This happens because multiplication is performed before sum, and the order depends uniquely on the structure created by the parser, and not by the visitor (which is at this point a pretty dumb component).

Once the parser outputs the correct structure the visitor shouldn't have issues, as it is already behaving in a recursive way. If you want to check feel free to add relevant tests, however.

---

### Solution

Ouch! It looks like putting multiplications and sums in the same line is not really working. As you may recall we didn't link `parse_term` with the other methods, and we use a generic function to treat literals. This works in principle, but doesn't consider operator precedence.

When we try to evaluate `2 + 3 * 4` the output of the parser is

``` python
{
  "type": "binary",
  "left": {
    "type": "binary",
    "left": {
      "type": "integer",
      "value": 2
    },
    "right": {
      "type": "integer",
      "value": 3
    },
    "operator": {
      "type": "literal",
      "value": "+"
    }
  },
  "right": {
    "type": "integer",
    "value": 4
  },
  "operator": {
    "type": "literal",
    "value": "*"
  }
}
```

As you can clearly see the parser recognised the multiplication operator, but then returns a nested sum (the oputput of a recursive call of `parse_term`). This gives the sum a greater precedence that that of the sum, which is against the mathematical rules we want to follow here. `2 + 3 * 4` shall be considered `2 + (3 * 4)` and not `(2 + 3) * 4`.

To fix this we have to rework `parse_term`. First of all it shall accept only the `*` and `/` operators, then it shall return the left part if it finds a different literal. Even `parse_expression` shall change a bit: the first thing to do is to call `parse_term` instead of `parse_integer` and then to return the left part.

The new code is then

``` python
    def parse_term(self):
        left = self.parse_integer()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self.parse_addsymbol()
            right = self.parse_integer()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left

    def parse_expression(self):
        left = self.parse_term()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL:
            operator = self.parse_addsymbol()
            right = self.parse_term()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

Let's see what happens parsing `2 * 3 + 4`. The test calls `parse_expression` which tries immediately to run `parse_term`. The latter recognises `2` and `*`, so it calls itself recursively just before the `3` and returns the binary node. This means that the multiplication is the first operation we return, the one with higher precedence. The recursive call recognises `3` but then doesn't know what to do with `+` as we specifically consider only `*` and `/`, so it just returns the integer value. Back to `parse_expression`, then the variable `left` will contain the binary node that represents `2 * 3`. The function will then finish adding the binary node for the sum.

Take your time to understand the mechanism, perhaps trying with different operations like `2 + 4 * 6 - 8`, which should return `18`.

---

## Level 10 - Parentheses

*"When nine hundred years old you reach, look as good you will not."* - Return of the Jedi (1983)

[Parentheses](https://en.wikipedia.org/wiki/Bracket#Parentheses), are curved brackets used in mathematics to change the order of operations. As this part is pretty important I will spend some time on it, because the order of operations will be of concerns also when it comes to language operators, and not only when dealing with mathematical operations. As explained in the previous section almost everything at this point happens in the parser, as the resulting structure that we will give to the visitor is the one that rules the precedence of operations.

Let's start to check that the lexer understands the parentheses symbols `(` and `)`.

``` python
def test_get_tokens_understands_parentheses():
    l = clex.CalcLexer()

    l.load('3 * ( 5 + 7 )')

    assert l.get_tokens() == [
        token.Token(clex.INTEGER, '3'),
        token.Token(clex.LITERAL, '*'),
        token.Token(clex.LITERAL, '('),
        token.Token(clex.INTEGER, '5'),
        token.Token(clex.LITERAL, '+'),
        token.Token(clex.INTEGER, '7'),
        token.Token(clex.LITERAL, ')'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

As our lexer is pretty open-minded it shouldn't raise any objections and happily pass the test (why?). 

As always, instead, its neighbour the parser is not that forgiving, and I bet it will make a fuss. Let's try and feed it with some simple expression with parentheses

``` python
def test_parse_expression_with_parentheses():
    p = cpar.CalcParser()
    p.lexer.load("(2 + 3)")

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

To make this test pass my suggestion is to introduce a method `parse_factor`, where the term _factor_ encompasses both integers and the expressions between parentheses. In the latter case, obviously, you will need to call `parse_expression`, which somehow breaks the hierarchical structure of methods in the parser.

---

### Solution

Let's have some Lisp time here and introduce parentheses. As happened for the new mathematical operators, parentheses are already accepted by the lexer as simple literals, so the first test passes without any change in the code. The parser complains, however, as it always expects an integer (`smallcalc/calc_parser.py:76`).

As I suggested, my idea is to introduce a method that parses a so-called _factor_, which can either be an integer of an expression between parentheses.

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

    def parse_factor(self):
        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value == '(':
            self.lexer.get_token()
            expression = self.parse_expression()
            self.lexer.get_token()
            return expression

        return self.parse_integer()
```

The method `parse_term` now has to call `parse_factor`

``` python
    def parse_term(self):
        left = self.parse_factor()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['*', '/']:
            operator = self.parse_addsymbol()
            right = self.parse_integer()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

And last we need to slightly change `parse_expression` introducing a check on the literal token value. This happens because I decided to identify everything with a literal, so the method has to rule out every literal it is not interested to manage. If you introduce specific tokens for operations, parentheses, etc., this change is not required (but you won't use `clex.LITERAL` at that point).

``` python
    def parse_expression(self):
        left = self.parse_term()

        next_token = self.lexer.peek_token()

        while next_token.type == clex.LITERAL\
                and next_token.value in ['+', '-']:
            operator = self.parse_addsymbol()
            right = self.parse_term()

            left = BinaryNode(left, operator, right)

            next_token = self.lexer.peek_token()

        return left
```

---

## Level 11 - Priorities

*"You got issues, Quill."* - Guardians of the Galaxy (2014)

As parentheses have been introduced to change the default priority rules between operators we need to be sure that this happens. We can test it easily with this code

``` python
def test_parse_parentheses_change_priority():
    p = cpar.CalcParser()
    p.lexer.load("(2 + 3) * 4")

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
            'value': '*'
        }
    }
```

Now when your parser passes this test you have a full-fledged calculator that supports parentheses. Make sure to test the new features in the CLI. Does multiple parentheses work? Why?

---

### Solution

Another feature that comes for free with the previous changes, as the first thing that `parse_expression` does is to run `parse_term`, and the first thing the latter does is to run `parse_factor`, which in turn manages expressions between parentheses. If the expression is enclosed between parentheses the method `parse_factor` doesn't call `parse_expression` and just returns the integer.

---

## Level 12 - Unary operators

*"There can be only one!"* - Highlander (1986)

Now it's time to introduce unary operators, which are very important in programming languages. Just think at `not x` and you will immediately understand why you need them. Unary operators do not fit in the current structure of our interpreter as the parser is always expecting either an integer or an open parenthesis as the first token.

Let's first write a test for the most simple unary operator, which is a minus (as in `-2`). Remember that we are testing the parser here, as the lexer is already able to parse the minus sign.

``` python
def test_parse_factor_supports_unary_operator():
    p = cpar.CalcParser()
    p.lexer.load("-5")

    node = p.parse_factor()

    assert node.asdict() == {
        'type': 'unary',
        'operator': {
            'type': 'literal',
            'value': '-'
        },
        'content': {
            'type': 'integer',
            'value': 5
        }
    }
```

When your parser passes this test we have to make sure that the unary minus can be applied also to expressions between parentheses

``` python
def test_parse_factor_supports_negative_expressions():
    p = cpar.CalcParser()
    p.lexer.load("-(2 + 3)")

    node = p.parse_factor()

    assert node.asdict() == {
        'type': 'unary',
        'operator': {
            'type': 'literal',
            'value': '-'
        },
        'content': {
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
    }
```

Once the parser is able to pass these two tests we are confident that the unary minus can be used in front of all the basic elements of our expressions. At this point it is time to execute the unary expressions produced by the parsing layer, so include this test for the visitor

``` python
def test_visitor_unary_minus():
    ast = {
        'type': 'unary',
        'operator': {
            'type': 'literal',
            'value': '-'
        },
        'content': {
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
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (-5, 'integer')
```

Change the visitor to pass this test and you can go straight to the CLI and start using negative numbers or negative expressions. Can you execute something like `--2` (minus minus 2)? What is the result? Why?

Now let's go back to the parser and ensure that the unary plus can be used as well. This is the test

``` python
def test_parse_factor_supports_unary_plus():
    p = cpar.CalcParser()
    p.lexer.load("+(2 + 3)")

    node = p.parse_factor()

    assert node.asdict() == {
        'type': 'unary',
        'operator': {
            'type': 'literal',
            'value': '+'
        },
        'content': {
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
    }
```

and the code should be trivial, as you already manage the unary minus. The relative test for the visitor is

``` python
def test_visitor_unary_plus():
    ast = {
        'type': 'unary',
        'operator': {
            'type': 'literal',
            'value': '+'
        },
        'content': {
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
    }

    v = cvis.CalcVisitor()
    assert v.visit(ast) == (5, 'integer')
```

Once your code passes all the tests head to the CLI and try to run something like `-+--++-3`. Does it work?

---

### Solution

The minus unary operator uses a literal that we already manage in the lexer, so there is nothing to do there. The first test I gave you checks if the parser can process a factor in the form `-5`.

The current implementation of `parse_factor` processes either an expression enclosed between parentheses or an integer, and actually the test doesn't pass, complaining against the minus sign not being a valid integer with base 10. The solution is pretty straightforward, as it is enough to add another `if` that manages the minus sign. When we encounter such a sign, however, we have to return a different type of node, as the test states, so we also have to introduce the relative class.

``` python

class UnaryNode(Node):

    node_type = 'unary'

    def __init__(self, operator, content):
        self.operator = operator
        self.content = content

    def asdict(self):
        result = {
            'type': self.node_type,
            'operator': self.operator.asdict(),
            'content': self.content.asdict()
        }

        return result


class CalcParser:

    def __init__(self):
        self.lexer = clex.CalcLexer()

    def parse_addsymbol(self):
        t = self.lexer.get_token()
        return LiteralNode(t.value)

    def parse_integer(self):
        t = self.lexer.get_token()
        return IntegerNode(t.value)

    def parse_factor(self):
        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value == '-':
            operator = self.parse_addsymbol()
            factor = self.parse_factor()
            return UnaryNode(operator, factor)

        if next_token.type == clex.LITERAL and next_token.value == '(':
            self.lexer.get_token()
            expression = self.parse_expression()
            self.lexer.get_token()
            return expression

        return self.parse_integer()
```

The second test passes automatically because `parse_factor` intercepts the `-` literal before the `(` one.

The visitor has to be updated with the new type of `unary` node. The new visitor is then

``` python
class CalcVisitor:

    def visit(self, node):
        if node['type'] == 'integer':
            return node['value'], node['type']

        if node['type'] == 'unary':
            operator = node['operator']['value']
            cvalue, ctype = self.visit(node['content'])

            if operator == '-':
                return - cvalue, ctype

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
```

Now the unary plus is easy to sort out, as we just need to take it into account in `parse_factor` along with the unary minus.

``` python
    def parse_factor(self):
        next_token = self.lexer.peek_token()

        if next_token.type == clex.LITERAL and next_token.value in ['-', '+']:
            operator = self.parse_addsymbol()
            factor = self.parse_factor()
            return UnaryNode(operator, factor)

        if next_token.type == clex.LITERAL and next_token.value == '(':
            self.lexer.get_token()
            expression = self.parse_expression()
            self.lexer.get_token()
            return expression

        return self.parse_integer()
```

And the visitor is missing a single return after the `if` statement that deals with the unary minus.

``` python
        if node['type'] == 'unary':
            operator = node['operator']['value']
            cvalue, ctype = self.visit(node['content'])

            if operator == '-':
                return - cvalue, ctype

            return cvalue, ctype
```

---

## Final words

That's all for this post. If you feel brave or do not like to wait for the next post go and try adding new operators! Next time I will cover variables, assignments and postfix-operators like the power operation (`2^3`).

The code I developed in this post is available on the GitHub repository tagged with `part2` ([link](https://github.com/lgiordani/smallcalc/tree/part2)).

## Updates

2017-12-24: `test_parse_term_with_multiple_operations` has been changed after Victor Uriarte spotted an error in the tree construction. See the updates section of the first post in the series for a full explanation of the issue.

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/blog_source/issues) page is the best place to submit corrections.
