Title: A game of tokens: write an interpreter in Python with TDD - Part 2
Date: 2017-10-01 15:00:00 +0100
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2
Series: A game of tokens
Image: a-game-of-tokens
Summary:

My solution to the [first part]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1.markdown) can be found [here](https://github.com/lgiordani/smallcalc/tree/part1), and a discussion about the code has been published [here]({filename}a-game-of-tokens-solution-part-1.markdown) Remember that this is only one possible solution, and that giving you the tests I already biased the whole project towards my implementation of choice, so feel free to experiment on your own!

In this instalment I'll give you new tests that will guide you through the implementation of a decent calculator, with multiplication, division, parenthesis, and unary operators. I will reference the structure I used in my solution, but you mileage may vary, so feel free to ignore the comments or the suggested solutions in case your code is different.

# Level 8 - Multiplication and division

*"They're coming outta the walls. They're coming outta the goddamn walls."*

As you remember from the previous post our interpreter is made of three different components, the lexer, the parser, and the visitor. So, to implement the missing basic operations, multiplication and division, we need to start with the lexer and ensure that it understands the traditional symbols `*` and `/`

## Lexer

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

Do the tests fail? Why? Please remember that when tests pass without requiring any code change you have to ask yourself "Why do they pass?", and be sure that you understood the answer before going further.

## Parser

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

This time you should have some failures, so go and edit the `CalcParser` class in order to pass the tests. As the two new binary operations are at this level the same as sum and difference you _could_ change the `parse_expression()` method (try it!). This will however make things harder later when we will prioritise operations (multiplications have to be performed before sums), so my advice is to introduce a `parse_term()` method in the parser.

## Visitor

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

# Level 9 - Mixing operators

*"Don't cross the streams."*

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

Chances are that this will fail miserably. Probably you have to rework a bit `parse_expression()` as it is ignoring the new entry, `parse_term()`. Please note that `2 * 3 + 4` must give `10` according to the standard math rules, and not `14`. This happens because multiplication is performed before sum, and the order depends uniquely on the structure created by the parser, and not by the visitor (which is at this point a pretty dumb component).

Once the parser outputs the correct structure the visitor shouldn't have issues, as it is already behaving in a recursive way. If you want to check feel free to add relevant tests, however.

# Level 10 - Parentheses

*"When nine hundred years old you reach, look as good you will not."*

[Parentheses](https://en.wikipedia.org/wiki/Bracket#Parentheses)) are curved brackets used in mathematics to change the order of operations. As this part is pretty important I will spend some time on it, because the order of operations will be of concerns also when it comes to language operators, and not only when dealing with mathematical operations. As explained in the previous section almost everything at this point happens in the parser, as the resulting structure that we will give to the visitor is the one that rules the precedence of operations.

Let's start to check that the lexer understands the parentheses

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

The parser is not that forgiving, however, and I bet it will make a fuss. Let's try and feed it with some simple expression with parentheses

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

To make this test pass my suggestion is to introduce a `parse_factor()` method, where the term _factor_ encompasses both the integers and the expressions between parentheses. In the latter case obviously, you will need to call `parse_expression()`, which somehow breaks the hierarchical structure of methods in the parser.

# Level 11 - Priorities

*"You got issues, Quill."*

As parentheses have been introduced to change the default priority rules between operators we need to be sure that this happens. We can test is very simply with this code

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

# Level 12 - Unary operators

*"There can be only one!"*

Now it's time to introduce unary operators, which are very important in programming languages. Just think at `not x` and you will immediately understand why you need them. Unary operators do not fit in the current structure of our interpreter as the parser is always expecting either an integer or a parentheses as the first token.

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

# Conclusion

That's all for this time. If you feel brave or do not like to wait for the next post go and try adding new operators! Next issue will cover variables, assignments and postfix-operators like the power operation (`2^3`). I will also publish my solution to the tests presented here before moving on.

# Titles

The section quotes come from some good films: "Aliens" (1986), "Ghostbusters" (1984), "Return of the Jedi" (1983), "Guardians of the Galaxy" (2014), "Highlander" (1986).

# Updates

2017-12-24: `test_parse_term_with_multiple_operations` has been changed after Victor Uriarte spotted an error in the tree construction. See the updates section of the first post in the series for a full explanation of the issue.

# Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
