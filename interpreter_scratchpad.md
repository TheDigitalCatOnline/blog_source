# Operators precedence

``` txt
integer: [0-9]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
term: integer (mulsymbol term)
expression: integer (addsymbol expression)
```

It's now time to expand the set of supported operators to multiplication and division. A new problem arises, however, namely that those operators do not have the same precedence as sum and subtraction. This means that you cannot execute sums, subtractions, multiplication, and divisions in random order and obtain the same result. A quick example comes from the expression `2 + 3 * 4`, where the result is `20` if you execute first the sum and then the multiplication, but becomes `14` if you swap the order in which you perform the operations.

This is not only a problem of mathematical operators, because languages in general have the problem of precedence. Even natural languages, where we adopt punctuation to signal precedence in ambiguous cases. One of the most famous examples in the world of compilers is that of the dangling else, where an expression like `if a then if b then c else d` is ambiguous; without knowing the precedence rules the `else d` part could equally stand as the optional closing statement of either the first or the second `if` clause.

We can start first implementing support for multiplication and division in our language, and then sort out the problem of precedence. Multiplication and divisions, when considered separately from other operators, work exactly like sum and subtraction, so we can include two tests that are copied from the previous ones (but with different names and operators). The tests for the lexer are

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

Keep in mind that I am adding multiple tests in one shot just because we are dealing with a situation that is very similar to a previous one, requiring a trivial copy of some code and the renaming of some methods and variables.

The tests for the parser are

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

As you can see, apart from the name of the method, the two tests are exactly like the previous ones for sum and subtraction. The implementation of the `parse_term()` method, consequently, will not differ from the one of `parse_expression()`.

The visitor as before is the place where we have to actually implement the mathematical operations represented by the symbols

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

Please note that I decided to give the symbol `/` the meaning of "integer division", that is a division that returns not the actual result, but the nearest integer (TODO rounded?). In Python 3 the `/` operator performs float division, while integer division is represented by `//`. We still do not know how to deal with operators made by multiple literals, so for now we will stick to the integer division, as it requires a non-trivial implementation of the visitor (hint: nothing complex, but you cannot just use Python's `/` operator).

The `cli.py` file, however, contains a call to `parse_expression()` and not to `parse_term()`, so it shouldn't be possible to use them now. In my implementation, however, I did not include any check on the literal in `parse_expression()`, so what happens is that the CLI can already deal with multiplications and divisions.

``` sh
smallcalc :> 3 * 4
(12, 'integer')
smallcalc :> 5 / 2
(2, 'integer')
smallcalc :> 3 * 9 / 4
(6, 'integer')
```

Trying to mix the two, however, leads to wrong results

``` sh
smallcalc :> 3 * 5 + 4
(27, 'integer')
```

The problem here is that `parse_expression()` interpreted the command as the multiplication between `3` and `5 + 4`, since the `*` symbol comes first. This is wrong, as we said, according to the standard precedence rules of mathematics.

How can we make the CLI understand the correct precedence, and how can we make use of `parse_term()`? This is the problem addressed by precedence rules.

# Precedence rules

``` txt
integer: [0-9]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
term: integer (mulsymbol term)
# An expression is now a term followed by an optional addsymbol and expression
expression: term (addsymbol expression)
```

As you see from the grammar the precedence (in this type of parser and language) is expressed simply by correctly chaining the rules. In the new grammar an expression is not built of integers, but of terms. This means that the parser will try to identify all the possible terms before moving on to process an addsymbol.

I believe it is worth giving an example of this concept, as it is of paramount importance to understand how the parser works.

Consider the string `3 * 4 + 5 * 6`. According to the standard precedence rules of math this is a sum between two terms, `3 * 4` and `5 * 6`. The result is thus `12 + 30 = 42`.

``` txt
expression -> term (addsymbol expression)
expression -> integer (mulsymbol term) (addsymbol expression)
expression -> 3 (mulsymbol term) (addsymbol expression)
expression -> 3 mulsymbol integer (mulsymbol term) (addsymbol expression)
expression -> 3 * 4 (mulsymbol term) (addsymbol expression)
```

There is no mulsymbol after the number `4` so the optional part is removed

```
expression -> 3 * 4 (addsymbol expression)
expression -> 3 * 4 addsymbol term (addsymbol expression)
expression -> 3 * 4 + term (addsymbol expression)
expression -> 3 * 4 + integer (mulsymbol term) (addsymbol expression)
expression -> 3 * 4 + 5 (mulsymbol term) (addsymbol expression)
expression -> 3 * 4 + 5 mulsymbol integer (mulsymbol term) (addsymbol expression)
expression -> 3 * 4 + 5 * 6 (mulsymbol term) (addsymbol expression)
```

There is no mulsymbol after the number `5` so the optional part is removed, and the same happens for the addsymbol and the following expression.

Your code should try to follow this behaviour, and with `peek_node()` you can now look ahead and decide if the upcoming tokens are part of an expression, a term, or just simple integers at the end of the line.

The test that we want to satisfy now is the following

``` python
def test_parse_expression_with_term():
    p = cpar.CalcParser()
    p.lexer.load("2 * 3 + 4")

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

# Changing the precedence

``` txt
integer: [0-9]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
# A factor can be an expression, strictly enclosed between parentheses, or an integer
factor: '(' expression ')' | integer
# A term starts with a factor
term: factor (mulsymbol term)
expression: term (addsymbol expression)
```

Congratulations if you succeeded in making every test pass! You achieved a great result, which is to write a parser for simple mathematical expressions with an implicit precedence rule. Your programming language can grow and become more and more complex, but the concepts that you learned following the project up to this point will always be useful.

The next challenge is to implement a system to override the implicit precedence rule that we put in the language. Currently, multiplication and division have a higher precedence than addition and subtraction if they are chained without any other specification. Sometimes, however, we want to change the order in which operations are performed, and classical mathematics TODO does it with the help of parentheses.

If we write `2 * (3 + 4)` we clearly signal that we want to perform `3 + 4` first, and then multiply the result by `2`. The plain (implicit) form `2 * 3 + 4`, indeed, corresponds to the explicit form `(2 * 3) + 4`.

So we want to add support for parentheses in our language. Let us first check that the lexer can handle the open and closed TODO parentheses symbols

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

As you can see from the updated grammar, now a term starts with a factor and no more with an integer. The factor, in turn, can be an integer or an expression, but with the strict requirement that the expression shall be enclosed between parentheses.

Take your time to understand the change and verify that this set of rules can actually allow to change the precedence in our language. Can we process `2 * (3 + 4 )`? And what about `2 + ((2 + 4) * 4)`?

Once you grasped the effect of that small change in the grammar you can move to the next test. The first thing we want to check is if an expression enclosed between parentheses can be parsed by `parse_expression()`.

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

And when you have the code that satisfies this test you want to finally check that the precedence of the operators is changed by the parentheses

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

Again, take your time to review what we did with the rules and why the whole system works. As a final check we can try to process some expressions in the command line

``` sh
smallcalc :> (2)
(2, 'integer')
smallcalc :> (2 + 4)
(6, 'integer')
smallcalc :> ((((2))))
(2, 'integer')
smallcalc :> (2+4)*3
(18, 'integer')
smallcalc :> 
```

# Unary operators

``` txt
integer: [0-9]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
factor: '(' expression ')' | '-' factor | integer
term: factor (mulsymbol term)
expression: term (addsymbol expression)
```

As we found out previously, our language can compute negative numbers out of the box, thanks to the help of the underlying Python integers. We cannot however use negative numbers in our input, so trying to process an expression like `-2 + 3` results in an error. Things don't go better if we try with explicitly positive numbers, like `+2 + 3`.

We shall introduce, then, unary operators, that is symbols that affect only one of the near numbers (the one on its right). We obviously do not need to change the lexer, as we already know that the minus symbol is recognised. The parser then, shall pass this test

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

As you can see from the grammar the change is happening in the factor, so `parse_factor()` is the method that we have to test. The idea is the same we implemented for binary operators, the only difference is that here we have a single `content` node instead of a `left` and `right` ones.

The grammar however states that the unary minus can be applied to factors, that include expressions enclosed between parentheses. To check if your code is correct, then, make sure that this test passes

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

As soon as your parser supports unary minus move to the visitor and make it satisfy the following test

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

Now adding support for unary plus should be a breeze. The test for the parser is

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

And the one for the visitor is

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

# Variables

``` txt
integer: [0-9]+
# A variable is a  sequence of one or more lowercase letters, uppercase letters, and underscores
variable: [a-zA-Z_]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
factor: '(' expression ')' | '-' factor | variable | integer
term: factor (mulsymbol term)
expression: term (addsymbol expression)
```

Variables are names that can be attached to a value, so the first thing we have to implement is the support for non-numerical characters in the lexer

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

Then we want the lexer to accept also uppercase letters and underscores in variable names. Uppercase variables are tested by this

``` python
def test_get_tokens_understands_uppercase_letters():
    l = clex.CalcLexer()

    l.load('SomeVar')

    assert l.get_tokens() == [
        token.Token(clex.NAME, 'SomeVar'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

while underscores can be tested by this

``` python
def test_get_tokens_understands_names_with_underscores():
    l = clex.CalcLexer()

    l.load('some_var')

    assert l.get_tokens() == [
        token.Token(clex.NAME, 'some_var'),
        token.Token(clex.EOL),
        token.Token(clex.EOF)
    ]
```

After these changes we have to actually provide support for variables in the parser. To do this we have first to add the concept of variable as one of the building blocks, side to side TODO with integers. Secondly TODO we need to be able to parse an assignment syntax and to actually store the value somewhere in memory.

The first test for the parser, thus is the following

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


integer: [0-9]+
variable: [a-zA-Z_]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
factor: '(' expression ')' | '-' factor | variable | integer
term: factor (mulsymbol term)
expression: term (addsymbol expression)
assignment: variable '=' expression

An assignment is simply the name of a variable followed by an expression, and its effect is to assign the result of the expression to the variable in memory. So the test we want to add to the suite is the following

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

Remember that the assignment doesn't actually create anything in the parsing stage. it gets recognised and put in the tree, but the visitor will be the one in charge of actually managing the variables table.

We should ensure that the parser correctly manages the assignment of an expression to a variable, so let's add a test to check this behaviour

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
```

It's time to make the visitor process an assignment and actually store the variable in memory. To store variables you can use a plain dictionary, as this will automatically provide you the feature to overwrite an existing variable in case of double assignment.

The first test for the visitor is

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

As you can see the visitor shall provide the `isvariable()`, `valueof()` and `typeof()` methods. Note that the result of an assignment operation is `(None, None)`.

The following test checks that the visitor can actually read a variable from the table in memory.

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

We need to parse a tree containing an assignment first, so that the variable is in memory. We do not yet included support for multiple statement, so we do not have a mean to put together the two trees. This is something that we will have to implement if we want our language to be able to run programs and not only commands given through the CLI.

You may have noticed that now our grammar contains two endpoints, that is rules that are not part of another rule. These are `expression` and `assignment`, and this grammar trait reflects in the code, where we have two methods, `parse_expression()` and `parse_assignment()`, but we have no means to decide which to use. Indeed, in the `cli.py` we have to opt for either the first or the second.

It would be a good idea to implement a rule that can contain all the endpoints. The new set of rules is then

integer: [0-9]+
variable: [a-zA-Z_]+
addsymbol: '+' | '-'
mulsymbol: '*' | '/'
factor: '(' expression ')' | '-' factor | variable | integer
term: factor (mulsymbol term)
expression: term (addsymbol expression)
assignment: variable '=' expression
line: expression | assignment

The first test we have to add to the parser checks that `parse_line()` can understand expressions

``` python
def test_parse_line_supports_expression():
    p = cpar.CalcParser()
    p.lexer.load("2 * 3 + 4")

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
            'value': '+'
        }
    }
```

and the second one that it supports assignments

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

While the first one is trivial to achieve (it is sufficient to make `parse_line()` return the result of `parse_expression()`), the second one will be tricky. TODO. You should try to make `parse_assignment()` or `parse_expression()` return an error or raise an exception if they cannot correctly parse the input and in that case call the other function.

Now make `parse_line()` the entrypoint of your CLI and enjoy the result

``` sh
smallcalc :> a = 5
(None, None)
smallcalc :> a * 3 + 2
(17, 'integer')
smallcalc :> b = 2
(None, None)
smallcalc :> a * b / 2
(5, 'integer')
smallcalc :> c = a
(None, None)
smallcalc :> c
(5, 'integer')
smallcalc :> c + 3
(8, 'integer')
```



