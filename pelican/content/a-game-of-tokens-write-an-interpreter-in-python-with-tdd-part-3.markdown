Title: A game of tokens: write an interpreter in Python with TDD - Part 3
Date: 2017-10-31 11:00:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-3
Series: A game of tokens
Image: a-game-of-tokens
Summary:

This is the third instalment of a series of posts on how to write an interpreter in Python. In the [first part](/blog/2017/05/09/a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-1) we developed together a small command line calculator that could sum and subtract numbers, while in the [second part](/blog/2017/10/01/a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2) we went further adding multiplication, division and unary plus and minus. Both parts give only the tests your code is supposed to pass, and my personal solution can be found [here](/blog/2017/07/12/a-game-of-tokens-solution-part-1) for the first part, and [here](/blog/2017/10/17/a-game-of-tokens-solution-part-2) for the second.

In this third part we wil start adding variables to our calculator, moving towards a real programming language.

# Level 13 - Variables

*I have been assigned by my strength and cunning.*

Variables are labels assigned to values, so what we need to add is a way for the user to make this assignment and then to use variables intead of actual values. The simplest syntax, used by many languages is `name = value` and we will stick to this. Usually languages allow only a subset of symbols in the name of a variable and we will learn how to use lower- and uppercase names that may also contain an underscore.

## Lexer

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

## Parser

To support variables in expressions we need to change the behaviour of `parse_factor()`, which is the method where we parse the building blocks like integers of unary operators. The test you need to add to `tests/test_calc_parser.py` is

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

This test tries to assign the value `5` to the variable `x`, but in general we want to support assignment with expressions, so we should test this behaviour as well

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

## Visitor

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

We want the visitor to provide three new methods, `isvariable()`, `valueof()`, and `typeof()`, that allow us to interact with the variables we defined.

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

# Level 14 - Parsing expressions and assignments

*Speak words we can all understand!*

We are missing a final step. The CLI uses `parse_expression()` as its default entry point, which means that it doesn't understand variable assignments for the time being. We need then to introduce a new entry point `parse_line()` that we will use to process general language statements. The test for this goes in `tests/test_calc_parser.py`

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

and checks that `parse_line()` can parse expressions (which can be solved just wrapping `parse_expression()` with it). The second test checks that `parse_line()` can parse variable assignments and goes in the same file

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

At this point we can change the entry point in the CLI, using `parse_line()` instead of `parse_expression()`. The new CLI is then

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

# Conclusion

Managing variables may look like a very easy task, but as soon as we will start implementing functions and local scopes we will have to move to something richer than a simple global dictionary. Memory management is another big topic that I didn't touch here, perhaps in the future I might discuss garbage collections and related problems.
In the next issue I will face with you the task of adding the power operator, support for floating point numbers, and a big refactoring with context managers that will greatly simplify the code.

# Titles

The section quotes come from some good films: "Up" (2009), "The Lord of the Rings: The Fellowship of the Ring" (2001).

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.

