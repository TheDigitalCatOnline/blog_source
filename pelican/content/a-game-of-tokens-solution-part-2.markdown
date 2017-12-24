Title: A game of tokens: solution - Part 2
Date: 2017-10-17 13:00:00 +0100
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-solution-part-2
Series: A game of tokens
Image: a-game-of-tokens-solutions
Summary: 

Here we are, with my solution to the second part of "A Game of Tokens", aka how to write a simple interpreter in Python and survive to tell the tale. The second part of the challenge can be found [here](/blog/2017/10/01/a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-2/). Again, it is never repeated too many times, this is my personal solution. Your solution can be different, and if all the test pass it is correct!

You can find the code for this part in [this repository](https://github.com/lgiordani/smallcalc). The branch called `part2` contains all the commits explained in this post, and every commit contains both the test(s) and the code that makes the test(s) pass.

# Level 8 - Multiplication and division

## Lexer

The tests we added for the lexer already pass. This is not surprising, as the lexer is designed to return everything it doesn't know as a `LITERAL` (`smallcalc/calc_lexer.py:119`). As we already instructed the lexer to skip spaces the new operators are happily digested. I decided for this project not to assign operators a specific token, so from this point of view our lexer is pretty open and could already understand instructions like `3 $ 5` or `7 : 9`, even though they do not have a mathematical meaning.

## Parser

The parser is not so merciful, and the two new tests do not pass. We are explicitly calling a `parse_term()` method that is not defined, so a success would have been very worrying. In these two tests `parse_term()` is called explicitly and there is no relationship with the other `parse_*` methods, so we can implement it as a stand-alone processing.

We know that a `term` is an operation between two integers, so we can follow what we did with `parse_expression()`. The first thing we do is to parse the first integer, then we peek the next token and we decide what to do. If the token is a `LITERAL` we suppose it is the operation, otherwise we probably hit the end of the file and we will just return the previously read integer. The second element may be a simple integer or another multiplication or division, so we recursively call `parse_term()` and return a `BinaryNode` with the result.

[Note: I notice that the `parse_addsymbol()` could be now named `parse_literal()` but this wasn't done when I prepared the source code. Regardless of the name, however, what this method does is to just pack a literal in a `LiteralNode` and return it.]

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

## Visitor

The visitor was instructed to deal with sums and subtractions, and it treats everything is not the former as the latter. This is why the new tests give as results `1` and `7`. We just need to extend the `if` statement to include the new operations

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

Now we have a pretty simple but working calculator! Enjoy the `cli.py`, as YOU did it this time! I rememberI was pretty excited the first time I run a command line calculator done by me. But hold tight, because you are going to learn and implement much more!

# Level 9 - Mixing operators

Ouch! It looks like putting multiplications and sums in the same line is not really working. As you may recall we didn't link `parse_term()` with the other methods, and we use a generic function to treat literals. This works in principle, but doesn't consider operator precedence.

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

As you can clearly see the parser recognised the multiplication operator, but then returns a nested sum (the oputput of a recursive call of `parse_term()`). This gives the sum a greater precedence that that of the sum, which is against the mathematical rules we want to follow here. `2 + 3 * 4` shall be considered `2 + (3 + 4)` and not `(2 + 3) * 4`.

To fix this we have to rework `parse_term()`. First of all it shall accept only the `*` and `/` operators, then it shall return the left part if it finds a different literal. Even `parse_expression()` shall change a bit: the first thing to do is to call `parse_term()` instead of `parse_integer()` and then to return the left part.

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

Let's see what happens parsing `2 * 3 + 4`. The test calls `parse_expression()` which tries immediately to run `parse_term()`. This latter recognises `2` and `*`, so it calls itself recursively just before the `3` and returns the binary node. This means that the multiplication is the first operation we return, the one with higher precedence. The recursive call recognises `3` but then doesn't know what to do with `+` as we specifically consider only `*` and `/`, so it just returns the integer value. Back to `parse_expression()`, then the variable `left` will contain the binary node that represents `2 * 3`. The function will then finish adding the binary node for the sum.

Take your time to understand the mechanism, perhaps trying with different operations like `2 + 4 * 6 - 8`, which should return `18`.

# Level 10 - Parentheses

Let's have some Lisp time here and introduce parenthesis. As happened for the new mathematical operators, parenthesis are already accepted by the lexer as simple literals, so the first test passes without any change in the code. The parser complains, however, as it always expects an integer (`smallcalc/calc_parser.py:76`).

As I suggested in the post my idea is to introduce a method that parses a so-called _factor_, which can either be an integer of an expression between parenthesis.

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

Then `parse_term()` method now has to call `parse_factor()`

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

And last we need to slightly change `parse_expression()` introducing a check on the literal token value. This happens because I decided to identify everything with a literal, so the method has to rule out every literal it is not interested to manage. If you introduce specific tokens for operations, parenthesis, etc., this change is not required (but you won't use `clex.LITERAL` at that point).

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

# Level 11 - Priorities

Another feature that comes for free with the previous changes, as the first thing that `parse_expression()` does is to run `parse_term()`, and the first thing the latter does is to run `parse_factor()`, which in turn manages expressions between parenthesis. If the expression is enclosed between parenthesis the `parse_factor()` method doesn't call `parse_expression()` and just returns the integer.

# Level 12 - Unary operators

The minus unary operator uses a literal that we already manage in the lexer, so there is nothing to do there. The first test I gave you checks if the parser can process a factor in the form `-5`.

The current implementation of `parse_factor()` processes either an expression enclosed between parenthesis or an integer, and actually the test doesn't pass, complaining against the minus sign not being a valid integer with base 10. The solution is pretty straightforward, as it is enough to add another `if` that manages the minus sign. When we encounter such a sign, however, wwe have to return a different type of node, as the test states, so we also have to introduce the relative class.

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

The second test passes automatically because `parse_factor()` intercepts the `-` literal before the `(` one.

The visitor, then, has to be updated with the new type of `unary` node. The new visitor is then

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

Now the unary plus is easy to sort out, as we just need to take it into account in `parse_factor()` along with the unary minus.

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

# Final words

Well, we have a pretty decent calculator now, don't we? Stay tuned, as in the next instalment we will explore variables and postfix operators. Get in touch if you want to discuss your solution or if you have questions about the code I posted here.

# Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/111444750762335924049) to comment the post. Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
