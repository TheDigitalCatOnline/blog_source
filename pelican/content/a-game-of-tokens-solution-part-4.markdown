Title: A game of tokens: solution - Part 4
Date: 2018-06-02 13:30:00 +0000
Category: Programming
Tags: Python, Python3, TDD, testing, compilers
Authors: Leonardo Giordani
Slug: a-game-of-tokens-solution-part-4
Series: A game of tokens
Image: a-game-of-tokens-solutions
Summary: Solutions to the 4th part of the series on how to write a language in Python with TDD

This is my solution to the fourth part of "A Game of Tokens", which can be found [here]({filename}a-game-of-tokens-write-an-interpreter-in-python-with-tdd-part-4.markdown).

You can find the tests and the code presented here in [this repository](https://github.com/lgiordani/smallcalc) in the branch called `part4`.

# Level 15 - Exponentiation

## Lexer

The lexer can process the exponentiation operator `^` out of the box as a `LITERAL` token, so no changes to the code are needed.

## Parser

The test `test_parse_exponentiation()` can be passed adding a `PowerNode` class

``` python
class PowerNode(BinaryNode):
    node_type = 'exponentiation'
```

and a `parse_exponentiation()` method to the parser

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

This allows the parser to explicitly parse the exponentiation operation, but when the operation is mixed with others the parser doesn't know how to deal with it, as `parse_exponentiation()` is not called by any other method.

To pass the `test_parse_exponentiation_with_other_operators()` test we need to change the `parse_term()` method and call `parse_exponentiation()` instead of `parse_factor()`

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

## Visitor

The given test `test_visitor_exponentiation()` requires the `CalcVisitor` to parse nodes of type `exponentiation`. The code required to do this is

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

# Level 16 - Float numbers

## Lexer

The first thing the lexer need is a label to identify `FLOAT` tokens

``` python
FLOAT = 'FLOAT'
```

then the method `_process_integer()` cna be extended to process float numbers as well. To do this the method is renamed to `_process_number()`, the regular expression is modified, and the `token_type` is managed according to the presence of the dot.

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

Remember that the `get_token()` function has to be modified to use the new name of the method. The new code is

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

## Parser

First we need to add a new type of node

``` python
class FloatNode(ValueNode):
    node_type = 'float'
```

The new version of `parse_integer()`, renamed `parse_number()`, shall deal with both cases but also raise the `TokenError` exception if the parsing fails

``` python
    def parse_number(self):
        t = self.lexer.get_token()

        if t.type == clex.INTEGER:
            return IntegerNode(int(t.value))
        elif t.type == clex.FLOAT:
            return FloatNode(float(t.value))

        raise clex.TokenError
```

## Visitor

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

just before evaluating the operator in the binary nodes. The full code of the `visit()` method is then

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

# Final words

I bet at this point of the challenge the addition of exponentiation and float numbers wasn't that hard. The refactoring might have been a bit thougher, however, but I hope that it showed you the real power of TDD.

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.
