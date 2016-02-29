Title: A simple example of Python OOP development (with TDD) - Part 2 
Date: 2015-09-10 20:00:00 +0100
Category: Programming
Tags: OOP, Python, Python3, TDD
Authors: Leonardo Giordani
Slug: python-oop-tdd-example-part2
Series: "Python OOP and TDD"
Summary: The second part of the post about Object-oriented Programming and Test Driven Development

In [the first part](/blog/2015/05/13/python-oop-tdd-example-part1/) of this small series I introduced you to TDD with Python by means of the powerful `py.test` package. We developed together a simple library which provides a `Binary` class that is a bit more useful than the default binary representation that Python provides with the `bin()` builtin function.

In this part I'll go on with the development of the library, discussing the implementation of a binary number with a fixed size, which is a very interesting and useful matter, being the foundation of the computer we are working with. Fixed-size binaries may also represent negative numbers with the two's complement technique, and this will be an important point to test.

You may happen to dislike some decisions about the interface or the behaviour of the resulting class. Since this post is just a way to show you a concrete TDD session you are totally free to change the tests and to come up with a better solution than mine. Feel free to get in touch and submit better ideas, I'm always looking for something new to learn.

As already suggested in the first installment try to follow the post up to the section where I write the tests. Then move on implementing your own class and try to make it pass all the tests: this is the best way for you to learn TDD, actually applying it.

## Fixed-size binaries

As soon as you build an electronic circuic to store information (a flip-flop, for example) you start dealing with binary numbers and with fixed-size quantities. Limiting the number of digits brings immediately the limitation of having a maximum number that can be represented and requires to decide what to do with bigger numbers. Another issue that arises is that of the representation of negative numbers. Since we can only use two symbols (one and zero) we have to decide a "syntax" for negative values.

You will find a lot of information about some of those issues in the following Wikipedia articles: [Integer overflow](https://en.wikipedia.org/wiki/Integer_overflow) and [signed number representations](https://en.wikipedia.org/wiki/Signed_number_representations). Check also this page on [bitwise operations](https://en.wikipedia.org/wiki/Bitwise_operation) as some of them will be implemented.

## Object interface

So we aim to create a class called `SizeBinary` that provides the following interface:

* Can be instantiated giving a size in bits and a value. The value is optional, if not specified is 0.
* The value can be set after instantiation with a `set()` method.
* If instantiated or set with a value greater than the maximum representable one the `overflow` attribute of the object becomes `True`.
* May initialized with all the data types supported by the `Binary` class developed in the first post.
* Just like the `Binary` class may be converted into an integer (e.g. `42`, a binary string (e.g. `0b101010`, an hexadecimal string (`0x2a`) and a bit string (`101010`). May also be converted to a `Binary` without size limitations.
* Support binary logic operations such as: NOT, AND, OR, XOR, shift left and right (without carry).
* Can be splitted in two arbitrary sized `SizeBinary` objects.
* Can be negated with both one's complement and two's complement techniques.

So now, obeying the TDD methodology, I will write some tests that exploit these features. After this I'm going to develop an object that makes all test pass. Like we did in the previous post you may follow along as I write te tests and then write your own class.

### Initialization

The `SizeBinary` object shall support all the initialization options supported by `Binary`, but its main interface is different because when creating a `SizeBinary` we must be allowed to specify the size. The first tests are though pretty straightforward.

``` python
def test_size_binary_init_int():
    size_binary = SizeBinary(8, 6)
    assert int(size_binary) == 6

def test_size_binary_init_int_overflow():
    size_binary = SizeBinary(8, 512)
    assert int(size_binary) == 0
    assert size_binary.overflow == True

def test_size_binary_set():
    size_binary = SizeBinary(8, 0)
    size_binary.set(22)
    assert str(size_binary) == '00010110'
    assert size_binary.overflow == False

def test_size_binary_set_overflow():
    size_binary = SizeBinary(8, 0)
    size_binary.set(512)
    assert str(size_binary) == '00000000'
    assert size_binary.overflow == True
```

I will cover all the cases already covered for the `Binary` class. As you can see, I'm specifying the bit size when instantiating the object and testing the overflow condition. Other initialization and conversion tests are very similar to their `Binary` counterpart and I will not copy them here, as you can find them in the source code.

## Splitting

One of the requirements is to provide a method to split a `SizeBinary` object into two arbitrarily sized `Binary` objects. The tests are

``` python
def test_size_binary_split():
    size_binary8 = SizeBinary(8, '01010110')
    size_binary4u, size_binary4l = size_binary8.split(4, 4)
    assert (size_binary4u, size_binary4l) == (SizeBinary(4, '0101'), SizeBinary(4, '0110'))

def test_size_binary_split_asymmetric():
    size_binary8 = SizeBinary(8, '01010110')
    size_binary9u, size_binary3l = size_binary8.split(9, 3)
    assert (size_binary9u, size_binary3l) == (SizeBinary(9, '000001010'), SizeBinary(3, '110'))
```

As you can see the split shall be able to pad the resulting values if the number of bits exceedes that of the available ones.

## Negative numbers

There are many techniques to represent negative numbers with binary digits, and there is no way to tell from a binary number neither if it is positive or negative nor which technique has been used. It is a matter of conventions into the system in use. We want to implement [one's complement](https://en.wikipedia.org/wiki/Ones'_complement) and [two's complement](https://en.wikipedia.org/wiki/Two's_complement), which are described in detail in the linked Wikipedia articles. The tests to check the correct behaviour are

``` python
def test_size_binary_OC():
    # 6 = 0b00000110 -> 0b11111001
    size_binary = SizeBinary(8, 6)
    assert size_binary.oc() == SizeBinary(8, '11111001')

    # 7 = 0b00000111 -> 0b11111000
    size_binary = SizeBinary(8, 7)
    assert size_binary.oc() == SizeBinary(8, '11111000')
    
    # 15 = 0b00001111 -> 0b11110000
    size_binary = SizeBinary(8, 15)
    assert size_binary.oc() == SizeBinary(8, '11110000')

    # 15 = 0b0000000000001111 -> 0b1111111111110000
    size_binary = SizeBinary(16, 15)
    assert size_binary.oc() == SizeBinary(16, '1111111111110000')

def test_size_binary_TC():
    # 6 = 0b00000110 -> 0b11111010
    size_binary = SizeBinary(8, 6)
    assert size_binary.tc() == SizeBinary(8, '11111010')

    # 7 = 0b00000111 -> 0b11111001
    size_binary = SizeBinary(8, 7)
    assert size_binary.tc() == SizeBinary(8, '11111001')

    # 15 = 0b00001111 -> 0b11110001
    size_binary = SizeBinary(8, 15)
    assert size_binary.tc() == SizeBinary(8, '11110001')

    # 15 = 0b0000000000001111 -> 0b1111111111110001
    size_binary = SizeBinary(16, 15)
    assert size_binary.tc() == SizeBinary(16, '1111111111110001')
```

## Mathematics, indexing and slicing

The basic mathematical and logical operations are the same implemented for the `Binary` class. Some tests have been added to check what happens when performing operations between two `SizeBinary` with a different size. We expect the result to have the size of the bigger of the two operands.

``` python
def test_binary_addition_int():
    assert SizeBinary(8, 4) + 1 == SizeBinary(8, 5)

def test_binary_addition_binary():
    assert SizeBinary(8, 4) + SizeBinary(8, 5) == SizeBinary(8, 9)

def test_binary_addition_binary_different_size():
    assert SizeBinary(8, 4) + SizeBinary(16, 5) == SizeBinary(16, 9)
```

Being very straightforward, I do not copy here all the tests written to cover this part, you will find them in the source code.

Even the shift left operation now drops bits if there is not enough space for them, while the `Binary` class did it only for the shift right operation. For simplicity's sake I didn't implement a carry flag, i.e. there is no way to retrieve the bits shifted outside the available space. You are free to try and implement it, it's a good exercise but remember to write tests first!

The indexing and slicing operations are basically the same as in the `Binary` case. The slicing operation produces a new `SizeBinary` with the correct size.

## Implementation time!

Now be a good OOP programmer and go, write a class that passes all the tests you find in the [source code](/code/python-oop-tdd-part2/test_size_binary.py). Just an advice, before you start headlong: being a `SizeBinary` mostly a `Binary` object with some new features it is recommended to use inheritance as the key delegation technique.

## My solution

Following my own advice my `SizeBinary` class inherits from `Binary`

``` python
from binary import Binary

class SizeBinary(Binary):
    pass
```

and with this simple declaration I get 1 test passed and still 50 to go. We obviously may also create a new object that does not inherit from `Binary` but we would have to explicitly delegate a lot of functions to this latter class. So, in this case, better to stick to an automatic delegation mechanism like inheritance. To get a review of those two concepts read [this post](/blog/2014/08/20/python-3-oop-part-3-delegation-composition-and-inheritance).

Composition could be another viable solution, with a `Binary` value stored internally and accessed whenever we call `super()` in the inheritance version. In this case, however, inheritance and composition lead to very similar results, with the latter being somehow counterintuitive and thus not the best choice.

We need to reimplement many of the special methods already implemented in the `Binary` class. This because Python resolves magic methods through a dedicated channel that avoids the `__getattr__()` and `__getattribute__()` methods, making the whole thing faster. This makes impossible to automatically delegate magic methods, except by means of metaclasses, which are however too complex to be a useful addition to this post.

The initialization function shall store the bit length and initialize a flag to signal the overflow condition. Since I also want to have a `set()` function that changes the value I implement it and call it in the `__init__()` method.

``` python
    def __init__(self, bits, value):
        self.bits = bits
        self.overflow = False

        super().__init__(0)
        
        self.set(value)

    def set(self, value):
        binary = Binary(value)
        upper, lower = binary.split(self.bits)

        if upper != 0:
            self.overflow = True

        self._value = Binary(lower)._value
```

I'm not that happy to poke into the `Binary` class implementation setting the `_value` attribute, but this is the only way to change the value of the underlying `Binary` class. If the `Binary` class had a `set()` method we could call it through `super()`, and I cannot directly set it through `__init__()` because I need to check the overflow condition.

With this code I get a surprising result of 37 passed tests, while 14 still refuse to let me call it a day. This is however misleading, as many tests assume the `SizeBinary` object correctly performs comparision, which is not the case, as shown by the failure of the `test_binary_equality_checks_bits` test.

This was done on purpose, to show you that writing tests is not something that automatically guarantees you to have correct code. As a matter of fact, in this case tests like

``` python
def test_binary_addition_int():
    assert SizeBinary(8, 4) + 1 == SizeBinary(8, 5)
```

do not fail, even if the result of the addition is a `Binary` and not a `SizeBinary`.

Let us check why this happens. The `SizeBinary` object is also a `Binary` object, and its `__add__()` method returns a `Binary`. The subsequent comparison is made using the `__eq__()` method of the `Binary` class, since the `SizeBinary` one does not provide its own version of the comparison. Since `Binary` classes just compare their value the test ends successfully.

Let us add some code to implement the correct comparision

``` python
    def __eq__(self, other):
        return super().__eq__(other) and self.bits == other.bits
```

which adds a check on the number of bits to the check already made by the `Binary` class. This results in 35 failed tests and 16 passed, showing that the previous result was biased by "inaccurate" tests. As a matter of fact, the presence of a test that shows if the comparison is correct or not makes perfectly reasonable to have tests that take it for granted.

One simple thing to add is the right padding with zeros that shall be provided for input values that are shorter than the given number of bits

``` python
    def __str__(self):
        s = super().__str__()
        return s.rjust(self.bits, '0')
```

And this simple addition lowers the count of failed tests from 35 to 30. Another small addition that satisfies 2 more tests is the splitting function, which shall return `SizeBinary` objects instead of `Binary` ones

``` python
    def split(self, upper_bits, lower_bits):
        upper, lower = super().split(lower_bits)
        return SizeBinary(upper_bits, upper), SizeBinary(lower_bits, lower)
```

As already explained we want to get the negative version of the number both with the one's complement and the two's complement techniques. For starters, let's deal with one's complement.

A very smart way to reverse all the n bits of a value x is to compute `(1 << n) - x - 1`. An example will show you that this works, avoiding long and boring mathematical explanations. We want to negate the number 1 represented as 8 bits ('00000001'). First we compute `1 << 8` which returns `100000000`, then we subtract the value 1 of the number which returns `1111111` and finally we subtract the constant 1, which returns `11111110`. Look and behold! So this is exactly what I will implement for my `oc()` function

``` python
    def oc(self):
        return SizeBinary(self.bits, (1 << self.bits) - self._value - 1)
```

From the same boring mathematics tomes we may discover that the two's complement representation of a number is its one's complement version plus 1. So the implementation of `tc()` is straightforward

``` python
    def tc(self):
        return self.oc() + 1
```

As a matter of fact this function doesn't make the relative test (`test_size_binary_TC`) pass, because it depends on the correct implementation of the sum operation, which is still not implemented. So now the updated battle report is: 27 failed, 24 passed.

Right and left shifts are the same of the `Binary` class, except that the result shall fit into the given bits. So we may delegate them to the `Binary` class implementation

``` python
    def __lshift__(self, pos):
        return SizeBinary(self.bits, super().__lshift__(pos))

    def __rshift__(self, pos):
        return SizeBinary(self.bits, super().__rshift__(pos))
```

And this is enough to make the 5 related tests pass.

Now it's time for some arithmetic and logic functions. Since the internal representation of our value is always in base 10, as already done with the arithmetic functions of the `Binary` class we may go back to the base-10 world, perform the requested operation, and go back to the base-2 representation.

The problem with those functions is that if we mix binaries with different sizes we want to "promote" the smaller one, to that the result has the size of the bigger. To handle this situation I isolated some code in a helper function called `_max_and_convert()`

``` python
    def _max_and_convert(self, other):
        try:
            other_bits = other.bits
            sb_other = other
        except AttributeError:
            other_bits = self.bits
            sb_other = SizeBinary(other_bits, other)

        return max(self.bits, other_bits), sb_other
```

It checks if the `other` argument has a `bits` attribute, otherwise converts it into a `SizeBinary`. Then it returns the converted argument and the greatest length between this latter and `self`. Using this function I can then implement some magic methods such as

``` python
    def __add__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__add__(sb_other))
```

This template is used to implement `__add__()`, `__sub__()`, `__mul__()`, `__truediv__()`, `__and__()`, `__or__()`, `__xor__()`. The `__invert__()` method is simpler, missing the second argument

``` python
    def __invert__(self):
        return SizeBinary(self.bits, super().__invert__())
```

With those functions I now have 49 successful tests and 2 still failing, namely `test_size_binary_to_binary` and `test_binary_slice`. The first one depends on the missing `to_binary()` method

``` python
    def to_binary(self):
        return Binary(self)
```

The second needs some consideration: slicing depends on the `__getitem__()` magic method, which is used also for the simple indexing, tested by `test_binary_get_bit` and `test_binary_negative_index`, for example. The requested behaviour of the `SizeBinary` class is to return a single character when indexing a single bit and a `SizeBinary` or the correct length when slicing. The following code handles both situations

``` python
    def __getitem__(self, key):
        res = super().__getitem__(key)
        bits = len(list(res))

        if bits > 1:
            return SizeBinary(bits, res)
        else:
            return res
```

First of all I use the slicing function of the underlying `Binary` object which was already tested and correctly working. Then I compute the length of that result and act depending on the result.

## Resources

* Wikipedia entries on [integer overflow](https://en.wikipedia.org/wiki/Integer_overflow), [signed number representations](https://en.wikipedia.org/wiki/Signed_number_representations), and [bitwise operations](https://en.wikipedia.org/wiki/Bitwise_operation).

The code developed in this post can be found here:

* The full code of the `SizeBinary` class is [here](/code/python-oop-tdd-part2/size_binary.py), while the tests file is [here](/code/python-oop-tdd-part2/test_size_binary.py).
* [Here](/code/python-oop-tdd-part2/binary.zip) you find a zip file containing both source code files and the relative tests.

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.