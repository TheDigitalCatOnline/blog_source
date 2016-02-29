Title: A simple example of Python OOP development (with TDD) - Part 1
Date: 2015-05-13 17:00:00 +0100
Category: Programming
Tags: OOP, Python, Python3, TDD
Authors: Leonardo Giordani
Slug: python-oop-tdd-example-part1
Series: "Python OOP and TDD"
Summary: A simple implementation of binary numbers in Python to exemplify OOP and TDD

If you are eager to learn some Python and do not know how to start, this post may give you some hints. I will develop a very simple Python package from scratch, exemplifying some Object-oriented Programming (OOP) techniques and concepts, and using a Test-Driven Development (TDD) approach.

The package will provide some classes to deal with binary numbers (see the Rationale section), but remember that it is just a toy project. Nothing in this package has been designed with performance in mind: it wants to be as clear as possible.

## Rationale

Binary numbers are rather easy to understand, even if becoming familiar with them requires some time. I expect you to have knowledge of the binary numeral system. If you need to review them just take a look at the [Wikipedia entry](http://en.wikipedia.org/wiki/Binary_number) or one of the countless resources on Internet.

The package we are going to write will provide a class that represents binary numbers (`Binary`) and a class that represents binary numbers with a given bit size (`SizeBinary`). They shall provide basic binary operations like logical (and, or, xor), arithmetic (addition, subtraction, multiplication, division), shifts and indexing.

A quick example of what the package shall do:

``` pycon
>>> b = Binary('0101110001')
>>> hex(b)
'0x171'
>>> int(b)
369
>>> b[0]
'1'
>>> b[9]
'0'
>>> b.SHR()
'10111000'
```

## Python and bases

Binary system is just a _representation_ of numbers with base 2, just like hexadecimal (base 16) and decimal (base 10). Python can already natively deal with different bases, even if internally numbers are always stored as decimal integers. Let us check it

``` pycon
>>> a = 5
>>> a
5
>>> a = 0x5
>>> a
5
>>> a = 0b101
>>> a
5
>>> hex(0b101)
'0x5'
>>> bin(5)
'0b101'
```

As you can see Python understands some common bases out of the box, using the `0x` prefix for hexadecimal numbers and the `0b` for binary ones (and `0o`) for octals). However the number is always printed in its base-10 form (`5` in this case). This means however that a binary number cannot be indexed, since integers does not provide support for this operation

``` pycon
>>> 0b101[0]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'int' object is not subscriptable
```

You can also use a different base when converting things to integers, through the `base` parameter

``` pycon
>>> a = int('101', base=2)
>>> a
5
>>> a = int('10', base=5)
>>> a
5
```

## Test-driven development

Simple tasks are the best way to try and use new development methodologies, so this is a good occasion to start working with the so-called test-driven approach. Test-driven Development (TDD) basically means that the first thing you do when developing is to write some _tests_, that is **programs that use what you are going to develop**. The purpose of those programs is to test that your final product complies with a given behaviour. So they provide 

* **Documentation** for your API: they are examples of use of your package.
* **Regression** checks: when you change the code to develop new features they shall not break the behaviour of the previous package versions.
* **TODO list**: until all tests run successfully you have something still waiting to be implemented.

I suggest you to follow this post until we wrote some tests (section "Writing some tests" included), then **write your own class, trying to make it pass all the tests**. This way, actually developing something, you can really learn both TDD and Python. Then you can check your code against mine and perhaps provide a far better solution that the one found by me.

## Development environment

Let us set up a simple environment for the upcoming development. First of all create a Python virtual environment

``` bash
~$ virtualenv -p python3 venv
```

then activate the virtualenv and install py.test, which is what we will use to write tests.

``` bash
~$ source venv/bin/activate
(venv)~$ pip install pytest
```

Then create a directory for the package, the `__init__.py` file and a directory for tests

``` bash
(venv)~$ mkdir binary
(venv)~$ cd binary
(venv)~/binary$ touch __init__.py
(venv)~/binary$ mkdir tests
```

Finally, let us check that everything is correctly working. Py.test does not find any test so it should exit without errors.

``` bash
(venv)~/binary$ py.test
===================== test session starts =====================
[...]
collected 0 items 

=======================  in 0.00 seconds ======================
```

where `[...]` will be filled with information about your execution environment.

## Py.test

The approach used by py.test is very straightforward: to test your library you just have to write some functions that use it. Those functions shall run without raising any exception; if a test (a function) runs without raising an exception it passes, otherwise it fails. Let us start writing a very simple test to learn the basic syntax. Create a `tests/test_binary.py` file and write the following code

``` python
def test_first():
    pass
```

If you run `py.test` again you shall obtain this result

``` bash
(venv)~/binary$ py.test
===================== test session starts =====================
[...]
collected 1 items 

tests/test_binary.py .

=================== 1 passed in 0.01 seconds ==================
```

if you prefer (as I do) you may use the `-v` verbose switch to get detailed information about what tests have been executed

``` bash
(venv)~/binary$ py.test -v
===================== test session starts =====================
[...]
collected 1 items 

tests/test_binary.py::test_first PASSED

=================== 1 passed in 0.01 seconds ==================
```

By default py.test looks for Python files which name starts with `test_`, and this is why it processes our file `tests/test_binary.py`. For each file it runs all functions which name, again, starts with `test_`, and this is why `test_first()` has been executed.

This latter does nothing, so it runs without raising any exception and the test passes. Let us try to raise an exception

``` python
def test_first():
    raise ValueError
```

which gives the following py.test output

``` bash
(venv)~/binary$ py.test -v
===================== test session starts =====================
[...]
collected 1 items 

tests/test_binary.py::test_first FAILED

=========================== FAILURES ==========================
__________________________ test_first _________________________

    def test_first():
>       raise ValueError
E       ValueError

tests/test_binary.py:2: ValueError
=================== 1 failed in 0.01 seconds ==================
```

To easily write tests that raise exceptions when failing we may use the `assert` Python statement, which shall be followed by an expression. If the expression returns a true value, `assert` does nothing, otherwise it raises an `AssertionError` exception. Let us do a quick check in the Python console

``` pycon
>>> assert True
>>> assert False
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AssertionError
>>>
>>> assert 1 == 1
>>> assert 1 == 2
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
AssertionError
```

So usually our tests will contain some code and one or more assertions. I prefer to have just one assertion for each test, except perhaps when testing the very same feature more than once (for example getting various elements from a list). This way you are immediately aware of what assertion raised the exception, that is you immediately know what feature does not work as expected.

## Writing some tests

So now we will pretend we have already developed our `Binary` class and write some tests that check its behaviour. You will find the whole file in the Resources section at the end of the post, I will show here just some snippets.

### Initialization

``` python
from binary import Binary

def test_binary_init_int():
    binary = Binary(6)
    assert int(binary) == 6
```

This is our first real test. First of all we import the class from the `binary.py` file (which doesn't exists yet). The `test_binary_init_int()` function shall (as the name suggests) initialize a `Binary` with an integer. The assertion checks that the newly created `binary` variable has a consistent integer representation, which is the number we used to initialize it.

We want to be able to initialize a `Binary` with a wide range of values: bit strings (`'110'`), binary strings (`'0b110'`), hexadecimal strings (`'0x6'`), hexadecimal values (`0x6`), lists of integers (`[1,1,0]`) and list of strings (`['1','1','0']`). The following tests check all those cases

``` python
def test_binary_init_bitstr():
    binary = Binary('110')
    assert int(binary) == 6

def test_binary_init_binstr():
    binary = Binary('0b110')
    assert int(binary) == 6

def test_binary_init_hexstr():
    binary = Binary('0x6')
    assert int(binary) == 6

def test_binary_init_hex():
    binary = Binary(0x6)
    assert int(binary) == 6

def test_binary_init_intseq():
    binary = Binary([1,1,0])
    assert int(binary) == 6

def test_binary_init_strseq():
    binary = Binary(['1','1','0'])
    assert int(binary) == 6
```

Finally, let us check that our `Binary` class cannot be initialized with a negative number. I decided to represent negative numbers through the two's complement technique, which however requires a predefined bit length. So for simple binaries I just discard negative numbers.

``` python
import pytest

[...]

def test_binary_init_negative():
    with pytest.raises(ValueError):
        binary = Binary(-4)
```

As you can see, now we have to check that our class raises an exception, but if we make the class raise it the test will fail. To let the test pass we shall check that the exception is raised but suppress it, and this can be done with `pytest.raises`, which is a suitable [context manager](https://www.python.org/dev/peps/pep-0343/).

### Conversions

I want to check that my binary numbers can be correctly converted to integers (through `int()`), binary strings (through `bin()`), hexadecimals (through `hex()`) and to strings (through `str()`). I want the string representation to be a plain sequence of zeros and ones, that is the binary string representation without the `0b` prefix.

Some examples (check the full code for the whole set of tests)

``` python
def test_binary_int():
    binary = Binary(6)
    assert int(binary) == 6

def test_binary_str():
    binary = Binary(6)
    assert str(binary) == '110'
```

### Project structure

I warmly suggest to check [this page](https://pytest.org/latest/goodpractises.html) for a project layout that allows py.test to work flawlessly. To avoid putting too many things in this post I am going to run `py.test` with a custom `PYTHONPATH` to make it correctly import the code. However, please remember that this setup is just a trick for simplicity's sake. Check [this detailed post](http://www.jeffknupp.com/blog/2013/08/16/open-sourcing-a-python-project-the-right-way/) by Jeff Knupp to learn a lot about Python packaging and project layouts.

### Writing the class

Trying to run the tests at this point just returns a big failure due to an import error, since the `binary.py` module does not exists yet.

``` bash
(venv)~/binary$ PYTHONPATH=. py.test -v
===================== test session starts =====================
[...]
collected 0 items / 1 errors
============================ ERRORS ===========================
____________ ERROR collecting tests/test_binary.py ____________
tests/test_binary.py:2: in <module>
    from binary import Binary
E   ImportError: No module named 'binary'

=================== 1 error in 0.01 seconds ===================
```

Let us create the file `binary.py` in the project root (i.e. inside the `binary/` directory you created, together with `__init__.py` and `tests/`) and start creating the `Binary` class.

``` python
class Binary:
    pass
```

Now running py.test shows that all tests can be run and that all them fail (I will just show the first one)

``` bash
(venv)~/binary$ PYTHONPATH=. py.test -v
===================== test session starts =====================
[...]
collected 13 items 

tests/test_binary.py::test_binary_init_int FAILED
[...]

============================ FAILURES ==========================
_____________________ test_binary_init_int _____________________

    def test_binary_init_int():
>       binary = Binary(6)
E       TypeError: object() takes no parameters

tests/test_binary.py:5: TypeError
[...]
=================== 13 failed in 0.03 seconds ==================
```

So now you can start writing code and use your test battery to check if it works as expected. Obviously "as expected" means that all the tests you wrote pass, but this does not imply you covered all cases. TDD is an iterative methodology: when you find a bug or a missing feature you first write a good test or a set of tests that address the matter and then produce some code that make the tests pass.

**At this point you are warmly encouraged to write the code by yourself** and to check your product with the given battery of tests. Just download the first version of the tests file ([`test_binary_ver1.py`](/code/python-oop-tdd-part1/test_binary_ver1.py)) and put it in the `tests/` directory. Then read it carefully to understand what the requirements are and start writing the class. When you think you are done with a part of it just run the tests and see if everything works well, then move on.

## My solution

The most complex part of the class is the initialization, since I want it to accept a wide range of data types. Basically we have to deal with sequences (strings and lists) or with plain values. The latter ones shall be convertible to an integer, otherwise trying to initialize a binary number with them makes no sense. Since binary numbers are just a representation of integers I decided to store the value in the class as an integer inside the `self._value` attribute. Pay attention that this decision means that all leading zeros will be stripped from the number, i.e., `Binary('000101')` is equal to `Binary('101')`. This will be important for indexing and slicing.

This is the code

``` python
import collections

class Binary:
    def __init__(self, value=0):
        if isinstance(value, collections.Sequence):
            if len(value) > 2 and value[0:2] == '0b':
                self._value = int(value, base=2)
            elif len(value) > 2 and value[0:2] == '0x':
                self._value = int(value, base=16)
            else:
                self._value = int(''.join([str(i) for i in value]), base=2)
        else:
            try:
                self._value = int(value)
                if self._value < 0:
                    raise ValueError("Binary cannot accept negative numbers. Use SizedBinary instead")
            except ValueError:
                raise ValueError("Cannot convert value {} to Binary".format(value))

    def __int__(self):
        return self._value
```

Running the tests I get `4 failed, 9 passed in 0.02 seconds`, which is a good score. The tests that still fail are `test_binary_eq`, `test_binary_bin`, `test_binary_str` and `test_binary_hex`. Since I still wrote no code for the conversions those failures were expected.

Let us review the code I wrote. I make use of the `collections` module to tell apart sequences from plain values in a pythonic way. If you do not know what Abstract Base Classes are, please check [this post](/blog/2014/09/04/python-3-oop-part-6-abstract-base-classes) and the [`collections.abc` module documentation](https://docs.python.org/3/library/collections.abc.html).

**WARNING**: if you are using Python 3.2 or less you will find those classes in the `collections` module instead of `collections.abc`, which has been introduced with Python 3.3

Basically through `isinstance(value, collections.abc.Sequence)` we check that the incoming value _behaves_ like a sequence, which is different from saying that it _is_ a `list`, a `string`, or other sequences. The first case covers an incoming string in the form `0bXXXXX`, which is converted to an integer through the `int()` function. The second case is the same but for hexadecimal strings in the form `0xXXXXX`.

The third case covers a generic sequence of values that shall be individually convertible to `0` or `1`. The code converts each element of the sequence to a string, joins them in a single string and converts it with base 2. This covers the case of a string of zeros and ones and the case of an iterable of integers, like a list for example.

If the incoming value is not a sequence it shall be convertible to an integer, which is exactly what the `try` part does. Here we also check if the value is negative and raise a suitable exception.

Finally, the `__int__()` method (one of the Python _magic methods_) is automatically called when we apply `int()` to our binary, just like we do in a lot of the tests. This method is basically the one responsible of providing a conversion to integer of a given class. In this case we just have to return the value we stored internally.

**Obviously I didn't write this code in a single burst. I had to run the tests more than once to tune my code.**

I already wrote the method that performs the conversion to an integer. Some tests however (namely `test_binary_bin` and `test_binary_hex`) still fail with the error message `TypeError: 'Binary' object cannot be interpreted as an integer`.

According to [the official documentation](https://docs.python.org/3/library/functions.html#bin),  "If x is not a Python int object, it has to define an `__index__()` method that returns an integer." so this is what we are missing. As for `__index__()`, the [documentation](https://docs.python.org/3/reference/datamodel.html#object.__index__) states that "In order to have a coherent integer type class, when `__index__()` is defined `__int__()` should also be defined, and both should return the same value."

So we just have to add

``` python
def __index__(self):
    return self.__int__()
```

inside the class, and we get two more successful tests.

To make `test_binary_str` pass we have to provide a magic method that converts the object into a string, which is

``` python
def __str__(self):
    return bin(self)[2:]
```

It makes use of the internal Python algorithm provided by `bin()` stripping the `0b` prefix.

The last failing test is `test_binary_eq` which tests for equality between two `Binary` objects

``` python
def test_binary_eq():
    assert Binary(4) == Binary(4)
```

Out of the box, Python compares objects on a very low level, just checking if the two references point to the same object in memory. To make it smarter we have to provide the `__eq__()` method

``` python
def __eq__(self, other):
    return int(self) == int(other)
```

And now all the tests run successfully.

## Binary operations

Now it is time to add new features to our `Binary` class. As already said, the TDD methodology wants us to first write the tests, then to write the code. Our class is missing some basic arithmetic and binary operations so the tests are (again you will find all tests in the attached file)

``` python
def test_binary_addition_int():
    assert Binary(4) + 1 == Binary(5)

def test_binary_addition_binary():
    assert Binary(4) + Binary(5) == Binary(9)

def test_binary_division_int():
    assert Binary(20) / 4 == Binary(5)

def test_binary_division_rem_int():
    assert Binary(21) / 4 == Binary(5)

def test_binary_get_bit():
    binary = Binary('0101110001')
    assert binary[0] == '1'
    assert binary[5] == '1'
    assert binary[9] == '0'

def test_binary_not():
    assert ~Binary('1101') == Binary('10')

def test_binary_and():
    assert Binary('1101') & Binary('1') == Binary('1')

def test_binary_shl_pos():
    assert Binary('1101') << 5 == Binary('110100000')
```

The first two tests check that adding both an integer and a `Binary` to a `Binary` works as expected. The division checks two different cases: because integer division produces a remainder, which is not considered here. The `test_binary_get_bit` function tests indexing and is one of the few tests that contain more than one assertion. Please note that binary indexing, unlike standard sequence indexing in Python, starts from the rightmost element.

Bitwise and arithmetic operations are implemented using Python magic methods. Please check [the official documentation](https://docs.python.org/3.4/library/operator.html) for a complete list of operators and related methods.

**Now download the file [`test_binary_ver2.py`](/code/python-oop-tdd-part1/test_binary_ver2.py) and use it to develop the new features.**

The code that implements the required behaviour is

``` python
    def __and__(self, other):
        return Binary(self._value & Binary(other)._value)

    def __or__(self, other):
        return Binary(self._value | Binary(other)._value)

    def __xor__(self, other):
        return Binary(self._value ^ Binary(other)._value)

    def __lshift__(self, pos):
        return Binary(self._value << pos)

    def __rshift__(self, pos):
        return Binary(self._value >> pos)

    def __add__(self, other):
        return Binary(self._value + Binary(other)._value)

    def __sub__(self, other):
        return Binary(self._value - Binary(other)._value)

    def __mul__(self, other):
        return Binary(self._value * Binary(other)._value)

    def __truediv__(self, other):
        return Binary(int(self._value / Binary(other)._value))

    def __getitem__(self, item):
        return str(self)[-(item + 1)]

    def __invert__(self):
        return Binary([abs(int(i) - 1) for i in str(self)])
```

All methods are straightforward, except perhaps `__getitem__()` and `__invert__()`. The `__invert__()` method is called when performing a bitwise NOT operation (`~`) and is implemented avoiding negative numbers. A simple solution is to convert the `Binary` into a string and then reverse every digit (`abs(int(i) - 1)` returns 0 for '1' and 1 for '0').

The `__getitem__()` method is required to provide indexing, and as said shall index starting from the rightmost element and moving left. This is the reason of the `-(item + 1)` index. Please notice that this method will be changed later to provide full support to slicing.

## Slicing

I want `Binary` to support slicing, just like lists. The difference between lists and my `Binary` type is that for the latter indexes start from the rightmost element. So when I get bits 3:7 of an 8-bit `Binary` I obtain the four leftmost ones. The desired behaviour is thus exemplified by

``` pycon
>>> b = Binary('01101010')
>>> b[4:7]
<binary.Binary object at 0x...> (110)
>>> b[1:3]
<binary.Binary object at 0x...> (101)
```

that can immediately be converted into tests

``` python
def test_binary_slice():
    assert Binary('01101010')[0:3] == Binary('10')
    assert Binary('01101010')[1:4] == Binary('101')
    assert Binary('01101010')[4:] == Binary('110')
```

**You will find the new tests in the [`test_binary_ver3.py`](/code/python-oop-tdd-part1/test_binary_ver3.py) file.**

Running the tests shows that the `__getitem__()` function does not provide support for `slice` objects, raising the exception `TypeError: unsupported operand type(s) for +: 'slice' and 'int'`. Checking [the documentation](https://docs.python.org/3.4/reference/datamodel.html#object.__getitem__) of `__getitem__()` we notice that it shall manage both integers and `slice` objects (this is the missing part) and that it shall raise `IndexError` for illegal indexes to make for loops work. So I immediately add a test for this rule

``` python
def test_binary_illegal_index():
    with pytest.raises(IndexError):
        Binary('01101010')[7]
```

and other tests to match the correct behaviour (you will find them in the full code). Remember that leading zeros are stripped, so getting the index number 7 shall fail, since the binary number has just 7 digits, even if the incoming string has 8 characters.

Instead of trying to reimplement the whole list slicing behaviour with reversed indexes, it is much simpler to make use of it. We can just take the string version of our `Binary`, slice its reversed version and return the result (reversed again). The result of the slice can be a single element, however, so we have to check it against the `Sequence` class

``` python
def __getitem__(self, key):
        reversed_list = [int(i) for i in reversed(str(self))]
        sliced = reversed_list.__getitem__(key)
        if isinstance(sliced, collections.abc.Sequence):
            if len(sliced) > 0:
                return Binary([i for i in reversed(sliced)])
            else:
                return Binary(0)
        else:
            return Binary(sliced)
```

The first list comprehension returns a list of integers with the reversed version of the binary number (i.e. from `Binary('01101')` to `[1, 0, 1, 1]`, remember that leading zeros are stripped). Then we delegate the slice to the `list` type, calling `__getitem__()`. The result of this call may be a sequence or a single element (integer), so we tell apart the two cases. In the first case we reverse the result again, in the second case we just return it. In both cases we create a `Binary` object. The check on the length of the list must be introduced because the slice may return no elements, but the `Binary` class does not accept an empty list.

## Splitting binaries

The last feature I want to add is a `split()` function that divides the binary number in two binaries. The rightmost one shall have the given size in bits, while the leftmost just contains the remaining bits. The following tests exemplify the behaviour of `split()`

``` python
def test_binary_split_no_remainder():
    assert Binary('110').split(4) == (0, Binary('110'))

def test_binary_split_remainder():
    assert Binary('110').split(2) == (1, Binary('10'))

def test_binary_split_exact():
    assert Binary('100010110').split(9) == (0, Binary('100010110'))

def test_binary_split_leading_zeros():
    assert Binary('100010110').split(8) == (1, Binary('10110'))
```

**These new tests are in the file [`test_binary_ver4.py`](/code/python-oop-tdd-part1/test_binary_ver4.py).**

The code that implements it is

``` python
def split(self, bits):
    return (self[bits:], self[:bits])
```

## Resources

* [Wikipedia entry](http://en.wikipedia.org/wiki/Binary_number) on binary numbers.
* [Context managers](https://www.python.org/dev/peps/pep-0343/) in Python.
* [Py.test project layouts](https://pytest.org/latest/goodpractises.html).
* Jeff Knupp on [open sourcing a Python project](http://www.jeffknupp.com/blog/2013/08/16/open-sourcing-a-python-project-the-right-way/).
* The Python [`collections.abc` module documentation](https://docs.python.org/3/library/collections.abc.html).
* [Python operators documentation](https://docs.python.org/3.4/library/operator.html).

The code developed in this post can be found here:

* The full code of the `Binary` class is [here](/code/python-oop-tdd-part1/binary.py).
* The last version of the tests file is [here](/code/python-oop-tdd-part1/test_binary_ver4.py).

## Final words

If you tried and write your own class before checking my solution I'm sure you experienced both some frustration when tests failed and a great joy when they finally passed. I'm also sure that you could appreciate the simplicity of TDD and perhaps understand why so many programmes adopt it.

In the next post I will guide you through the addition of the `SizeBinary` class, again following the TDD methodology.

## Updates

2015-05-15 As suggested by [Jacob Zimmerman](https://plus.google.com/u/0/b/110554719587236016835/+JacobZimmermanCrapEDM/posts) in [this comment](https://plus.google.com/110554719587236016835/posts/F7TrsivP94B) the class lacks some methods to be a complete numeric class, most notably `__radd__` and `__rsub__`. Indeed, my first goal was to show TDD so I did not add the whole series of reflected arithmetic operations. You will find all those methods [here](https://docs.python.org/3/reference/datamodel.html#object.__radd__) and try to implement them following the methodology shown in the post. Jacob also suggested to shorten the `__str__()` implementation, and I fixed it. Thanks Jacob!

2015-09-22 [Christopher McCormack](https://github.com/cmccormack) spotted an error about binary indexing: "starts from the leftmost element" should be "starts from the rightmost element". Now it has been fixed. Thanks Christopher!

## Feedback

Feel free to use [the blog Google+ page](https://plus.google.com/u/0/b/110554719587236016835/110554719587236016835/posts) to comment the post. The [GitHub issues](https://github.com/lgiordani/lgiordani.github.com/issues) page is the best place to submit corrections.


