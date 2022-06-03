# strproc.py
# This file implements the technique explained at
# https://www.thedigitalcatonline.com/blog/2014/10/14/decorators-and-metaclasses/

import collections


class FilterClass(type):

    """
    A metaclass that extracts all methods that have
    a _filter attribute and lists them in order in
    the _filters attribute of the class.
    """

    def __prepare__(name, bases, **kwds):
        # This returns an OrderedDict to host the namespace of the created
        # class
        return collections.OrderedDict()

    def __new__(metacls, name, bases, namespace, **kwds):
        # This method returns an instance of the metaclass
        # aka the created class.

        # Just call the standard creation method
        result = type.__new__(metacls, name, bases, dict(namespace))

        # Process all namespace items and extract the marked ones
        result._filters = [
            value for value in namespace.values() if hasattr(value, '_filter')]
        return result


def stringfilter(func):
    # Just sets the _filter attribute in the input callable
    func._filter = True
    return func


class StringProcessor(metaclass=FilterClass):

    """
    A callable class that filters strings.
    It may be given methods decorated with @stringfilter and it will apply
    them on the input string in definition order.
    """

    def __call__(self, string):
        _string = string

        # Loop on filters and apply them on the string
        for _filter in self._filters:
            _string = _filter(self, _string)

        return _string


class MyStringProcessor(StringProcessor):

    @stringfilter
    def capitalize(self, string):
        # This filter just returns the string with the first letter uppercase
        return string.capitalize()

    @stringfilter
    def remove_double_spaces(self, string):
        # This filter replaces double spaces with single ones
        return string.replace('  ', ' ')

if __name__ == '__main__':
    msp = MyStringProcessor()
    input_string = "a test  string"
    output_string = msp(input_string)
    print("INPUT STRING:", input_string)
    print("OUTPUT STRING:", output_string)
