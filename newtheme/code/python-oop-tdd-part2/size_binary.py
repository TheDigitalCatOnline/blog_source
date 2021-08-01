from binary import Binary

class SizeBinary(Binary):

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

    def __eq__(self, other):
        return super().__eq__(other) and self.bits == other.bits

    def __str__(self):
        s = super().__str__()
        return s.rjust(self.bits, '0')

    def split(self, upper_bits, lower_bits):
        upper, lower = super().split(lower_bits)
        return SizeBinary(upper_bits, upper), SizeBinary(lower_bits, lower)

    def oc(self):
        return SizeBinary(self.bits, (1 << self.bits) - self._value - 1)

    def tc(self):
        return self.oc() + 1

    def __lshift__(self, pos):
        return SizeBinary(self.bits, super().__lshift__(pos))

    def __rshift__(self, pos):
        return SizeBinary(self.bits, super().__rshift__(pos))

    def _max_and_convert(self, other):
        try:
            other_bits = other.bits
            sb_other = other
        except AttributeError:
            other_bits = self.bits
            sb_other = SizeBinary(other_bits, other)

        return max(self.bits, other_bits), sb_other

    def __add__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__add__(sb_other))

    def __sub__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__sub__(sb_other))

    def __mul__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__mul__(sb_other))

    def __truediv__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__truediv__(sb_other))

    def __and__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__and__(sb_other))

    def __or__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__or__(sb_other))

    def __xor__(self, other):
        bits, sb_other = self._max_and_convert(other)
        return SizeBinary(bits, super().__xor__(sb_other))

    def __invert__(self):
        return SizeBinary(self.bits, super().__invert__())

    def __getitem__(self, key):
        res = super().__getitem__(key)
        bits = len(list(res))

        if bits > 1:
            return SizeBinary(bits, res)
        else:
            return res

    def to_binary(self):
        return Binary(self)

