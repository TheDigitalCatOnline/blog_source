import pytest

from binary import Binary
from size_binary import SizeBinary

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

def test_size_binary_init_bitstr():
    size_binary = SizeBinary(8, '110')
    assert int(size_binary) == 6

def test_size_binary_init_binstr():
    size_binary = SizeBinary(8, '0b110')
    assert int(size_binary) == 6

def test_size_binary_init_hexstr():
    size_binary = SizeBinary(8, '0x6')
    assert int(size_binary) == 6

def test_size_binary_init_hex():
    size_binary = SizeBinary(8, 0x6)
    assert int(size_binary) == 6

def test_size_binary_init_intseq():
    size_binary = SizeBinary(8, [1,1,0])
    assert int(size_binary) == 6

def test_size_binary_init_strseq():
    size_binary = SizeBinary(8, ['1','1','0'])
    assert int(size_binary) == 6


## Comparison

def test_binary_equality_checks_value():
    assert SizeBinary(8, 1) != SizeBinary(8, 2)

def test_binary_equality_checks_bits():
    assert SizeBinary(8, 1) != SizeBinary(16, 1)


## Conversions

def test_size_binary_eq():
    assert SizeBinary(8, 6) == SizeBinary(8, 6)

def test_size_binary_bin():
    register8 = SizeBinary(8, 126)
    assert bin(register8) == '0b1111110'

def test_size_binary_str():
    register8 = SizeBinary(8, 126)
    assert str(register8) == '01111110'

def test_size_binary_hex():
    register8 = SizeBinary(8, 126)
    assert hex(register8) == '0x7e'

def test_size_binary_to_binary():
    register8 = SizeBinary(8, 126)
    assert register8.to_binary() == Binary('1111110')

## Split

def test_size_binary_split():
    size_binary8 = SizeBinary(8, '01010110')
    size_binary4u, size_binary4l = size_binary8.split(4, 4)
    assert (size_binary4u, size_binary4l) == (SizeBinary(4, '0101'), SizeBinary(4, '0110'))

def test_size_binary_split_asymmetric():
    size_binary8 = SizeBinary(8, '01010110')
    size_binary9u, size_binary3l = size_binary8.split(9, 3)
    assert (size_binary9u, size_binary3l) == (SizeBinary(9, '000001010'), SizeBinary(3, '110'))

## Negative numbers

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


## Basic operations

def test_binary_addition_int():
    assert SizeBinary(8, 4) + 1 == SizeBinary(8, 5)

def test_binary_addition_binary():
    assert SizeBinary(8, 4) + SizeBinary(8, 5) == SizeBinary(8, 9)

def test_binary_addition_binary_different_size():
    assert SizeBinary(8, 4) + SizeBinary(16, 5) == SizeBinary(16, 9)

def test_binary_subtraction_int():
    assert SizeBinary(8, 4) - 1  == SizeBinary(8, 3)

def test_binary_subtraction_binary():
    assert SizeBinary(8, 5) - SizeBinary(8, 4)  == SizeBinary(8, 1)

def test_binary_subtraction_binary_different_size():
    assert SizeBinary(16, 5) - SizeBinary(8, 4)  == SizeBinary(16, 1)

def test_binary_multiplication_int():
    assert SizeBinary(8, 5) * 4  == SizeBinary(8, 20)

def test_binary_multiplication_binary():
    assert SizeBinary(8, 5) * SizeBinary(8, 6)  == SizeBinary(8, 30)

def test_binary_multiplication_binary_different_size():
    assert SizeBinary(8, 5) * SizeBinary(16, 6)  == SizeBinary(16, 30)

def test_binary_division_int():
    assert SizeBinary(8, 20) / 4  == SizeBinary(8, 5)

def test_binary_division_rem_int():
    assert SizeBinary(8, 21) / 4 == SizeBinary(8, 5)

def test_binary_division_binary():
    assert SizeBinary(8, 20) / SizeBinary(8, 5) == SizeBinary(8, 4)

def test_binary_division_binary_different_size():
    assert SizeBinary(16, 20) / SizeBinary(8, 5) == SizeBinary(16, 4)

def test_binary_division_rem_binary():
    assert SizeBinary(8, 21) / SizeBinary(8, 5) == SizeBinary(8, 4)

def test_binary_division_rem_binary_different_size():
    assert SizeBinary(8, 21) / SizeBinary(16, 5) == SizeBinary(16, 4)

def test_binary_get_bit():
    size_binary = SizeBinary(8, '01110001')
    assert size_binary[0] == '1'
    assert size_binary[5] == '1'

def test_binary_negative_index():
    size_binary = SizeBinary(8, '01110001')
    assert size_binary[-1] == '0'
    assert size_binary[-2] == '1'

def test_binary_illegal_index():
    with pytest.raises(IndexError):
        SizeBinary(8, '01101010')[8]

def test_binary_inappropriate_type_index():
    with pytest.raises(TypeError):
        SizeBinary(8, '01101010')['key']

def test_binary_for_loop():
    assert [int(i) for i in SizeBinary(8, '01101010')] == [0, 1, 0, 1, 0, 1, 1, 0]

def test_binary_not():
    assert ~SizeBinary(4, '1101') == SizeBinary(4, '0010')

def test_binary_and():
    assert SizeBinary(4, '1101') & SizeBinary(4, '1') == SizeBinary(4, '1')

def test_binary_or():
    assert SizeBinary(4, '1101') | SizeBinary(4, '1') == SizeBinary(4, '1101')

def test_binary_xor():
    assert SizeBinary(4, '1101') ^ SizeBinary(4, '1') == SizeBinary(4, '1100')

def test_binary_shl():
    assert SizeBinary(4, '1101') << 1 == SizeBinary(4, '1010')

def test_binary_shl_pos():
    assert SizeBinary(8, '1101') << 5 == SizeBinary(8, '10100000')

def test_binary_shl_pos_drop():
    assert SizeBinary(4, '1101') << 4 == SizeBinary(4, '0000')

def test_binary_shr():
    assert SizeBinary(4, '1101') >> 1 == SizeBinary(4, '110')

def test_binary_shr_pos():
    assert SizeBinary(4, '1101') >> 5 == SizeBinary(4, '0')

## Slicing

def test_binary_slice():
    assert SizeBinary(8, '01101010')[0:3] == SizeBinary(2, '10')
    assert SizeBinary(8, '01101010')[1:4] == SizeBinary(3, '101')
    assert SizeBinary(8, '01101010')[4:] == SizeBinary(3, '110')


