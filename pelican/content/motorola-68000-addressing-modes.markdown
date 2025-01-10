Title: Motorola 68000: addressing modes
Date: 2019-03-04 22:30:00 +0100
Category: Retro
Tags: Assembly, M68000, retroprogramming
Authors: Leonardo Giordani
Slug: motorola-68000-addressing-modes
Image: mc68000
Style: retro
Summary: A review of the addressing modes of one of the most famous 32-bit CPUs

The Motorola 68000 is an impressive microprocessor, and this is reflected by the large amount of addressing modes that it provides; it is actually surprising, for people used to the x86 family, to find in this microprocessor's Assembly language constructs that are very similar to the ones provided by high level languages such as pre- and postdecrements, or multiconditional branching instructions.

The processor provides 6 different addressing modes, each of which has multiple versions, for a grand total of 14. The specific instruction used to manipulate the data can also work on different data sizes, but these variations are not considered here, being features of the instruction and not of the addressing mode.

Addressing refers to the format of the **effective address** (`ea` or `EA` in many manuals), that is the representation of the source or destination of an instruction. Remember that not all the instructions support all 14 modes, so whenever you read `ea` remember that we are talking about data which address can be represented by one or more of those modes. 

The syntax of the `movea` instruction, for example, is `movea <ea>, An`, which tells us that the source is one of the 14 possible combinations presented here, while the destination is one of the address registers `a1-a6`. Strictly speaking, however, the syntax of the instruction is `movea <ea1>, <ea2>`, where `<ea1>` can be one of the 14 modes, and `<ea2>` can only be an address register (Address Register Direct Mode).

The addressing mode is encoded using three fields of the binary instruction. The **EA Mode** field, the **EA register** field, and the **Extension words**. The first two are 3-bit fields contained in the instruction word, which combination uniquely identifies the addressing mode and the number of the register, in case this is needed. The extension words, instead, are words that follow the instruction word in memory, and that usually represent actual 8-, 16-, or 32-bit numbers.

## Sign extension

Before we discuss the addressing modes provided by the MC68000 it is worth explaining the sign-extension mechanism used by this processor. Sometimes addressing modes use 8-bit or 16-bit data instead of a full long word, for example to provide a constant that is added to a register before using its value. Calculations inside the microprocessor, however, are always performed on 32-bits numbers, so such values are _extended_ to a long word.

There are two ways to extend a byte/word to a long word. One is to pad with zeroes on the left (unsigned extension) and the other is to pad preserving the sign (signed extension). While this doesn't change positive numbers it affects negative ones. Let's consider an 8-bit negative number like -126, which is represented by `10000010` in 8-bit two's complement, `0x82` in hexadecimal. A 32-bit signed extension of this number becomes `0xffffff82`, which is still -126 in 32-bit two's complement, but an unsigned extension would give `0x00000082`, which is 130.

While the MC68000 can use both address and data registers for general-purpose data storage, the two categories are meant to manage data of different nature. In particular, _data registers never sign-extend bytes or words_, as this would change the pure representation of that sequence of bits, adding spurious bits to keep the sign. Addressed, instead, should never change their value, so the _address registers sign-extend incoming values_ to preserve the real address or displacement represented by the bits.

## Addressing Modes

### Register Direct 

This is the simplest addressing mode, as it reads or writes data in one of the microprocessor's registers. There are two versions of it, one for data registers and one for address registers.

#### Data Register Direct

* Assembly syntax: `Dn`
* EA Mode field: `000`
* EA Register field: Register number
* Extension words: 0

This mode addresses the data contained in one of the data registers `d0-d7`. The EA Mode field is `000` and the EA Register field contains the register number. The official documentation uses the syntax `Dn` to identify this mode. No extension words are used.

``` m68k
cmpi.w  #0x1111,d1
```

```
                           Compare
                        +----------+
                        |  0x1111  |
                        +----------+
d1 0x12ca ------------> |  0x12ca  |
                        +----------+
```

#### Address Register Direct Mode

* Assembly syntax: `An`
* EA Mode field: `001`
* EA Register field: Register number
* Extension words: 0

This mode identifies the data contained in one of the address registers `a0-a6`. The EA Mode field is `001` and the EA Register field is the number of the register, while the official syntax for it is `An`.  No extension words are used.

``` m68k
cmpi.w  #0x1111,a1
```

```
                           Compare
                        +------------+
                        |  0x1111    |
                        +------------+
a1 0xfc1d28 ----------> |  0xfc1d28  |
                        +------------+
```

### Register Indirect

As the name of this mode suggests, the addressing is performed using a register, but the data is accessed indirectly. The register doesn't contain the data we want to use, but the address in memory of the data. This is what higher level languages like C call _memory pointer_.

#### Address Register Indirect

* Assembly syntax: `(An)`
* EA Mode field: `010`
* EA Register field: Register number
* Extension words: 0

The simplest form of indirect access is when the address of the data is stored in one of the address registers `a0-a6`. The syntax for this mode is `(An)`, while the binary form has the EA Mode field set to `010` and the EA Register field represents the number of the address register in use. No extension words are used.

The following example compares the number `0x1111` with the content of the memory cell which address is contained in `a1`

``` m68k
cmpi.w  #0x1111,(a1)
```

```
a1 0xfc1d28
      |              0xfc1d24  |         |                Compare
      |                        +---------+              +----------+
      |              0xfc1d26  |         |              |  0x1111  |
      |                        +---------+              +----------+
      +------------> 0xfc1d28  |  0x13c  | -----------> |   0x13c  |
                               +---------+              +----------+
                     0xfc1d2a  |         |
                               +---------+
                     0xfc1d2c  |         |
```

#### Address Register Indirect with Postincrement

* Assembly syntax: `(An)+`
* EA Mode field: `011`
* EA Register field: Register number
* Extension words: 0

This addressing mode is another of the high-level languages constructs that the MC68000 provides directly in its Assembly language. This mode works exactly like the Address Register Indirect, but _after_ the data has been fetched from memory the address register is incremented by the size of the data itself. So, this addressing mode is perfectly suited for algorithms that need to read consecutive arrays from memory, as there is no need to add instructions that increment the pointer.

``` m68k
cmpi.w  #0x1111,(a1)+
```

```
    a1 0xfc1d28
     ^       |              0xfc1d24  |         |                Compare
     |       |                        +---------+              +----------+
     |       |              0xfc1d26  |         |              |  0x1111  |
   +----+    v                        +---------+              +----------+
   | +2 | <--+------------> 0xfc1d28  |  0x13c  | -----------> |   0x13c  |
   +----+                             +---------+              +----------+
                            0xfc1d2a  |         |
                                      +---------+
                            0xfc1d2c  |         |
```

The standard syntax is `(An)+`, and for this mode, the EA Mode field is `011`, while the EA Register field contains the register number.  No extension words are used.

This mode and the following one are very powerful, as they automatically add to the address the size of the data that has been read, so 1 for a byte read, 2 for a word, and 4 for a long word. The only exception to this rule is when the register is `a7`, which is an alias for `sp`, the system Stack Pointer. In that case the pointer is always kept aligned to a word boundary, so the increment is 2 even for a byte read.

#### Address Register Indirect with Predecrement

* Assembly syntax: `-(An)`
* EA Mode field: `100`
* EA Register field: Register number
* Extension words: 0

This is the specular version of the previous mode, where the address register used to point to the data is decremented _before_ the addressing is performed. The standard syntax is `-(An)`; the EA Mode field is `100` and the EA Register field contains the register number. No extension words are used.

``` m68k
cmpi.w  #0x1111,-(a1)
```

```
   a1 0xfc1d28
    ^       |
    |       v
    |    +----+
    |    | -2 |          0xfc1d22  |         |                Compare
    |    +----+                    +---------+              +----------+
    |       |            0xfc1d24  |         |              |  0x1111  |
    |       v                      +---------+              +----------+
    +-------+----------> 0xfc1d26  |  0x13c  | -----------> |   0x13c  |
                                   +---------+              +----------+
                         0xfc1d28  |         |
                                   +---------+
                         0xfc1d2a  |         |
```

#### Address Register Indirect with Displacement

* Assembly syntax: `(d16,An) / d16(An)`
* EA Mode field: `101`
* EA Register field: Register number
* Extension words: 1

The natural evolution of the previous two addressing modes is to use an arbitrary offset that is added to the base address contained in the register. The standard syntax for this mode is `(d16,An)` or `d16(An)`, where `d16` is a 16-bit signed integer. So for example `0xf(a1)` is the data contained in memory at the address `a1 + 0xf`. The EA Mode field is `101` and the EA register fields is the number of the address register used. This address mode requires 1 extension word that contains the 16-bit displacement.

``` m68k
cmpi.w  #0x1111,0x140(a1)
```

```
     a1 0xfc1d28
           |
           v
         +---+
0x140 -> | + |            0xfc1e64  |         |                Compare
         +---+                      +---------+              +----------+
           |              0xfc1e66  |         |              |  0x1111  |
           v                        +---------+              +----------+
           +------------> 0xfc1e68  |  0x13c  | -----------> |   0x13c  |
                                    +---------+              +----------+
                          0xfc1e6a  |         |
                                    +---------+
                          0xfc1e6c  |         |
```

Please note that the displacement is fixed to 16-bit, so its value limited in the range `(-32768,32767)`; the displacement is however sign-extended to 32-bit before being added to the base address.

Note: this mode is sometimes called "Register Indirect with Offset".

#### Address Register Indirect with Index

* Assembly syntax: `(d8,An,Dn)`
* EA Mode field: `110`
* EA Register field: Register number
* Extension words: 1

Besides an addressing mode that mimics C-style loops and a way to perform random access of arrays through a 16-bit displacement, the MC68000 provides a double-indexed array access with this addressing mode. The base address contained in one of the address registers is added to the content of a 16/32-bit register and an 8-bit index. This address mode requires 1 extension word that contains the 8-bit index; only the 8 least significant bits of the extension words are kept and sign-extended to 32-bits before any calculation.

``` m68k
cmpi.w  #0x1111,(0x4,a1,d0)
```

```
    a1 0xfc1d28   d0 0x140
          |            |
          v            |
        +---+          |
        | + | <--------+
        +---+
          |
          v
        +---+
0x4 --> | + |            0xfc1e68  |          |                Compare
        +---+                      +----------+              +----------+
          |              0xfc1e6a  |          |              |  0x1111  |
          v                        +----------+              +----------+
          +------------> 0xfc1e6c  |   data   | -----------> |   0x13c  |
                                   +----------+              +----------+
                         0xfc1e6e  |          |
                                   +----------+
                         0xfc1e70  |          |
   
```

For this mode the EA Mode field is set to `110` and the EA Register field contains the number of the address register in use. The standard syntax used by manuals is `(d8, An, Dn.SIZE)`, where `SIZE` can be either `w` or `l`. This addressing mode can provide an invaluable way to access two-dimensional arrays, and once again shows how powerful this microprocessor is.

Note: this mode is sometimes called "Indexed Register Indirect with Offset"

### Absolute Data

These modes provide a version of the Address Register Indirect mode where the address is specified directly in the instruction and not through a register.

#### Absolute Short Data

* Assembly syntax: `<address>.w`
* EA Mode field: `111`
* EA Register field: `000`
* Extension words: 1

This mode specifies the address of the data in memory through a 16-bit direct operand specified in the extension word. The standard syntax is `<address>.w`, while the EA mode and EA register fields are respectively `111` and `000`. Since the address is a signed word, only the first or the last 32KiB of memory can be addressed (respectively using positive and negative addresses).

#### Absolute Long Data

* Assembly syntax: `<address>.l`
* EA Mode field: `111`
* EA Register field: `001`
* Extension words: 2

This is the 32-bit version of the previous mode, with EA mode and EA register fields set respectively to `111` and `001`. The standard syntax is `<address>.l`, and it requires two extension words. As always in the MC68000 long words are given in big endian order, that is the first word is the most significant part of the address and the second word is the least significant one.

It is worth noting that this mode overcomes the limitation of the previous one, allowing you to access the full 16MiB address space. However, it requires more memory space, having two extension words, and 4 additional CPU cycles to be executed.

### Program Counter Relative

The addressing modes relative to the Program Counter (PC) are the fundamental building block of relocatable programs, as the effective address is computed as a displacement from the address of the current instruction being executed. Strictly speaking the base address is that of the extension word, as will be shown in detail later in this article.

Please note that effective addresses expressed with Program Counter Relative can only be used to read from memory.

#### Program Counter Relative with Displacement

* Assembly syntax: `(d16,PC)` or `d16(PC)`
* EA Mode field: `111`
* EA Register field: `010`
* Extension words: 1

This mode is very similar to Address Register Indirect with Displacement, as both use a 16-bit offset added to a base address; in this case the latter is provided by the PC instead of an address register. The EA mode field is `111` and the EA Register field is `010`. One extension word is needed, to provide the signed 16-bits displacement, extended to 32-bit before any other calculation.

Note: this mode is sometimes called "Program Counter Relative with Offset".

#### Program Counter Relative with Index

* Assembly syntax: `(d8,Dn,PC)`
* EA Mode field: `111`
* EA Register field: `011`
* Extension words: 1

This is the Program Counter version of Address Register Indirect with Index. The EA mode field is `111` and the EA Register field is `011`. One extension word is needed, to provide the signed 8-bits displacement, which will be extended to 32 bit before using it.

Note: this mode is sometimes called "Program Counter Relative with Index and Offset".

### Immediate Data

* Assembly syntax: `#<data>`
* EA Mode field: `111`
* EA Register field: `100`
* Extension words: 1,2

Immediate data uses the plain data written in the extension words instead of referring to the system memory. In this mode you can specify a constant of any length (byte, word, long word). The EA mode and EA register fields are respectively `111` and `100`, and the number of extension words is either 1 (byte and word) or 2 (long word). Remember that the 68000 sign-extends data only when the destination is an address register, leaving it untouched when a data register is used. The standard syntax for this addressing mode is `#<data>`

#### Quick Immediate

This addressing mode is available for a set of 3 instructions only, namely `addq`, `subq`, and `moveq`. For the first two instructions, it allows to specify a value between 1 and 8 (3 bits), while the third one can interact with a full signed byte, managing a value between -128 and 127. The "quick" label comes from the fact that the instructions use bits of their own binary representation to store the data, thus requiring no extension words. As happens for the simple Immediate Data addressing, EA mode field is `111` and EA Register field is `100`.

### Implied 

This is another mode that is available only for some instructions. Those are bound to specific registers, and are thus not really allowing any generic effective address to be used. The registers used in this addressing mode are only `SP`, `PC`, `SP`, `SSP`, `SR`, and `USP`.

## Table of addressing modes

The following table gives an overview of all the addressing modes. For each of them I show the name, the standard Assembly syntax, the value of the EA Mode field, the value of the EA Register field, and the number of extension word required.

| Name  | Syntax | EA Mode | EA Register | Extension words |
|-------|--------|---------|-------------|-----------------|
| Data Register Direct                         | `Dn`                   | `000` | Reg. number | 0   |
| Address Register Direct Mode                 | `An`                   | `001` | Reg. number | 0   |
| Address Register Indirect                    | `(An)`                 | `010` | Reg. number | 0   |
| Address Register Indirect with Postincrement | `(An)+`                | `011` | Reg. number | 0   |
| Address Register Indirect with Predecrement  | `-(An)`                | `100` | Reg. number | 0   |
| Address Register Indirect with Displacement  | `(d16,An)` or `d16(An)` | `101` | Reg. number | 1   |
| Address Register Indirect with Index         | `(d8,Dn,An)`           | `110` | Reg. number | 1   |
| Absolute Short Data                          | `<address>.w`          | `111` | `000`       | 1   |
| Absolute Long Data                           | `<address>.l`          | `111` | `001`       | 2   |
| Program Counter Relative with Displacement   | `(d16,PC)` / `d16(PC)` | `111` | `010`       | 1   |
| Program Counter Relative with Index          | `(d8,Dn,PC)`           | `111` | `011`       | 1   |
| Immediate                                    | `#<data>`              | `111` | `100`       | 1,2 |

## Examples

Let's consider some example of actual MC68000 code that uses effective addressing modes.

### Example 1

``` m68k
0280 0003 ffff          andi.l  #0x3ffff,d0
```

This instruction uses the the Data Register Direct mode to address register `d0`. The instruction format of `andi` is the following

```
| 15 | 14 | 13 | 12 | 11 | 10 | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
| 0  | 0  | 0  | 0  | 0  | 0  | 1  | 0  | SIZE    | EFFECTIVE ADDRESS           |
|                                       |         | MODE         | REGISTER     |
```

which in the example shown above translates to

```
00000010  10    000  000 0000000000000011 1111111111111111
^         ^     ^    ^
andi      long  Dn   d0  
```

So the microprocessor expects the instruction to be followed by two extension words (long), that will contain the immediate data that will be added to the register. The register is selected among the data ones because the EA Mode field is `000`, and the EA Register field selects register number 0. The two following extension words are `0003` and `ffff`, so the number `0x3ffff` is added to the register.

### Example 2

``` m68k
2052                    movea.l (a2),a0
```

The `movea` instruction moves data into an address register, but in this case uses the Address Register Indirect mode to specify the source. The instruction format is

```
| 15 | 14 | 13 | 12 | 11 | 10 | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
| 0  | 0  | SIZE    | DEST. REG.   | 0  | 0  | 1  | SOURCE EFFECTIVE ADDRESS    |
|         |         |              |              | MODE         | REGISTER     |
```

so in this case the hexadecimal code `2052` becomes

```
00  10    000  001  010   010
    ^     ^         ^     ^
    long  a0        (An)  a2
```


### Example 3

``` m68k
397c 0200 0100          move.w  #0x200,0x100(a4)
```

This `move` instruction puts a word with the value `0x200` into an address which is `0x100` above the address pointed by `a4`. It uses Immediate Data for the source and Address Register Indirect with Displacement for the destination. The format of the `move` instruction is

```
| 15 | 14 | 13 | 12 | 11 | 10 | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
| 0  | 0  | SIZE    | DEST. EFFECTIVE ADDRESS     | SOURCE EFFECTIVE ADDRESS    |
|         |         | REGISTER     | MODE         | MODE         | REGISTER     |
```

(please note that register and mode are swapped in the destination part)

In this case we have

```
00    11    100  101       111100
^     ^     ^    ^         ^    
move  word  a4   (d16,An)  Immediate Data
```

It is interesting to note that the word for the source is given with Immediate Data, and is indeed just after the instruction (`0x0200`), followed by the 16-bit displacement for Address Register Indirect with Displacement (`0x0100`).

## LEA: Load Effective Address

Many newcomers to Assembly are confused by the need of the `lea` instruction, so I want to briefly show why we need it, and dig into its low-level representation to clarify possible doubts.

As we saw in the previous sections there are three ways to manage data in the M68000 Assembly language: the first is to mention a _pure number_, the second is to use a _register_, and the third is to use a _memory address_. Registers can be considered memory, but since they are not proper arrays I will consider them something different.

The Immediate Data addressing mode allows us to use pure data in an instruction. For example we may write `move.l  #0x20000,d0`, which puts the number `0x20000` into the first data register. So when we mention a pure number the microprocessor uses its binary representation directly.

When we mention a register, the microprocessor does the only thing it can do with it, that is it reads its value or writes into it. The instruction mentioned previously, `move.l  #0x20000,d0`, puts the number into the register `d0`. An instruction like `cmp.l d2,d3`, instead, reads the value of both registers and performs the comparison.

Memory addresses are similar to registers, but they are identified by a number and not by a name (and are part of a contiguous array). Whenever an instruction mentions a memory address the microprocessor automatically tries to access that location, to read or to write. An instruction like `move.l 0x4,d3` moves into `d3` _the content_ of the address `0x4`, and this happens just because `0x4` is a memory address.

That said, the problem we face is that often we want to compute a memory address and deal with _its value_ and not with its content. For example, if we write `move.w 0xe(a1),d0`, the microprocessor computes `a1 + 0xe`, that is, the content of `a1` plus the number `0xe`, and then fetches the content of that address in memory, putting it into `d0`. How can we compute `a1 + 0xe` and put that _result_ into `d0`?

This is where `lea` comes into play. This instruction loads the effective address computed by the addressing mode that we are using into an address register. So `lea 0xe(a1),a2` puts the sum between the content of `a1` and `0xe` into the register `a2`. Familiarising with `lea` is very important, as it is one of the most important instructions that the Motorola 68000 provides. A quick analysis of the Amiga Kickstart code shows that `lea` is the 4th most used instruction, after `move`, `jsr`, and `bra`.

## Program Counter Relative syntax and representation

As we discussed previously, the two Program Counter Relative modes just mirror Address Register Indirect with Displacement and Address Register Indirect with Index, binding them to the Program Counter instead of a generic register. It is worth however digging exactly into what the microprocessor is doing when decoding this addressing mode, and what the standard Assembly representation means.

To describe the mechanism behind this modes let's consider an example of actual M68000 code

``` m68k
00000364: 41fa ffa6         lea     0x30c(pc),a0
```

This `lea` instruction stores the address `0x30c` into `a0`, but it's pretty evident that this address mode doesn't work like the traditional Address Register Indirect with Displacement. The instruction is at address `0x364` and if we read the displacement as usual we would expect the effective address to be at `0x364 + 0x30c`. It is important to understand that this is what the Assembler (or the disassembler) shows, and that the proper meaning of `0x30c(pc)` is "the address `0x30c` knowing that this instruction is at `0x364`". I believe this clearly shows why relocatable code makes use of this addressing mode. The address that we identify with `0x364` might actually be anywhere in memory, as this number means only `0x364` words after the first instruction (which is at `0x0` in our relative space).

The binary representation of the instruction is actually revealing. The hexadecimal values of the two words `41fa ffa6` become `01000001111110101111111110100110` which can be split as follows

```
0100  000  111  111010    1111111110100110
^     ^         ^         ^
lea   a0        (d16,PC)  extension word
```

According to the documentation of the addressing mode, the extension word `1111111110100110` is a signed 16-bit displacement, so it is a number expressed in two's complement notation. The conversion gives `-0x5a`, which added to the instruction relative address `0x364` surprisingly gives `0x30a`.

The documentation of the addressing mode, however, states that

>In this mode, the operand is in memory. The address of the operand is the sum of the address in the program counter (PC) and the sign-extended 16-bit displacement integer in the extension word. The value in the PC is the address of the extension word.

>(2.2.11, page 13)

The thing that can be easily overlooked is that the PC points to the extension word and not to the instruction word. In this case, while the instruction word is at `0x364`, the extension word is at `0x366`, and `0x366 - 0x5a` gives exactly `0x30c`, which is what the Assembly syntax shows us. As you can see, the Assembler and the Disassembler have to perform some calculations to show the actual relative final value.

## Resources

* Motorola M68000 Family Programmer's Reference Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
* M68000 Microprocessors User's Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/MC68000UM.pdf)
* The 68000 Principles and Programming, Leo J. Scanion, 1981

## Updates

2017-12-24: Reddit user [SpaceShrimp](https://new.reddit.com/user/SpaceShrimp) pointed out the rage of a signed 16-bit number is `(-32768,32767)` and not `(-32767,32768)`. Thanks!

2023-11-16: [René W. Olsen](https://github.com/rolsen74) spotted a mistake in the Address Register Indirect with Index, where the two registers `An` and `Dn` were swapped. Thanks René!

## Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](https://github.com/TheDigitalCatOnline/blog_source/issues) page is the best place to submit corrections.
