Title: Exploring the Amiga - Part 2
Date: 2018-05-28 15:00:00 +0100
Modified: 2019-02-12 20:00:00 +0000
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-2
Series: Exploring the Amiga
Image: exploring-the-amiga-2
Style: retro
Summary: 

# The library jump table

As already mentioned when a library is loaded in memory a jump table is created just before the library base address. This table contains the addresses of the functions exposed by the library, and Exec itself has one.

The jump table functions order for the Exec library is specified in one of the include files provided by the NDK, namely `include_i/exec/exec_lib.i`.

``` text
    FUNCDEF Supervisor
    FUNCDEF execPrivate1
    FUNCDEF execPrivate2
    FUNCDEF execPrivate3
    ...
    FUNCDEF OpenLibrary
    ...
```

As you can see this file makes use of the `FUNCDEF` macro, which is not provided and has to be implemented by the coder. The idea of the macro is very simple: as the order of the jump table does not change we can just replace the first `FUNCDEF` with the offset of the first function in the library and then increment this offset with the default size of the jump address. The expected output of the macro is

``` text
    _LVOSupervisor     EQU     -30
    _LVOexecPrivate1   EQU     -36
    _LVOexecPrivate2   EQU     -42
    _LVOexecPrivate3   EQU     -48
    ...
    _LVOOpenLibrary    EQU     -552
    ...
```

Please note that the name of the function has been replaced by another string prepending `_LVO` to avoid clashes with the actual function definition (LVO stands for Library Vector Offset).

The above figures come from the "Special Constants" section contained in the `include_i/exec/libraries.i` file

``` m68k
*------ Special Constants ---------------------------------------
LIB_VECTSIZE    EQU 6       ;Each library entry takes 6 bytes
LIB_RESERVED    EQU 4       ;Exec reserves the first 4 vectors
LIB_BASE        EQU -LIB_VECTSIZE
LIB_USERDEF     EQU LIB_BASE-(LIB_RESERVED*LIB_VECTSIZE) ;First user func
LIB_NONSTD      EQU LIB_USERDEF
```

AS you can see from the comments, Exec reserves the first 4 vectors, so the first function's address is `LIB_USERDEF`. To understand why the addresses are negative and how the offset is computed let's get a snapshot of the library once it has been loaded in memory

``` c
                               HIGHER MEMORY ADDRESSES
                             +-------------------------+
Last byte of the             | End of the library      |
library loaded in ---------> +-------------------------+
memory                       | [...]                   |
                             +-------------------------+
                             | Content of the library  |
                             +-------------------------+
                             | Library structure       |
Library base address ------> +-------------------------+
                             | 1st reserved vector     | 
                             +-------------------------+ <--- LIB_BASE
                             | 2nd reserved vector     | 
                             +-------------------------+ <--+
                             | 3rd reserved vector     |    | LIB_VECTSIZE
                             +-------------------------+ <--+
                             | 4th reserved vector     | 
                             +-------------------------+ 
                             | 1st defined function    | 
                             +-------------------------+ <--- LIB_USERDEF
                             | 2nd defined function    |
                             +-------------------------+
                             | [...]                   |
                             +-------------------------+
First byte of the            | End of the jump table   |
library loaded in ---------> +-------------------------+
memory                         LOWER MEMORY ADDRESSES
```

You can find an official version of this in the [documentation](http://amigadev.elowar.com/read/ADCD_2.1/AmigaMail_Vol2_guide/node0189.html
). Pay attention that the picture in the documentation represents memory upside down, with lower memory addresses towards the top of the page.

As you can see the library is loaded as expected from the base address towards the higher memory addresses, but at the same time the jump table is prefixed _in reverse order_. This is done to allow you to find the address of a function with a simple (negative) indexing instead of a more complex algorithm. Function number 1 is at address `-1 * address_size`, function number 2 at address `-2 * address_size`, etc.

This is why we use negative offsets to call library functions but positive ones to access the library data and structures.

You can also see from the figure where the Special Constants `LIB_BASE` and `LIB_USERDEF` are located. The actual values are

``` m68k
LIB_BASE    EQU -6
LIB_USERDEF EQU -30
```

A good definition of the `FUNCDEF` macro, thus, is the following

``` m68k
    INCLUDE "exec/libraries.i"

    MACRO   FUNCDEF
_LVO\1      EQU      FUNC_CNT
FUNC_CNT    SET      FUNC_CNT-LIB_VECTSIZE
    ENDM

FUNC_CNT    SET      LIB_USERDEF
```

The last line initializes the `FUNC_CNT` symbol with the `LIB_USERDEF` value. Then each call of the `FUNCDEF <arg>` macro does two things:

1. Creates the `_LVO<arg>` symbol with value `FUNC_CNT` (e.g. `_LVOSupervisor EQU -30`)
2. Decrements the `FUNC_CNT` symbol by `LIB_VECTSIZE`

Please note that the example `FUNCDEF` that you can find (commented) in `libraries.i` won't work out of the box as `FUNC_CNT` is defined inside the macro itself, while it has to be already defined before the first use of the macro.

``` m68k
*------ FUNCDEF is used to parse library offset tables.  Many applications
*------ need a special version of FUNCDEF - you provide your own macro
*------ to match your needs.  Here is an example:
*
*    FUNCDEF     MACRO
*    _LVO\1      EQU    FUNC_CNT
*    FUNC_CNT    SET    FUNC_CNT-6  * Standard offset-6 bytes each
*    FUNC_CNT    EQU    LIB_USERDEF * Skip 4 standard vectors
*                ENDM
```

You can put the `FUNCDEF` macro code in a local include file like `funcdef.i`. Including it your code allows you to use `_LVO` prefixed labels for the functions that you want to load

``` m68k
    INCLUDE "funcdef.i"
    INCLUDE "exec/exec_lib.i"

    move.l  4.w,a6
    clr.l   d0
    move.l  #libname,a1
    jsr     _LVOOpenLibrary(a6)

libname:
    dc.b "somename.library",0
```

Finally, if you want to be even more explicit you can use the `CALLLIB` macro defined in `libraries.i` and write

``` m68k
    INCLUDE "funcdef.i"
    INCLUDE "exec/exec_lib.i"
    INCLUDE "exec/libraries.i"

    move.l  4.w,a6
    clr.l   d0
    move.l  #libname,a1
    CALLLIB _LVOOpenLibrary

libname:
    dc.b "somename.library",0
```

# The four reserved vectors

As we saw, the Amiga system reserves 4 vectors at the beginning of the jump table of a library. These 4 spaces host 3 standard functions that shall be provided by any library, `Open()`, `Close()`, and `Expunge()`. The fourth slot is kept for possible future expansions and must contain a function that returns 0.

The offsets of these functions are contained in the `include_i/exec/libraries.i` file

``` m68k
*----------------------------------------------------------------
*
*   Standard Library Functions
*
*----------------------------------------------------------------

    LIBINIT LIB_BASE

    LIBDEF  LIB_OPEN
    LIBDEF  LIB_CLOSE
    LIBDEF  LIB_EXPUNGE ; must exist in all libraries
    LIBDEF  LIB_EXTFUNC ; for future expansion - must return zero.
```

the effect of the above macros with the previous constants is

``` m68k
LIB_OPEN        EQU     -6
LIB_CLOSE       EQU     -12
LIB_EXPUNGE     EQU     -18
LIB_EXTFUNC     EQU     -24
```

You can try to follow the definitions of the `LIBINIT` and `LIBDEF` macros to obtain the same result.

# Types and structures

Let's see how the Exec library defines its types, which are the base components of the Amiga system. The main entry point for this investigation is the `include_i/exec/types.i` file.

When working with data structures in Assembly, everything is expressed in terms of offsets. The main idea behind structures is to create something like this

``` m68k
STRUCT1         EQU     0
OFFS            SET     0
FIELD1          EQU     OFFS
OFFS            EQU     OFFS+SIZE_OF_FIELD1
FIELD2          EQU     OFFS
OFFS            EQU     OFFS+SIZE_OF_FIELD2
; ...
STRUCT1_SIZE    EQU     OFFS
```

which, once run through the macro expansion, creates the following code

``` m68k
STRUCT1         EQU     0
FIELD1          EQU     0
FIELD2          EQU     SIZE_OF_FIELD1
FIIELD3         EQU     SIZE_OF_FIELD1+SIZE_OF_FIELD2
; ...
STRUCT1_SIZE    EQU     SIZE_OF_FIELD1+...+SIZE_OF_FIELDn
```

So, the type macros are all defined with code like this

``` m68k
TYPENAME    MACRO
\1          EQU     SOFFSET
SOFFSET     SET     SOFFSET+SIZE_OF_TYPE
            ENDM
```

For example the `BYTE` macro is

``` m68k
BYTE        MACRO       ; byte (8 bits)
\1          EQU     SOFFSET
SOFFSET     SET     SOFFSET+1
            ENDM
```

Note that the field is defined with `EQU` to avoid unwanted overwrites, while `SOFFSET` uses `SET` that allows to redefine the symbol.

Let's see now how a real structure is defined. A good example is `LN` defined in `include_i/exec/nodes.i` which represents a node of a linked list.

``` m68k
   STRUCTURE    LN,0    ; List Node
    APTR    LN_SUCC ; Pointer to next (successor)
    APTR    LN_PRED ; Pointer to previous (predecessor)
    UBYTE   LN_TYPE
    BYTE    LN_PRI  ; Priority, for sorting
    APTR    LN_NAME ; ID string, null terminated
    LABEL   LN_SIZE ; Note: word aligned
```

The `STRUCTURE` macro is defined in `types.i` as

``` m68k
STRUCTURE   MACRO       ; structure name, initial offset
\1          EQU     0
SOFFSET     SET     \2
            ENDM
```

And the resulting declarations, once the macros have been expanded, are the following

``` m68k
LN          EQU     0
LN_SUCC     EQU     0
LN_PRED     EQU     4
LN_TYPE     EQU     8
LN_PRI      EQU     9
LN_NAME     EQU     10
LN_SIZE     EQU     14
```

As you can see the field names are just offsets inside the structure, and there is no specific padding at the end to align the structure. In this case there is no need, as the structure size is already a multiple of a word (14 bytes).

## How to align structures

If we need to align the bytes however we can use a little binary trick. If you ignore the least significant bit of a binary number you convert it to the nearest even number (downwards). An example in Python is

``` python
>>> bin(13)
>>> '0b1101'
>>> bin(12)
>>> '0b1100'
```

You can ignore the least significant bits with a simple bitwise AND

``` python
>>> bin(14)
'0b1110'
>>> 14&0b1100
12
```

So if we get the current offset, we increase it by one and round down to the nearest integer we are aligning the offset to multiples of a word (2 bytes). The `ALIGNWORD` macro in the `include_i/exec/types.i` file implements exactly this algorithm

``` m68k
ALIGNWORD   MACRO       ; Align structure offset to nearest word
SOFFSET     SET     (SOFFSET+1)&$fffffffe
            ENDM
```

This can be seen in action in the `CardHandle` structure defined in `include_i/resources/card.i`. The same algorithm is implemented in other parts of the Kickstart code, for example in the `AddMemList` function that adds memory space to the free memory pool.

# What's next

The next article will describe in depth how the jump table of the Exec library is created through the `MakeFunctions` routine. This will be shown step by step discussing the reverse engineering method followed to discover the mechanism and the relevant code.

# Resources

* Amiga System Programmers Guide, Abacus ([pdf here](https://archive.org/details/Amiga_System_Programmers_Guide_1988_Abacus))
* [AmigaOS Developer Docs](http://amigadev.elowar.com)

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.