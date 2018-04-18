* Install emulator
* Download Kickstart and Workbench
* Download NDK
* Setup editor
* Install vasm
* `export VBCC=/opt/vbcc`
* `export PATH=~/devel/amiga:$VBCC/bin:$PATH`
* `vasmm68k_mot -kick1hunks -Fhunkexe -I/home/leo/devel/amiga/NDK_3.9/Include/include_i -o ${1/.asm/} -nosym $1`
* Workflow
* Disassemble on the development machine
* Disassemble on the Amiga

* Reasons to retroprogramming
* Reason to learn Assembly
* Why Amiga
    - A wonderful architecture
    - An active community
    - A lot of documentation
    - Hardware still available

# How to open a library

When you code in a high level language like C you usually refer to functions of an external library in your code and then provide the library object files on the compiler command line. The code of the library is either included in the code of your program or loaded into memory at runtime, but in both cases the function call is, at machine language level, just a jump to a different address in memory.

The Assembly language is not different, you can always rely on the linker to properly address external libraries. In a platform like Amiga, however, it's custom to access libraries in a direct way, opening the library and manually jumping to the right address.

When the Amiga OS loads a library in memory the Exec master library also analyses the structure of the library and creates the so-called _jump table_. This is nothing more than an array that lists the addresses of the functions exposed by the library, and the order of this list is guaranteed to be fixed between versions of the OS. This is a very simple and effective way to let the OS free to load the library anywhere in memory.

The Exec master library is not different, but this library is loaded as part of the bootstrap process, and the base address is always stored in memory location `$4` (`0x00000004`). To use one of Exec's functions, then, we just need to issue a `jsr <address>`, where address is the current position in memory of the function we want to call. Since we don't know the absolute address, being the library dynamically loaded, we use the library's jump table to retrieve the base address and get the function address as a fixed offset from the former.

Many Amiga programmers knew the addresses by heart, which is fine since the Amiga OS promises not to change them. So for example the `OpenLibrary` function can be found at address `-552` relative to the library base, while `CloseLibrary` is at `-414`. To call the `OpenLibrary` function, then, you need the following code

``` asm
    move.l 4.w,a6
    jsr -552(a6)
```

which moves the address `$4` in to the `a6` register. This way the register will contain the base address of Exec. Then it jumps to the subroutine which address is 552 bytes before the address contained in `a6`. So if `a6` contains an address like `0x20000` the code jump to `0x1fdd8` (`0x20000 - 552`).

The `OpenLibrary` function, however, expects some parameters, as you can see on the documentation page TODO. The pointer to the library name has to be in the register `a1` and the minimum accepted version in `d0` (`0` means that any version is accepted). The code becomes then

``` asm
    move.l 4.w,a6
    clr.l d0
    move.l #libname,a1
    jsr -552(a6)

libname:
    dc.b "somename.library",0
```


# The jump table and `FUNCDEF`

The jump table for the Exec library is specified in one of the include files provided by the NDK, namely `include_i/exec/exec_lib.i`.

```
    FUNCDEF Supervisor
    FUNCDEF execPrivate1
    FUNCDEF execPrivate2
    FUNCDEF execPrivate3
[...]
    FUNCDEF OpenLibrary
[...]
```

As you can see this file makes use of the `FUNCDEF` macro, which is not provided and has to be implemented by the coder. The idea of the macro is very simple: as the order of the jump table does not change we can just replace the first `FUNCDEF` with the offset of the first function in the library and then increment this offset with the default size of the jump address. The expected output of the macro is

```
    _LVOSupervisor     EQU     -30
    _LVOexecPrivate1   EQU     -36
    _LVOexecPrivate2   EQU     -42
    _LVOexecPrivate3   EQU     -48
[...]
    _LVOOpenLibrary    EQU     -552
[...]
```

Please note that the name of the function has been replaced by another string (in this example prepending `_LVO`) to avoid clashes with the actual function definition (`LVO` stands for Library Vector Offset).

The above figures come from the Special Constants contained in the `include_i/exec/libraries.i` file

``` asm
*------ Special Constants ---------------------------------------
LIB_VECTSIZE    EQU 6       ;Each library entry takes 6 bytes
LIB_RESERVED    EQU 4       ;Exec reserves the first 4 vectors
LIB_BASE    EQU -LIB_VECTSIZE
LIB_USERDEF EQU LIB_BASE-(LIB_RESERVED*LIB_VECTSIZE) ;First user func
LIB_NONSTD  EQU LIB_USERDEF
```

AS you can see from the comments Exec reserves the first 4 vectors, so the first function's address is `LIB_USERDEF`. To understand why the addresses are negative and how the offset is computed let's get a snapshot of the library once it has been loaded in memory

http://amigadev.elowar.com/read/ADCD_2.1/AmigaMail_Vol2_guide/node0189.html

``` txt
                              HIGHER MEMORY ADDRESSES
                            +-------------------------+
Last byte of the            | End of the library      |
library loaded in --------->+-------------------------+
memory                      | [...]                   |
                            +-------------------------+
                            | Content of the library  |
                            +-------------------------+
                            | Library structure       |
Library base address ------>+-------------------------+
                            | 1st reserved vector     |
                            +-------------------------+<--- LIB_BASE
                            | 2nd reserved vector     |
                            +-------------------------+<--+
                            | 3rd reserved vector     |   | LIB_VECTSIZE
                            +-------------------------+<--+
                            | 4th reserved vector     |
                            +-------------------------+
                            | 1st defined function    |
                            +-------------------------+<--- LIB_USERDEF
                            | 2nd defined function    |
                            +-------------------------+
                            | [...]                   |
                            +-------------------------+
Last byte of the            | End of the jump table   |
library loaded in --------->+-------------------------+
memory                        LOWER MEMORY ADDRESSES
```

As you can see the library is loaded as expected from the base address towards the higher memory addresses, but at the same time the jump table is prefixed _in reverse order_. This is done to allow you to find the address of a function with a simple (negative) indexing instead of a more complex algorithm. Function number 1 is at address `-1 * address_size`, function number 2 at address `-2 * address_size`, etc.

This is why we use negative offsets to call library functions but positive ones to access the library data and structures.

You can also see from the figure where the Special Constants `LIB_BASE` and `LIB_USERDEF` are located. For Exec the actual values are

``` asm
LIB_BASE    EQU -6
LIB_USERDEF EQU -30
```

A good definition of the `FUNCDEF` macro, thus, is the following

``` asm
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

(Note that the example `FUNCDEF` commented in `libraries.i` won't work as `FUNC_CNT` is defined inside the macro itself, while it has to be already defined before the first use of the macro)

You can put the previous code in a `funcdef.i` file. Icluding it your code allows you to use `_LVO` prefixed labels for the functions that you want to load

``` asm
    INCLUDE "funcdef.i"

    move.l 4.w,a6
    clr.l d0
    move.l #libname,a1
    jsr _LVOOpenLibrary(a6)

libname:
    dc.b "somename.library",0
```

FInally, if you want to be even more explicit you can use the `CALLLIB` macro defined in `libraries.i` and write

``` asm
    INCLUDE "funcdef.i"
    INCLUDE "exec/libraries.i"

    move.l 4.w,a6
    clr.l d0
    move.l #libname,a1
    CALLLIB _LVOOpenLibrary

libname:
    dc.b "somename.library",0
```

## The four reserved vectors

As we saw, Exec reserves 4 vectors at the beginning of the jump table of a library, and this is true for Exec itself. These 4 spaces host 3 standard functions that shall be provided by any library in the Amiga system, `Open()`, `Close()`, and `Expunge()`. The fourth slot is kept for possible future expansions and must contain a function that returns 0.

The offsets of these functions are contained in the `exec/libraries.i` file

``` asm
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

``` asm
LIB_OPEN        EQU     -6
LIB_CLOSE       EQU     -12
LIB_EXPUNGE     EQU     -18
LIB_EXTFUNC     EQU     -24
```

You can try to follow the definitions of the `LIBINIT` and `LIBDEF` macros to obtain the same result. For the Exec Amiga library the definition of the three functions is pretty simple, as that library is already open and shouldn't need to be opened again, neither closed or expunged.

## Kickstart 1.3

The table is called vector table

The Kickstart ROM contains the Exec library code and is responsible of loading it into memory creating the structure that I described above. One of the Exec functions itself, `MakeFunctions()` is used to load the structure of the library in memory, and to do this it uses a table that lists the addresses of the fuctions inside the code.

The concept is not complex. The pseudocode is basically something like this:

``` txt
address1:
    code
    [...]
address2:
    code
    [...]
address3:
    code
    [...]
vectors:
    address1 - vectors
    address2 - vectors
    address3 - vectors
```

In this situation we have 3 functions defined at `address1`, `address2`, adn `address3`, which are relative offsets inside the code. Somewhere in the code at the address `vectors` there is a plain list of the previous addresses, which are not known in advance because they depend on the size of the functions themselves.

One of these functions, let's say the number 2, given the address of a table can create a jump table.

``` txt
address1:
    code
    [...]
address2:
    for each address from START create a jump table entry
    [...]
address3:
    code
    [...]
vectors:
    address1 - vectors
    address2 - vectors
    address3 - vectors
```

As you can see the function at `address2` depends on a global `START` value which will be contained in some register. At this point, since `address2` is known, there can be some code that runs it on the table contained in the code itself

``` txt
    run address2 on vectors
address1:
    code
    [...]
address2:
    for each address from START create a jump table entry
    [...]
address3:
    code
    [...]
vectors:
    address1 - vectors
    address2 - vectors
    address3 - vectors
```

For example, if we consider the Kickstart 1.3 ROM (`exec` 34.2) we find the functions table at `0x1a7c`. The first value is `0x08a0`, and if we sum this value to the address of the table itself we get `0x1a7c + 0x08a0 = 0x231c`. At this address we will find the first function defined in the jump table, in the first reserved slot, namely `Open()`.

The code is the following

``` asm
; Open()

0000231c: 200e      move.l  a6,d0
0000231e: 526e 0020 addq.w  #0x1,0x20(a6)
00002322: 4e75      rts
```

The `Open()` routine expects the address of the library to be in the `a6` register, and returns the same value in `d0` TODO. It then increments the number of current opens, i.e. the number of times the library has been opened. This field is called `LIB_OPENCNT` in the NDK include files, and its relative address from the library base address is `0x20` or 32 bytes.

Let's look at the definitions of the structure of a library to discover how this address was obtained. The `include_i/exewc/libraries.i` file defines the `STRUCTURE LIB` as

``` asm
 STRUCTURE LIB,LN_SIZE
    UBYTE   LIB_FLAGS           ; see below
    UBYTE   LIB_pad         ; must be zero
    UWORD   LIB_NEGSIZE     ; number of bytes before LIB
    UWORD   LIB_POSSIZE     ; number of bytes after LIB
    UWORD   LIB_VERSION     ; major
    UWORD   LIB_REVISION        ; minor
    APTR    LIB_IDSTRING        ; ASCII identification
    ULONG   LIB_SUM         ; the system-calculated checksum
    UWORD   LIB_OPENCNT     ; number of current opens
    LABEL   LIB_SIZE    ;Warning: Size is not a longword multiple!
```

where `LN_SIZE` is the structure of a library node and is defined in `include_i/exec/nodes.i`

``` asm
 STRUCTURE    LN,0    ; List Node
    APTR    LN_SUCC ; Pointer to next (successor)
    APTR    LN_PRED ; Pointer to previous (predecessor)
    UBYTE   LN_TYPE
    BYTE    LN_PRI  ; Priority, for sorting
    APTR    LN_NAME ; ID string, null terminated
    LABEL   LN_SIZE ; Note: word aligned
```

The size of the last structure is 14 bytes: 2 `APTR` (8 bytes) + 1 `UBYTE` (1 byte) + 1 `BYTE` (1 byte) + 1 `APTR` (4 byte) = 14 bytes.

`LABEL` is a macro that creates the definition `LN_SIZE EQU 14` using the actual offset inside the structure, so it is not part of the structure itself. As the comment after the `LABEL` macro says, the structure is word aligned, and indeed its size is a multiple of a word (2 bytes).

The structure `LIB`, then, has a prefix of 14 bytes. The `LIB_OPENCNT` field comes after 2 `UBYTE` (2 bytes) + 4 `UWORD` (8 bytes) + 1 `APTR` (4 bytes) + 1 `ULONG` (4 bytes) = 18 bytes. The offset of the fields is then 14 + 18 bytes = 32 bytes or `0x20` in hexadecimal notation.

The last instruction of the `Open()` function is `rts` that returns to the instruction after the `jsr` that called the function.

Immediately after the definition of the `Open()` function, we find the definition of `Close()`, listed in the vector table as `0x08a8`, which becomes `0x1a7c + 0x08a8 = 0x2324`. The next two entries in the vector table contain the same value `0x08ac`, which translates to the absolute address `0x1a7c + 0x08ac = 0x2328`. This address is then the location of both the `Expunge()` function and the reserved function that must return 0.

The code of the `Close()` function is very simple, it just decrements the open counter (`0x20` in the library). There is no explicit `rts` as `Close()` uses the adjacent `Expunge()` code for that. Since it's impossible to remove the Exec library in the Amiga system, the `Expunge()` function of the Exec library just returns 0, which is exactly what the reserved function has to do (thus the same address in the vector table), and what `Close()` does after having decremented the open counter.

``` asm
; Close()

00002324: 536e 0020 subq.w  #0x1,0x20(a6)
```

``` asm
; Expunge()

00002328: 7000      moveq   #0,d0
0000232a: 4e75      rts     
```

# Types and structures definition

Let's see how the Exec library defines its types, which are the base components of the Amiga system. The main entry point for this investigation is the `include_i/exec/types.i` file.

When working with data structures in Assembly everything is expressed in terms of offsets. The main idea behind Exec structures is to create something like this

``` asm
STRUCT1         EQU     0
OFFSET          SET     0
FIELD1          EQU     OFFSET
OFFSET          EQU     OFFSET+SIZE_OF_FIELD1
FIELD2          EQU     OFFSET
OFFSET          EQU     OFFSET+SIZE_OF_FIELD2
[...]
STRUCT1_SIZE    EQU     OFFSET
```

which, once run through the Assembler program, creates the following code

``` asm
STRUCT1         EQU     0
FIELD1          EQU     0
FIELD2          EQU     SIZE_OF_FIELD1
FIIELD3         EQU     SIZE_OF_FIELD1+SIZE_OF_FIELD2
[...]
STRUCT1_SIZE    EQU     SIZE_OF_FIELD1+...+SIZE_OF_FIELDn
```

So, the type macros are all defined like

``` asm
TYPENAME        MACRO
\1      EQU     SOFFSET
SOFFSET     SET     SOFFSET+SIZE_OF_TYPE
        ENDM
```

For example the `BYTE` macro is

``` asm
BYTE        MACRO       ; byte (8 bits)
\1      EQU     SOFFSET
SOFFSET     SET     SOFFSET+1
        ENDM
```

Let's see now how a real structure is defined. A good example is `LN` defined in `include_i/exec/nodes.i` which represents a node of a linked list.

``` asm
   STRUCTURE    LN,0    ; List Node
    APTR    LN_SUCC ; Pointer to next (successor)
    APTR    LN_PRED ; Pointer to previous (predecessor)
    UBYTE   LN_TYPE
    BYTE    LN_PRI  ; Priority, for sorting
    APTR    LN_NAME ; ID string, null terminated
    LABEL   LN_SIZE ; Note: word aligned
```

the resulting declarations, once the macros have been expanded, is the following

``` asm
LN          EQU     0
LN_SUCC     EQU     0
LN_PRED     EQU     4
LN_TYPE     EQU     8
LN_PRI      EQU     9
LN_NAME     EQU     10
LN_SIZE     EQU     14
```

As you can see the field names are just offset inside the structure, and there is no specific padding at the end to align the structure. In this case there is no need, as the structure size is already a multiple of a word (14 bytes). If we need to align the bytes however we can use a little binary trick.

If you ignore the least significant bit of a binary number you convert it to the nearest even number (downwards)

``` python
>>> bin(13)
>>> '0b1101'
>>> bin(12)
>>> '0b1100'
```

and even numbers are separated exactly by two positions. So if we get the current offset, we increase it by one and roung down to the nearest integer we are alignign the offset to multiples of a word (2 bytes). The `ALIGNWORD` macro in the `include_i/exec/types.i` file implements exactly this algorithm

``` asm
ALIGNWORD   MACRO       ; Align structure offset to nearest word
SOFFSET     SET     (SOFFSET+1)&$fffffffe
        ENDM
```

