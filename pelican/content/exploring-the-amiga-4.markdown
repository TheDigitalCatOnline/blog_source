Title: Exploring the Amiga - Part 4
Date: 2018-06-14 14:30:00 +0100
Modified: 2019-02-12 20:00:00 +0000
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-4
Series: Exploring the Amiga
Image: exploring-the-amiga
Style: retro
Summary: 

# The Exec base functions

We found the Kickstart 1.3 (Exec 34.2) vector table at address `0x1a7c`, and the first 4 entries read

``` m68k
00001a7c: 08a0
00001a7e: 08a8
00001a80: 08ac
00001a82: 08ac
```

If we translate these relative values into absolute addresses, summing the address of the table itself, we discover the address of the 4 base functions that every Amiga library has to provide, namely `Open`, `Close`, `Expunge`, and a reserved slot that should contain a function that returns 0.

## `Open`

The first value is `0x08a0`, and if we sum this value to the address of the table itself we get `0x1a7c + 0x08a0 = 0x231c`. At this address we will find the first function defined in the jump table, that is `Open`.

The code is the following

``` m68k
0000231c: 200e          move.l  a6,d0
0000231e: 526e 0020     addq.w  #0x1,0x20(a6)
00002322: 4e75          rts
```

The `Open` routine expects the address of the library to be in the `a6` register, and returns the same value in `d0`. It then adds 1 to the number contained 32 bytes (`0x20`) after the address of the library itself and then returns. To find out what this number is we can go back again to the NDK and its include files.

From a previous investigation we know that, once the library has been installed in memory, there are two structures defined one after the other. The first is the `LN` structure that represents a linked list node, and the second is the `LIB` structure that represents the library.

We find the definition of `LN` in `include_i/exec/nodes.i`

``` m68k
 STRUCTURE    LN,0    ; List Node
    APTR    LN_SUCC ; Pointer to next (successor)
    APTR    LN_PRED ; Pointer to previous (predecessor)
    UBYTE   LN_TYPE
    BYTE    LN_PRI  ; Priority, for sorting
    APTR    LN_NAME ; ID string, null terminated
    LABEL   LN_SIZE ; Note: word aligned
```

and the definition of `LIB` in `include_i/exewc/libraries.i`

``` m68k
 STRUCTURE LIB,LN_SIZE
    UBYTE   LIB_FLAGS       ; see below
    UBYTE   LIB_pad         ; must be zero
    UWORD   LIB_NEGSIZE     ; number of bytes before LIB
    UWORD   LIB_POSSIZE     ; number of bytes after LIB
    UWORD   LIB_VERSION     ; major
    UWORD   LIB_REVISION    ; minor
    APTR    LIB_IDSTRING    ; ASCII identification
    ULONG   LIB_SUM         ; the system-calculated checksum
    UWORD   LIB_OPENCNT     ; number of current opens
    LABEL   LIB_SIZE        ; Warning: Size is not a longword multiple!
```

As you can see the latter mentions the former reserving space for it at the beginning (`LN_SIZE`).

`LABEL` is a macro that creates an alias for the current size of the structure, and you can find its definition in `include_i/exec/types.i`. It works in conjunction with the type macros, which increment the global variable `SOFFSET`. The code `LABEL LN_SIZE` in the `LN` structure produces the definition `LN_SIZE EQU 14`, which is thus a simple marker and does not contribute to the size of the structure itself.

The comment after the `LABEL` macro in the `LN` structure says that the structure is word aligned, and indeed its size is a multiple of a word (2 bytes): 2 `APTR` (8 bytes) + 1 `UBYTE` (1 bytes) + 1 `BYTE` (1 byte) + 1 `APTR` (4 bytes) = 14 bytes.

To find the field updated by `Open` we need to skip 32 bytes. So, after we skip the whole `LN` structure, we still have 18 bytes to skip into the `LIB` structure. At that offset we find the `LIB_OPENCNT` field (remember that `UBYTE` is 1 byte, `UWORD` 2 bytes, and `APTR` and `ULONG` 4 bytes). This field is, as the comment reads, the "number of current opens".

The last instruction of the `Open` function is `rts` (ReTurn from Subroutine) that returns to the instruction after the `jsr` that called the function.

## `Close`, `Expunge`, and the reserved slot

Immediately after the definition of the `Open` function, we find the definition of `Close`, listed in the vector table as `0x08a8`, which becomes `0x1a7c + 0x08a8 = 0x2324`. The next two entries in the vector table contain the same value `0x08ac`, which translates to the absolute address `0x1a7c + 0x08ac = 0x2328`. This address is then the location of both the `Expunge` function and the reserved function that must return 0.

The code of the `Close` function is very simple, it just decrements the open counter (`0x20` in the library). There is no explicit `rts` as `Close` uses the adjacent `Expunge` code for that. Since it's impossible to remove the Exec library in the Amiga system, the `Expunge` function of the Exec library just returns 0, which is exactly what the reserved function has to do (thus the same address in the vector table), and what `Close` does after having decremented the open counter.

``` m68k
; Close
00002324: 536e 0020     subq.w  #0x1,0x20(a6)

; Expunge
00002328: 7000          moveq   #0,d0
0000232a: 4e75          rts     
```

# `MakeFunctions`

The `MakeFunctions` routine is used by Exec to create the vector table at the beginning of the library when it is loaded in memory. You might recall that the vector table is created backward from the beginning of the library, thus allowing to use a simpler addressing scheme.

The prototype of the `MakeFunctions` routine is

``` text
size = MakeFunctions(addess, vectors, offset)
d0                   a0      a1       a2
```

and the code that we found in one of the previous investigations is at offset `0x15b2`

``` m68k
000015b2: 2f0b                      move.l  a3,-(sp)
000015b4: 7000                      moveq   #0,d0
000015b6: 220a                      move.l  a2,d1
000015b8: 6716                      beq.b   0x15d0
000015ba: 3219                      move.w  (a1)+,d1
000015bc: 0c41 ffff                 cmpi.w  #-0x1,d1
000015c0: 6722                      beq.b   0x15e4
000015c2: 47f2 1000                 lea     (0,a2,d1.w),a3
000015c6: 210b                      move.l  a3,-(a0)
000015c8: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015cc: 5c80                      addq.l  #0x6,d0
000015ce: 60ea                      bra.b   0x15ba
000015d0: 2219                      move.l  (a1)+,d1
000015d2: 0c81 ffff ffff            cmpi.l  #-0x1,d1
000015d8: 670a                      beq.b   0x15e4
000015da: 2101                      move.l  d1,-(a0)
000015dc: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015e0: 5c80                      addq.l  #0x6,d0
000015e2: 60ec                      bra.b   0x15d0
000015e4: 265f                      movea.l (sp)+,a3
000015e6: 4e75                      rts     
```

The code is probably easier to read if we replace the addresses with some labels

``` m68k
Setup:
000015b2: 2f0b                      move.l  a3,-(sp)
000015b4: 7000                      moveq   #0,d0
000015b6: 220a                      move.l  a2,d1
000015b8: 6716                      beq.b   Absolute

Relative:
000015ba: 3219                      move.w  (a1)+,d1
000015bc: 0c41 ffff                 cmpi.w  #-0x1,d1
000015c0: 6722                      beq.b   Cleanup
000015c2: 47f2 1000                 lea     (0,a2,d1.w),a3
000015c6: 210b                      move.l  a3,-(a0)
000015c8: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015cc: 5c80                      addq.l  #0x6,d0
000015ce: 60ea                      bra.b   Relative

Absolute:
000015d0: 2219                      move.l  (a1)+,d1
000015d2: 0c81 ffff ffff            cmpi.l  #-0x1,d1
000015d8: 670a                      beq.b   Cleanup
000015da: 2101                      move.l  d1,-(a0)
000015dc: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015e0: 5c80                      addq.l  #0x6,d0
000015e2: 60ec                      bra.b   Absolute

Cleanup:
000015e4: 265f                      movea.l (sp)+,a3
000015e6: 4e75                      rts     
```

## Setup

The first part of the code is

``` m68k
Setup:
000015b2: 2f0b                      move.l  a3,-(sp)
000015b4: 7000                      moveq   #0,d0
000015b6: 220a                      move.l  a2,d1
000015b8: 6716                      beq.b   Absolute
```

The first thing this code does is to save the `a3` register in the stack because it will be changed during the routine.

``` m68k
Setup:
000015b2: 2f0b                      move.l  a3,-(sp)
```

In Assembly, variables are provided by registers and thus are not namespaced, as the registers are the same through the whole program. This is why you should save and restore them and document which one you will change, for instance to return values.

Secondly, the code sets the `d0` register to 0.

``` m68k
000015b4: 7000                      moveq   #0,d0
```

This register, according to the function prototype, will contain the final size of the jump table, and 0 is a sensible starting value.

Lastly, the code moves the `a2` register to `d1` to be able to manipulate it. 

``` m68k
000015b6: 220a                      move.l  a2,d1
000015b8: 6716                      beq.b   Absolute
```

This, however, also sets the processor flags according to the value of `a2` itself, and those flags are used in the next `beq.b` instruction. If `a2` contains the value 0 the table is absolute (code at `0x15d0`, labelled `Absolute` here) otherwise it is relative (code at `0x15ba`, labelled `Relative` here).

## Relative

The second section manages relative jump vectors

``` m68k
Relative:
000015ba: 3219                      move.w  (a1)+,d1
000015bc: 0c41 ffff                 cmpi.w  #-0x1,d1
000015c0: 6722                      beq.b   Cleanup
000015c2: 47f2 1000                 lea     (0,a2,d1.w),a3
000015c6: 210b                      move.l  a3,-(a0)
000015c8: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015cc: 5c80                      addq.l  #0x6,d0
000015ce: 60ea                      bra.b   Relative
```

This fetches one of the vectors from the address stored in `a1` (the address of the vector table), immediately incrementing the register value to point at the next vector.

``` m68k
Relative:
000015ba: 3219                      move.w  (a1)+,d1
```

then compares it with `0xffff` (or `#-0x1`) to see if we reached the end of the table. In that case the code jumps to the fourth section (`0x15e4`, labelled `Cleanup`), otherwise the execution continues with the next instruction

``` m68k
000015bc: 0c41 ffff                 cmpi.w  #-0x1,d1
000015c0: 6722                      beq.b   Cleanup
```

The routine then loads the effective address of the relative vector using `a2` as the base addressing, and stores it in `a3`. This register now contains the final entry that will be part of the jump table.

``` m68k
000015c2: 47f2 1000                 lea     (0,a2,d1.w),a3
```

Since the address of the jump table is contained in `a0`, the resulting absolute vector is stored there, then the code stores the value `0x4ef9` which is the code for the `jmp` instruction (more on this later)

``` m68k
000015c6: 210b                      move.l  a3,-(a0)
000015c8: 313c 4ef9                 move.w  #0x4ef9,-(a0)
```

Note that since we are storing the jump table at negative addresses starting from the library's base pointer we have to copy the argument (the address) first and then the function (the code for `jmp`). The last thing this part of the code does is to add 6 to the size of the jump table (1 word for the instruction, 2 words for the address) and then jumps back to the beginning of the loop.

``` m68k
000015cc: 5c80                      addq.l  #0x6,d0
000015ce: 60ea                      bra.b   Relative
```

## Absolute

The third section is very similar to the second one, because it performs the same actions, only with absolute addresses instead of relative ones.

``` m68k
000015d0: 2219                      move.l  (a1)+,d1
000015d2: 0c81 ffff ffff            cmpi.l  #-0x1,d1
000015d8: 670a                      beq.b   Cleanup
000015da: 2101                      move.l  d1,-(a0)
000015dc: 313c 4ef9                 move.w  #0x4ef9,-(a0)
000015e0: 5c80                      addq.l  #0x6,d0
000015e2: 60ec                      bra.b   Absolute
```

The only difference with the previous section is that there is no need to load the effective address, as the value contained in `d1` is already absolute, so the latter can be stored directly.

## Cleanup

The last section simply restores the stack pointer, popping the value of the `a3` register where the pointer was previously saved, and returns to the caller.

``` m68k
000015e4: 265f                      movea.l (sp)+,a3
000015e6: 4e75                      rts     
```

# Self-modifying code

The creation of the jump table performed by the `MakeFunctions` routine leverages a very powerful feature of any assembly language, that is being able to produce code that self-modifies. This feature comes from a property of machine languages called [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity).

The basic idea of homoiconicity is that the language doesn't consider data and code two different things, thus allowing to use the code as an input for routines and to change it programmatically. Pay attention that self-modifying code is just one of the implications of homoiconicity, and not its definition.

Homoiconicity is a feature rarely provided by languages. It is very powerful, but it basically forces to keep the language at the level of its own AST (Abstract Syntax Tree), which in turn means that you cannot add a proper abstraction, or, if you prefer, a proper high level language, if we identify with this term a computer language that is similar to the human language.

Famous examples of high level languages that are homoiconic are Lisp and Prolog. Lisp is a language that manages lists and its syntax is based on... lists. This means that you can pass Lisp code to a function and transform it like you would do with standard data.

Back to the Motorola Assembly code, the line we are interested in is 

``` m68k
000015c8: 313c 4ef9                 move.w  #0x4ef9,-(a0)
```

This decrements the address contained in `a0` by 2 bytes, then stores at the resulting address the hexadecimal number `0x4ef9`.

The interesting part is that the number `0x4ef9` has a specific meaning for the Motorola 68000 processor, and namely that of the `jmp` instruction. This is clearly shown by the tables in the Programmer's Reference Manual (Section 8, Instruction Format Summary, 8-15).

First we have to convert the number in binary, and we get

``` txt
0x4ef9 -> 0100 1110 1111 1001
```

The first 10 bits (`0100 1110 11`) already identify a `jmp` instruction. The following 6 bits (`111 001`) identify the addressing mode, which in this case is `Absolute Long` or `(xxx).L` (Programmer's Reference Manual, 4-108). This instruction shall then be followed by a 4 bytes absolute address.

With that `move.w`, then, the code writes in memory some Assembly code. Techniques like this have been and are used by games and viruses to obfuscate the code, as a static analysis of the binary will not reveal what will be there only at runtime!

# What's next

The next article will discuss the reason behind the address `0x4` used for the Exec base address, and then will move on discussing linked lists and how Exec manages system resources using them.

# Resources

* Motorola M68000 Family Programmer's Reference Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
* [AmigaOS Developer Docs](http://amigadev.elowar.com)

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.