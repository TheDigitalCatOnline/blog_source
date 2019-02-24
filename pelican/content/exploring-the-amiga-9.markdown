Title: Exploring the Amiga - Part 9
Date: 2019-02-19 14:00:00 +0100
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-9
Series: Exploring the Amiga
Image: exploring-the-amiga
Summary: 

At the end of the previous instalment we left Exec at the very end of the `AddLibrary` function. Before this function returns to the caller the system computes a checksum, which in this case is just the arithmetic sum of all the addresses in the jump table of the library we installed. This algorithm, simple as it may be TODO, has several advantages. First of all it is fast, consisting in as many 16 bits sums (`add.w`) as functions in the library. Exec contains 104 functions, which results in TODO clock cycles 104 * TODO, TODO seconds on a BLA TODO
It is also independent from TODO the order of the functions in the library: this is generally speaking not a concern, but since it is a property of this type of sum, it is worth mentioning it. The real concern we have with checksums is how easy it is to create a collision, that is to mistake a set of values for another, based on the fact that they have the same checksum. In this case are we really concerned? I don't believe so. TODO

# `SumLibrary`

To understand how `SumLibrary` works we have to recall the structure of a library node. The library we are going to process is contained in a linked list, thus it starts with a structure `LN`, which is followed ("contains") a structure `LIB`. TODO USEFUL?

In particular we are interested in a field the structure `LIB`, that is `LIB_FLAGS` t. This is the first field of the structure and it is actually a mask of bits, each one representing an on/off (flag) condition. The file `include_i/exec/libraries.i` contains the following definition of `LIB_FLAGS`

``` m68k
*------ LIB_FLAGS bit definitions (all others are system reserved)
    BITDEF  LIB,SUMMING,0  ; system is currently checksumming
    BITDEF  LIB,CHANGED,1  ; something has changed the library since last sum
    BITDEF  LIB,SUMUSED,2  ; indicates if the library allows checksumming
    BITDEF  LIB,DELEXP,3   ; delayed expunge flag (for use by library)
    BITDEF  LIB,EXP0CNT,4  ; special system expunge flag.
```

``` m68k
000014d4: 0829 0002 000e            btst    #0x2,0xe(a1)
000014da: 674a                      beq.b   0x1526
000014dc: 522e 0127                 addq.b  #0x1,0x127(a6)
000014e0: 08a9 0001 000e            bclr    #0x1,0xe(a1)
000014e6: 6704                      beq.b   0x14ec ; [A]
000014e8: 4269 001c                 clr.w   0x1c(a1)
000014ec: 2049                      movea.l a1,a0
000014ee: 3029 0010                 move.w  0x10(a1),d0
000014f2: e248                      lsr.w   #1,d0
000014f4: 7200                      moveq   #0,d1
000014f6: 6002                      bra.b   0x14fa
000014f8: d260                      add.w   -(a0),d1
000014fa: 51c8 fffc                 dbf     d0,0x14f8
000014fe: 3029 001c                 move.w  0x1c(a1),d0
00001502: 671a                      beq.b   0x151e
00001504: b240                      cmp.w   d0,d1
00001506: 671a                      beq.b   0x1522
00001508: 48e7 0106                 movem.l d7/a5-a6,-(sp)
0000150c: 2e3c 8100 0003            move.l  #-0x7efffffd,d7
00001512: 2c78 0004                 movea.l 0x4.w,a6
00001516: 4eae ff94                 jsr     -0x6c(a6)
0000151a: 4cdf 6080                 movem.l (sp)+,d7/a5-a6
0000151e: 3341 001c                 move.w  d1,0x1c(a1)
00001522: 4eae ff76                 jsr     -0x8a(a6)
00001526: 4e75                      rts
```

Since there are some internal jumps it is worth splitting the function in several parts

``` m68k
000014d4: 0829 0002 000e            btst    #0x2,0xe(a1)
000014da: 674a                      beq.b   End
000014dc: 522e 0127                 addq.b  #0x1,0x127(a6)
000014e0: 08a9 0001 000e            bclr    #0x1,0xe(a1)
000014e6: 6704                      beq.b   PrepareChecksum
000014e8: 4269 001c                 clr.w   0x1c(a1)

PrepareChecksum:
000014ec: 2049                      movea.l a1,a0
000014ee: 3029 0010                 move.w  0x10(a1),d0
000014f2: e248                      lsr.w   #1,d0
000014f4: 7200                      moveq   #0,d1
000014f6: 6002                      bra.b   LoopOnJumpTableEntries

AddJumpTableEntry:
000014f8: d260                      add.w   -(a0),d1

LoopOnJumpTableEntries:
000014fa: 51c8 fffc                 dbf     d0,AddJumpTableEntry
000014fe: 3029 001c                 move.w  0x1c(a1),d0
00001502: 671a                      beq.b   StoreNewChecksum
00001504: b240                      cmp.w   d0,d1
00001506: 671a                      beq.b   AllowTaskRescheduling
00001508: 48e7 0106                 movem.l d7/a5-a6,-(sp)
0000150c: 2e3c 8100 0003            move.l  #-0x7efffffd,d7
00001512: 2c78 0004                 movea.l 0x4.w,a6
00001516: 4eae ff94                 jsr     -0x6c(a6)
0000151a: 4cdf 6080                 movem.l (sp)+,d7/a5-a6

StoreNewChecksum:
0000151e: 3341 001c                 move.w  d1,0x1c(a1)

AllowTaskRescheduling:
00001522: 4eae ff76                 jsr     -0x8a(a6)

End:
00001526: 4e75                      rts
```

TODO
In particular we can see that the `PrepareChecksum` and `LoopOnJumpTableEntries` jumps just skip the next instruction. 

``` text
test
branch Skip
instruction
Skip:
```

``` m68k
000014d4: 0829 0002 000e            btst    #0x2,0xe(a1)
000014da: 674a                      beq.b   End
```

The first instruction tests the bit number 2 of the `LIB_FLAGS`, the flag called `LIB_SUMUSED` that, according to the documentation, is set if the library allows the checksum operation. The `btst` manual page (Programmer's Manual, section 4-61, page 165) tells us that the instruction sets `Z` if the bit is zero. The next instruction `beq`, then, branches when the library cannot be checksummed, and indeed the jump address is the end of the function, just before the instruction that returns to the caller.

``` m68k
000014dc: 522e 0127                 addq.b  #0x1,0x127(a6)
000014e0: 08a9 0001 000e            bclr    #0x1,0xe(a1)
000014e6: 6704                      beq.b   0x14ec
000014e8: 4269 001c                 clr.w   0x1c(a1)
```

The code of this function is executed in a task-switching free environment, so the first line performs the `Forbid` operation. The code then checks the `LIB_CHANGED` flag of `LIB_FLAGS` that is set when the library has changed after the last checksum was calculated. The `bclr` instruction clears a bit but at the same time tests it as `btst` would do, that is affecting only `Z`. So, after this instruction we know if the library was changed, but at the same time we set it as unchanged. Only, if the library was changed we execute the last instruction, that clears the `LIB_SUM` field.

This field is documented in `include_i/exec/libraries.i` as `ULONG`, so it uses 4 bytes of the structure memory, but the `SumLibrary` function seems to use the two least significant ones only (`clr.w`).

``` m68k
PrepareChecksum:
000014ec: 2049                      movea.l a1,a0
000014ee: 3029 0010                 move.w  0x10(a1),d0
000014f2: e248                      lsr.w   #1,d0
000014f4: 7200                      moveq   #0,d1
000014f6: 6002                      bra.b   LoopOnJumpTableEntries
```

The first instruction of this part of the function stores the address of the library node so that it can be used and modified in a loop without losing a reference to the whole structure. The address `0x10(a1)` is where the `LIB_NEGSIZE` field of the library node structure is located, so we store the value in `d0` to use it as a counter of the checksum loop. Since we are going to sum words, and the size of the jump table is in bytes, we have to divide the number by 2,  which in Assembly is easily performed with a right shift (`lsr`) of one bit. We then initialise the register `d1` with 0 to be the recipient of the checksum and unconditionally skip the next instruction.

``` m68k
AddJumpTableEntry:
000014f8: d260                      add.w   -(a0),d1

LoopOnJumpTableEntries:
000014fa: 51c8 fffc                 dbf     d0,AddJumpTableEntry
000014fe: 3029 001c                 move.w  0x1c(a1),d0
00001502: 671a                      beq.b   StoreNewChecksum
00001504: b240                      cmp.w   d0,d1
00001506: 671a                      beq.b   AllowTaskRescheduling

```

In Assembly, when you fins an unconditional jump you are probably looking at the beginning of a loop. The jump is a way to perform the initial check that decides if the loop has to be started, like happens when you use TODO

In this case the loop is

``` m68k
000014f6: 6002                      bra.b   LoopOnJumpTableEntries

AddJumpTableEntry:
000014f8: d260                      add.w   -(a0),d1

LoopOnJumpTableEntries:
000014fa: 51c8 fffc                 dbf     d0,AddJumpTableEntry
```

The `dbf` instruction belongs to the `DBcc` family. These instructions perform the same check of the equivalent `Bcc` ones, but they also decrement a counter, stopping the loop when the latter reaches the value -1. This means that there are two conditions in a `DBcc` loop: one is the condition `cc`, and the second one is the counter. In this specific case we use the condition `f` which is always false and is not available for the `Bcc` family, as it would not make sense. The `dbf` instruction, instead, never exits because of the condition (always false) and just decrements the counter, leaving the loop when it reaches -1.

TODO convoluted not well explained.

The `add.w` instruction predecrements the register `a0`, so the addition is performed exactly `a0` times, which is the reason why that register contains the number of words in the jump table.

``` m68k
000014fe: 3029 001c                 move.w  0x1c(a1),d0
00001502: 671a                      beq.b   StoreNewChecksum
00001504: b240                      cmp.w   d0,d1
00001506: 671a                      beq.b   AllowTaskRescheduling
```

When the loop ends we move the old checksum in `d0`, so we can compare it with the new one. If the library is flagged as changed, the old checksum has been cleared, and the `move.w` instruction will set the `Z` flag. In that case the `beq` instruction will skip the comparison between the two checksums and jump to `StoreNewChecksum`. If the old checksum is present the `cmp.w` instruction compares it with the new one, expecting them to be the same. If they match the code jumps to `AllowTaskRescheduling`, to wrap up the function and return to the caller, otherwise it raises an error.

``` m68k
00001508: 48e7 0106                 movem.l d7/a5-a6,-(sp)
0000150c: 2e3c 8100 0003            move.l  #-0x7efffffd,d7
00001512: 2c78 0004                 movea.l 0x4.w,a6
00001516: 4eae ff94                 jsr     -0x6c(a6)
0000151a: 4cdf 6080                 movem.l (sp)+,d7/a5-a6
```

System errors are managed by Exec through the `Alert` function, which is reachable through the jump table at `-0x6c(a6)`. Before the code calls that function, some registers have to be saved on the stack. The `movem` instruction of the Motorola 68000 can store multiple registers, in this case `d7`, `a5`, `a6`, on the stack and retrieve them in case `Alert` returned.

``` m68k
StoreNewChecksum:
0000151e: 3341 001c                 move.w  d1,0x1c(a1)

AllowTaskRescheduling:
00001522: 4eae ff76                 jsr     -0x8a(a6)

End:
00001526: 4e75                      rts
```

The `move` instruction stores the new checksum in the `LIB_SUM` field, replacing the old one. The last thing the function does is to call `Permit` to return in a multitasking environment, and then returns to the caller.

At this point the function returns to `AddLibrary`, which in turn returns to the main trunk of the code, that we left in the [sixth post]({exploring-the-amiga-6.markdown}). I will continue the analysis of that code in a future post, and temporarily go back to the very beginning of the Kickstart ROM, to have a look at the bootstrap mechanism.

# The Amiga bootstrap mechanism

The Amiga is built around a Motorola 68000, so when the system is powered up the microprocessor start operating. In particular, a microprocessor needs to start fetching instruction from an address, to perform the initial setup of the system before an operating system starts managing the system resources.

The Motorola 68000 reads keeps the fist 1024 KiB of the mapped memory for the exception vectors, that is to list the location of code that has to be run under specific conditions. We discovered this in the [sixth post]({exploring-the-amiga-6.markdown}), when we discussed the magic numbers `0x676` and `0x400`. The table with all the exception vectors can be found in the Motorola 68000 Programmer's Manual, table B-1, page 629.

The first two long words contained in that table are named "Reset Initial Interrupt Stack Pointer" (`0x0`) and "Reset Initial Program Counter" (`0x4`). The "reset" these two vectors refer to can be both the cold or warm boot, so don't mistake it for something that happens only in the second case. When we power up the system, then, we need to store in the first 8 bytes of the memory two sensible addresses, in particular the second one, as the microprocessor will immediately fetch the instruction at that address and execute it. The first address is not very important at the very beginning of the system operations, at least until we need a stack.

The system memory, however, is empty at boot (actually filled with random values), so we need some mechanism that puts the correct address at `0x4`. It is important at this point to understand the difference between physical and logical memory TODO CHECK. The processor accesses memory through some hardware that, given an address, fetches the relative content from the physical memory banks TODO, so what the processor asks in terms of logical addresses, `0x4` in this case, might not correspond to the same physical address.

All the Amiga models (but TODO the Amiga 1000) have Kickstart stored on a ROM, that is a read-only memory chip. This ROM has a size of 262144 bytes (524288 in later models), so its addresses go from `0x00000` to `0x3ffff` (`0x00000` to `0x7ffff` for the bigger version). These are, however, just the relative addresses inside the ROM, and their position in the system memory depends on where we map them. The Amiga systems with a 256KiB Kickstart (TODO < 2.0) map the ROM between `0xf80000` and `0xfbffff`, and again between `0xfc0000` and `0xffffff`. Systems with a Kickstart version equal or higher than 2.0 TODO map the entire ROM between `0xf80000` and `0xffffff` directly.

When the system boots a special circuit re-maps the area between `0xf80000` and `0xffffff` at `0x0`, so that it becomes visible between `0x000000` and `0x07ffff`. This circuit acts according to the status of a bit called "memory overlay" (often shortened with OVL) connected to the lowest bit of CIA-A port A; the latter is itself mapped in memory at the address `0xbfe001`, so can be manipulated by Kickstart.

The strategy employed by the Amiga is thus the following. At boot the ROM is mapped at `0x0`, so the address `0x4` that the 68000 will use as initial Program Counter is the second long word of the ROM itself. At a certain point during the initial bootstrap the memory overlay bit will be cleared, so the addresses from `0x0` will again map the system memory.

TODO OK WHAT THE FUCK DOES THE 0xf00000 ADDRESS DO? CHECK FLASH RAM AND AMIGA MODELS

``` m68k
; ROM fingerprint
00000000: 1111

; Initial instruction and initial program counter
; Motorola 68000 processors read the initial PC from addres 0x4
; in memory and the system boot the Kickstart ROM is mapped at 0x0
00000002: 4ef9 00fc 00d2            jmp     0xfc00d2.l
```

Note that the code jumps at `0xfc00d2`, but while OVL is set this means `0x0000d2`. At that address we find the proper initial code of the Kickstart ROM

