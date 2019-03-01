Title: Exploring the Amiga - Part 6
Date: 2018-06-25 13:00:00 +0100
Modified: 2019-02-12 20:00:00 +0000
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-6
Series: Exploring the Amiga
Image: exploring-the-amiga
Style: retro
Summary: 

# Memory initialisation

The Amiga has two types of memory. The first one is found on-board and it is traditionally referred to as "Chip Memory". This name comes from the fact that both the CPU and the custom chips have access to it, which also means that all this components have to share the access. This slows down the CPU when the custom chips are using the memory. While they can use DMA to access it without blocking the CPU, the memory cannot be accessed by multiple components at the same time.

The second type of memory is called "Fast Memory" because the CPU has exclusive access to it, thus providing better performances. It is also referred to as "expansion memory" since it comes with expansion boards.

At a certain point during boot time Kickstart figures out the types of memory installed in the Amiga machine and puts the expansion memory size in `a4`. If this register contains a non-zero value, Kickstart knows that an expansion board has been installed and configures the memory accordingly.

The memory initialisation code is the following

``` m68k
00000380: 200c                      move.l  a4,d0
00000382: 6724                      beq.b   0x3a8
00000384: 41ee 024c                 lea     0x24c(a6),a0
00000388: 43fa ffa8                 lea     0x332(pc),a1
0000038c: 7400                      moveq   #0,d2
0000038e: 323c 0005                 move.w  #0x5,d1
00000392: 200c                      move.l  a4,d0
00000394: 9088                      sub.l   a0,d0
00000396: 0480 0000 1800            subi.l  #0x1800,d0
0000039c: 6100 1688                 bsr.w   0x1a26
000003a0: 41f8 0400                 lea     0x400.w,a0
000003a4: 7000                      moveq   #0,d0
000003a6: 600a                      bra.b   0x3b2
000003a8: 41ee 024c                 lea     0x24c(a6),a0
000003ac: 203c ffff e800            move.l  #-0x1800,d0
000003b2: 323c 0003                 move.w  #0x3,d1
000003b6: 2448                      movea.l a0,a2
000003b8: 43fa ff6c                 lea     0x326(pc),a1
000003bc: 74f6                      moveq   #-0xa,d2
000003be: d08b                      add.l   a3,d0
000003c0: 9088                      sub.l   a0,d0
000003c2: 6100 1662                 bsr.w   0x1a26
000003c6: 224e                      movea.l a6,a1
000003c8: 6100 107e                 bsr.w   0x1448
```

As I did in the previous post I will split the code in parts and replace some addresses with labels to try and make the routine more understandable

``` m68k
Check_expansion:
00000380: 200c                      move.l  a4,d0
00000382: 6724                      beq.b   Only_chip

Add_expansion:
00000384: 41ee 024c                 lea     0x24c(a6),a0
00000388: 43fa ffa8                 lea     0x332(pc),a1
0000038c: 7400                      moveq   #0,d2
0000038e: 323c 0005                 move.w  #0x5,d1
00000392: 200c                      move.l  a4,d0
00000394: 9088                      sub.l   a0,d0
00000396: 0480 0000 1800            subi.l  #0x1800,d0
0000039c: 6100 1688                 bsr.w   0x1a26
000003a0: 41f8 0400                 lea     0x400.w,a0
000003a4: 7000                      moveq   #0,d0
000003a6: 600a                      bra.b   Add_chip

Only_chip:
000003a8: 41ee 024c                 lea     0x24c(a6),a0
000003ac: 203c ffff e800            move.l  #-0x1800,d0

Add_chip:
000003b2: 323c 0003                 move.w  #0x3,d1
000003b6: 2448                      movea.l a0,a2
000003b8: 43fa ff6c                 lea     0x326(pc),a1
000003bc: 74f6                      moveq   #-0xa,d2
000003be: d08b                      add.l   a3,d0
000003c0: 9088                      sub.l   a0,d0
000003c2: 6100 1662                 bsr.w   0x1a26
000003c6: 224e                      movea.l a6,a1
000003c8: 6100 107e                 bsr.w   0x1448
```

The two addresses `0x326` and `0x332` mentioned in the code contain the two zero-terminated strings `Chip Memory` and `Fast Memory`

``` m68k
; ################################################################
; 'Chip Memory' string

00000326: 43 ; C
00000327: 68 ; h
00000328: 69 ; i
00000329: 70 ; p
0000032a: 20 ; SP
0000032b: 4d ; M
0000032c: 65 ; e
0000032d: 6d ; m
0000032e: 6f ; o
0000032f: 72 ; r
00000330: 79 ; y
00000331: 00 ; NUL

; ################################################################
; 'Fast Memory' string

00000332: 46 ; F
00000333: 61 ; a
00000334: 73 ; s
00000335: 74 ; t
00000336: 20 ; SP
00000337: 4d ; M 
00000338: 65 ; e
00000339: 6d ; m
0000033a: 6f ; o
0000033b: 72 ; r
0000033c: 79 ; y
0000033d: 00 ; NUL
```

Let's analyse the code line by line.

``` m68k
Check_expansion:
00000380: 200c                      move.l  a4,d0
00000382: 6724                      beq.b   Only_chip
```

The first thing that Kickstart does is to check if `a4` contains a non-zero value, which signals that we have a memory expansion board available. If `a4` is zero the code jumps to the part that initialises chip memory only.

``` m68k
Add_expansion:
00000384: 41ee 024c                 lea     0x24c(a6),a0
00000388: 43fa ffa8                 lea     0x332(pc),a1
```

The code then loads two effective addresses. The first one is the first free memory location (`0x24c`) and the second one is the string `Fast Memory`. The reason behind the address `0x24c` is explained in a later section in detail.

The purpose of the code is to call the `AddMemList` routine, which adds the memory to the system free memory pool. It has the following prototype

``` text
size = AddMemList(size, attributes, pri, base, name)
D0                D0    D1          D2   A0    A1
```

where `size` is the size of the memory (bytes), `attributes` contains flags that identify memory attributes, `pri` is the priority of the memory, `base` is the base address of the new area and `name` is a name for this list.

``` m68k
0000038c: 7400                      moveq   #0,d2
0000038e: 323c 0005                 move.w  #0x5,d1
```

The code then prepares the registers for the `AddMemList` call. It gives this memory priority `0` and sets the `attributes` flags to `0x5`, which is `101`, or `PUBLIC` and `FAST` (see `include_i/exec/memory.i`).

``` m68k
00000392: 200c                      move.l  a4,d0
00000394: 9088                      sub.l   a0,d0
00000396: 0480 0000 1800            subi.l  #0x1800,d0
0000039c: 6100 1688                 bsr.w   0x1a26
```

The expansion memory base address is copied in `d0` (this is actually an unneeded repetition of what the code did 6 lines before, I think). It then subtracts the address of the first free location (because if this is not 0 it means that something is already stored in memory) and the size of the stack, that was initialised previously by Kickstart to 6 KBytes (hardcoded). After that the code jumps to `AddMemList` (`0x1a26`).

When the routine returns the code begins the initialisation of the chip memory. The chip memory has to be initialised in two different ways depending on the presence of the expansion memory, as the latter is preferably used by the CPU.

``` m68k
000003a0: 41f8 0400                 lea     0x400.w,a0
000003a4: 7000                      moveq   #0,d0
000003a6: 600a                      bra.b   Add_chip

Only_chip:
000003a8: 41ee 024c                 lea     0x24c(a6),a0
000003ac: 203c ffff e800            move.l  #-0x1800,d0
```

If the initial test on the presence of the expansion memory fails the code jumps directly to `0x03a8`. If the expansion memory has already been initialised, instead, the CPU executes the code at `0x03a0` and then jumps to `0x03b2` (Renamed `Add_chip` here).

``` m68k
000003a0: 41f8 0400                 lea     0x400.w,a0
```

So if there is expansion memory, Exec will be loaded there, which means that both it and the system stack are not in the chip memory. We can then add the whole space above `0x400` (more on this number in a later section)

``` m68k
000003a4: 7000                      moveq   #0,d0
000003a6: 600a                      bra.b   Add_chip
```

Since the stack has already been created in fast memory we can store a 0 in `d0` and jump to the code that adds the memory to the system lists

If the expansion is not present, instead, Exec is installed in the chip memory, so we compute the base like we did for the fast memory.

``` m68k
Only_chip:
000003a8: 41ee 024c                 lea     0x24c(a6),a0
000003ac: 203c ffff e800            move.l  #-0x1800,d0
```

Here we load the effective address of the first free location, `0x24c` bytes after the ExecBase address, and we specify the size of the memory as 6 Kbytes less than the maximum, to keep some space for the system stack. The size is negative as later the memory size will be added to the register.

``` m68k
Add_chip:
000003b2: 323c 0003                 move.w  #0x3,d1
000003b6: 2448                      movea.l a0,a2
000003b8: 43fa ff6c                 lea     0x326(pc),a1
000003bc: 74f6                      moveq   #-0xa,d2
000003be: d08b                      add.l   a3,d0
000003c0: 9088                      sub.l   a0,d0
000003c2: 6100 1662                 bsr.w   0x1a26
```

After this we repeat the same procedure that was described for the fast memory. The attributes are now `CHIP` and `PUBLIC` (`0x3`), the string at `0x326` is `Chip Memory`, and the priority is -10 (`-0xa`). The `a3` register already contains the end address of the chip memory, so we add it to `d0` and then subtract the first free location computed before to get the size of the memory. As happened before, the routine calls `AddMemList` at `0x1a26`.

``` m68k
000003c6: 224e                      movea.l a6,a1
000003c8: 6100 107e                 bsr.w   0x1448
```

The last action of this part of the code is to call `AddLibrary` at `0x1448`. The only parameter the routine requires is the base address of the library in `a1`, which is why the code copies `a6` there.

# The "magic number" `0x24c`

When we discussed the way the memory is initialised we discovered a "magic number" that Kickstart uses to find the first free location in memory

``` m68k
00000384: 41ee 024c                 lea     0x24c(a6),a0
```

The first free location, according to this code is `0x24c` (588) bytes after the Exec base address. The reason behind this number is simple. When the Exec library is installed its structures use exactly 588 bytes, thus that is the address of the first free space in memory.

It's easy to calculate this number. Here you find the annotated version of the `ExecBase` structure that I already used in the previous instalment.

The structure is described in the `include_i/exec/execbase.i` include file, and I added the displacement in bytes of each field. The first column is the displacement in the `ExecBase` structure, while the second starts from `0x22`. The latter comes from the fact that the structure follows an `LN` structure and a `LIB` structure, described in the fourth instalment of this series.

``` m68k
0000 0022    UWORD   SoftVer
0002 0024    WORD    LowMemChkSum
0004 0026    ULONG   ChkBase
0008 002a    APTR    ColdCapture
000c 002e    APTR    CoolCapture
0010 0032    APTR    WarmCapture
0014 0036    APTR    SysStkUpper
0018 003a    APTR    SysStkLower
001c 003e    ULONG   MaxLocMem
0020 0042    APTR    DebugEntry
0024 0046    APTR    DebugData
0028 004a    APTR    AlertData
002c 004e    APTR    MaxExtMem

0030 0052    WORD    ChkSum


******* Interrupt Related ********************************************

    LABEL   IntVects
0032 0054    STRUCT  IVTBE,IV_SIZE
003e 0060    STRUCT  IVDSKBLK,IV_SIZE
004a 006c    STRUCT  IVSOFTINT,IV_SIZE
0056 0078    STRUCT  IVPORTS,IV_SIZE
0062 0084    STRUCT  IVCOPER,IV_SIZE
006e 0090    STRUCT  IVVERTB,IV_SIZE
007a 009c    STRUCT  IVBLIT,IV_SIZE
0086 00a8    STRUCT  IVAUD0,IV_SIZE
0092 00b4    STRUCT  IVAUD1,IV_SIZE
009e 00c0    STRUCT  IVAUD2,IV_SIZE
00aa 00cc    STRUCT  IVAUD3,IV_SIZE
00b6 00d8    STRUCT  IVRBF,IV_SIZE
00c2 00e4    STRUCT  IVDSKSYNC,IV_SIZE
00ce 00f0    STRUCT  IVEXTER,IV_SIZE
00da 00fc    STRUCT  IVINTEN,IV_SIZE
00e6 0108    STRUCT  IVNMI,IV_SIZE


******* Dynamic System Variables *************************************

00f2 0114    APTR    ThisTask

00f6 0118    ULONG   IdleCount
00fa 011c    ULONG   DispCount
00fe 0120    UWORD   Quantum
0100 0122    UWORD   Elapsed
0102 0124    UWORD   SysFlags
0104 0126    BYTE    IDNestCnt
0105 0127    BYTE    TDNestCnt

0106 0128    UWORD   AttnFlags

0108 012a    UWORD   AttnResched
010a 012c    APTR    ResModules
010e 0130    APTR    TaskTrapCode
0112 0134    APTR    TaskExceptCode
0116 0138    APTR    TaskExitCode
011a 013c    ULONG   TaskSigAlloc
011e 0140    UWORD   TaskTrapAlloc


******* System List Headers (private!) ********************************

0120 0142    STRUCT  MemList,LH_SIZE
012e 0150    STRUCT  ResourceList,LH_SIZE
013c 015e    STRUCT  DeviceList,LH_SIZE
014a 016c    STRUCT  IntrList,LH_SIZE
0158 017a    STRUCT  LibList,LH_SIZE
0166 0188    STRUCT  PortList,LH_SIZE
0174 0196    STRUCT  TaskReady,LH_SIZE
0182 01a4    STRUCT  TaskWait,LH_SIZE
0190 01b2    STRUCT  SoftInts,SH_SIZE*5

01e0 0202    STRUCT  LastAlert,4*4

01f0 0212    UBYTE   VBlankFrequency
01f1 0213    UBYTE   PowerSupplyFrequency

01f2 0214    STRUCT  SemaphoreList,LH_SIZE

0200 0222    APTR    KickMemPtr
0204 0226    APTR    KickTagPtr
0208 022a    APTR    KickCheckSum

020c 022e    UBYTE   ExecBaseReserved[10];
0216 0238    UBYTE   ExecBaseNewReserved[20];
022a 024c    LABEL   SYSBASESIZE
```

The include file that I found in the Amiga Developer CD has a slightly different version of this structure that contains fields from higher versions of Exec. This structure can be found in the Amiga System Programmer's Guide, page 308. You can easily find the definitions of values like `SH_SIZE` in the include files contained in the `include_i/exec/` directory.

As you can see the final address is `0x24c`, which is exactly where the free memory begins (remember that `LABEL` is a macro and not a field, so it doesn't use space).

# The "magic numbers" `0x676` and `0x400`

The Motorola 68000 architecture forces to reserve the first 1024 bytes (`0x400`) for the exception vectors. The table of these vectors can be found in the Programmer's Reference Manual, page B-2, and this is the source for the magic number used when adding the chip memory to the system lists in case an expansion memory is installed.

The Exec base address, however, is not `0x400` but `0x676`. As we already know the library is preceded by the jump table, and since Exec exports 105 functions we use `105*6 = 630` bytes for the jump vectors. Adding these 630 bytes to the first 1024 reserved for the exception vectors gives 1654 (`0x676`) as the base address of the library.

``` c
        +---------------------------------------+
        | First free address                    |
   2242 +---------------------------------------+ 0x8c2 <-+
        | ExecBase structure                    |         |
   1688 +---------------------------------------+ 0x698   |
        | LIB structure                         |         | Exec base
   1668 +---------------------------------------+ 0x684   | structure
        | LN structure                          |         |
   1654 +---------------------------------------+ 0x676 <-+
        | Jump vector #105                      |         |
        +---------------------------------------+         |
        | [...]                                 |         |
        +---------------------------------------+         | Exec jump
        | Jump vector #2                        |         | vector table
   1030 +---------------------------------------+ 0x406   |
        | Jump vector #1                        |         |
   1024 +---------------------------------------+ 0x400 <-+
        | End of reserved space                 |         |
        +---------------------------------------+         |
        | [...]                                 |         |
     12 +---------------------------------------+ 0xc     | 1 Kilobyte
        | Vector #2                             |         | reserved by
      8 +---------------------------------------+ 0x8     | the M68k
        | Reset Initial Program Counter         |         | architecture
      4 +---------------------------------------+ 0x4     |
        | Reset Initial Interrupt Stack Pointer |         |
      0 +---------------------------------------+ 0x0   <-+
```

# What's next

It's time to show the complete Exec vector table, as we are going to use and analyse all the functions defined there. I will then discuss the structure of the memory list header and its relationship with linked lists. Last I will dissect the code of `AddMemList` and thus show in detail how the chip and expansion memory areas are added to to free memory pool.

# Resources

* Microprocessor-based system design by Ricardo Gutierrez-Osuna ([slides](
http://courses.cs.tamu.edu/rgutier/ceg411_f01/)), in particular [Lesson 9 - Exception processing](http://courses.cs.tamu.edu/rgutier/ceg411_f01/l9.pdf)
* Motorola M68000 Family Programmer's Reference Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
* Amiga System Programmers Guide, Abacus ([pdf here](https://archive.org/details/Amiga_System_Programmers_Guide_1988_Abacus))

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.