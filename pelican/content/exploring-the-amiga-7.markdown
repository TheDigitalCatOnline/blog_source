Title: Exploring the Amiga - Part 7
Date: 2019-02-11 14:00:00 +0100
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-7
Series: Exploring the Amiga
Image: exploring-the-amiga
Summary: 

# The complete Exec vector table

In [the 3rd post]({filename}exploring-the-amiga-3.markdown) of this series I showed the Exec vectors table and the way `MakeFunctions` creates the jump table when Exec is installed in memory. In that post I focused on the first 4 reserved vectors, but it is useful to have the full table while reading the Kickstart code. This is the full vectors table for Exec 34.2 (28 Oct 1987).

The **Relative offset** column contains the offset of the function in the jump table once the library has been installed, which allows any Amiga program to call Exec functions through the `offset(a6)` syntax (e.g. `OpenLibrary` can be called by `jsr -0x228(a6)`). **Vector position** is the offset of the vector in the Kickstart 1.3 ROM (i.e. `0x0001a7c` is the position of the vector table itself), **Relative address** is the hexadecimal value stored in the table before the relocation, **Absolute address** is the function position after relocation (i.e. the wrapped sum between the table address and the relative address), and **Function** is the function name according to the include files and the Amiga documentation.

| Relative offset | Vector position | Relative address | Absolute address | Function |
|-----------------|-----------------|--------------|------------------|----------|
| -0x00           | 00001a7c        | 08a0         | 0000231c         | Open()   |
| -0x06           | 00001a7e        | 08a8         | 00002324         | Close()  |
| -0x12           | 00001a80        | 08ac         | 00002328         | Expunge()    |
| -0x18           | 00001a82        | 08ac         | 00002328         | Reserved for future use  |
| -0x1e           | 00001a84        | ee6a         | 000008e6         | Supervisor()     |
| -0x24           | 00001a86        | f420         | 00000e9c         | ExitIntr()   |
| -0x2a           | 00001a88        | f446         | 00000ec2         | Schedule()   |
| -0x30           | 00001a8a        | 04f8         | 00001f74         | Reschedule()     |
| -0x36           | 00001a8c        | f4a0         | 00000f1c         | Switch()     |
| -0x3c           | 00001a8e        | f4ea         | 00000f66         | Dispatch()   |
| -0x42           | 00001a90        | f58e         | 0000100a         | Exception()  |
| -0x48           | 00001a92        | f0b0         | 00000b2c         | InitCode()   |
| -0x4e           | 00001a94        | f188         | 00000c04         | InitStruct()     |
| -0x54           | 00001a96        | faac         | 00001528         | MakeLibrary()    |
| -0x5a           | 00001a98        | fb36         | 000015b2         | MakeFunctions()  |
| -0x60           | 00001a9a        | f080         | 00000afc         | FindResident()   |
| -0x66           | 00001a9c        | f0e8         | 00000b64         | InitResident()   |
| -0x6c           | 00001a9e        | 1596         | 00003012         | Alert()  |
| -0x72           | 00001aa0        | 08ee         | 0000236a         | Debug()  |
| -0x78           | 00001aa2        | f9ac         | 00001428         | Disable()    |
| -0x7e           | 00001aa4        | f9ba         | 00001436         | Enable()     |
| -0x84           | 00001aa6        | 051a         | 00001f96         | Forbid()     |
| -0x8a           | 00001aa8        | 0520         | 00001f9c         | Permit()     |
| -0x90           | 00001aaa        | f6e2         | 0000115e         | SetSR()  |
| -0x96           | 00001aac        | f708         | 00001184         | SuperState()     |
| -0x9c           | 00001aae        | f734         | 000011b0         | UserState()  |
| -0xa2           | 00001ab0        | f74e         | 000011ca         | SetIntVector()   |
| -0xa8           | 00001ab2        | f794         | 00001210         | AddIntServer()   |
| -0xae           | 00001ab4        | f7d4         | 00001250         | RemIntServer()   |
| -0xb4           | 00001ab6        | f8e0         | 0000135c         | Cause()  |
| -0xba           | 00001ab8        | fc5c         | 000016d8         | Allocate()   |
| -0xc0           | 00001ac0        | fcc4         | 00001740         | Deallocate()     |
| -0xc6           | 00001abc        | fd54         | 000017d0         | AllocMem()   |
| -0xcc           | 00001abe        | fe00         | 0000187c         | AllocAbs()   |
| -0xd2           | 00001ac0        | fdb0         | 0000182c         | FreeMem()    |
| -0xd8           | 00001ac2        | fe90         | 0000190c         | AvailMem()   |
| -0xde           | 00001ac4        | fede         | 0000195a         | AllocEntry()     |
| -0xe4           | 00001ac6        | ff6c         | 000019e8         | FreeEntry()  |
| -0xea           | 00001ac8        | fb6c         | 000015e8         | Insert()     |
| -0xf0           | 00001aca        | fb98         | 00001614         | AddHead()    |
| -0xf6           | 00001acc        | fba8         | 00001624         | AddTail()    |
| -0xfc           | 00001ace        | fbc0         | 0000163c         | Remove()     |
| -0x102          | 00001ad0        | fbce         | 0000164a         | RemHead() |
| -0x108          | 00001ad2        | fbde         | 0000165a         | RemTail() |
| -0x10e          | 00001ad4        | fbf4         | 00001670         | Enqueue() |
| -0x114          | 00001ad6        | fc1a         | 00001696         | FindName() |
| -0x11a          | 00001ad8        | 0208         | 00001c84         | AddTask() |
| -0x120          | 00001ada        | 02b4         | 00001d30         | RemTask() |
| -0x126          | 00001adc        | 0334         | 00001db0         | FindTask() |
| -0x12c          | 00001ade        | 0388         | 00001e04         | SetTaskPri() |
| -0x132          | 00001ae0        | 03e2         | 00001e5e         | SetSignal() |
| -0x138          | 00001ae2        | 03d8         | 00001e54         | SetExcept() |
| -0x13e          | 00001ae4        | 0490         | 00001f0c         | Wait() |
| -0x144          | 00001ae6        | 0408         | 00001e84         | Signal() |
| -0x14a          | 00001ae8        | 0584         | 00002000         | AllocSignal() |
| -0x150          | 00001aea        | 05bc         | 00002038         | FreeSignal() |
| -0x156          | 00001aec        | 054e         | 00001fca         | AllocTrap() |
| -0x15c          | 00001aee        | 0574         | 00001ff0         | FreeTrap() |
| -0x162          | 00001af0        | 00d8         | 00001b54         | AddPort() |
| -0x168          | 00001af2        | 00f0         | 00001b6c         | RemPort() |
| -0x16e          | 00001af4        | 00f4         | 00001b70         | PutMsg() |
| -0x174          | 00001af6        | 016e         | 00001bea         | GetMsg() |
| -0x17a          | 00001af8        | 019c         | 00001c18         | ReplyMsg() |
| -0x180          | 00001afa        | 01b6         | 00001c32         | WaitPort() |
| -0x186          | 00001afc        | 01de         | 00001c5a         | FindPort() |
| -0x18c          | 00001afe        | f9cc         | 00001448         | AddLibrary() |
| -0x192          | 00001b00        | f9da         | 00001456         | RemLibrary() |
| -0x198          | 00001b02        | f9f0         | 0000146c         | OldOpenLibrary() |
| -0x19e          | 00001b04        | fa26         | 000014a2         | CloseLibrary() |
| -0x1a4          | 00001b06        | fa3a         | 000014b6         | SetFunction() |
| -0x1aa          | 00001b08        | fa58         | 000014d4         | SumLibrary() |
| -0x1b0          | 00001b0a        | ec14         | 00000690         | AddDevice() |
| -0x1b6          | 00001b0c        | ec22         | 0000069e         | RemDevice() |
| -0x1bc          | 00001b0e        | ec26         | 000006a2         | OpenDevice() |
| -0x1c2          | 00001b10        | ec74         | 000006f0         | CloseDevice() |
| -0x1c8          | 00001b12        | ec9c         | 00000718         | DoIO() |
| -0x1ce          | 00001b14        | ec8a         | 00000706         | SendIO() |
| -0x1d4          | 00001b16        | ed0e         | 0000078a         | CheckIO() |
| -0x1da          | 00001b18        | ecb2         | 0000072e         | WaitIO() |
| -0x1e0          | 00001b1a        | ed2a         | 000007a6         | AbortIO() |
| -0x1e6          | 00001b1c        | 01e8         | 00001c64         | AddResource() |
| -0x1ec          | 00001b1e        | 01f0         | 00001c6c         | RemResource() |
| -0x1f2          | 00001b20        | 01f4         | 00001c70         | OpenResource() |
| -0x1f8          | 00001b22        | 07b8         | 00002234         | execPrivate7() |
| -0x1fe          | 00001b24        | 07c2         | 0000223e         | execPrivate8() |
| -0x204          | 00001b26        | 07ee         | 0000226a         | execPrivate9() |
| -0x20a          | 00001b28        | 06a8         | 00002124         | RawDoFmt() |
| -0x210          | 00001b2a        | f700         | 0000117c         | GetCC() |
| -0x216          | 00001b2c        | fdda         | 00001856         | TypeOfMem() |
| -0x21c          | 00001b2e        | 131c         | 00002d98         | Procure() |
| -0x222          | 00001b30        | 1332         | 00002dae         | Vacate() |
| -0x228          | 00001b32        | f9f8         | 00001474         | OpenLibrary() |
| -0x22e          | 00001b34        | 1354         | 00002dd0         | InitSemaphore() |
| -0x234          | 00001b36        | 1374         | 00002df0         | ObtainSemaphore() |
| -0x23a          | 00001b38        | 13c4         | 00002e40         | ReleaseSemaphore() |
| -0x240          | 00001b3a        | 1428         | 00002ea4         | AttemptSemaphore() |
| -0x246          | 00001b3c        | 1458         | 00002ed4         | ObtainSemaphoreList() |
| -0x24c          | 00001b3e        | 14ce         | 00002f4a         | ReleaseSemaphoreList() |
| -0x252          | 00001b40        | 14f4         | 00002f70         | FindSemaphore() |
| -0x258          | 00001b42        | 14e4         | 00002f60         | AddSemaphore() |
| -0x25e          | 00001b44        | 14f0         | 00002f6c         | RemSemaphore() |
| -0x264          | 00001b46        | effc         | 00000a78         | SumKickData() |
| -0x26a          | 00001b48        | ffaa         | 00001a26         | AddMemList() |
| -0x270          | 00001b4a        | 1504         | 00002f80         | CopyMem() |
| -0x276          | 00001b4c        | 1500         | 00002f7c         | CopyMemQuick() |

# The memory list header

Before we dig into the code of the `AddMemList` function to see how the memory space is added to the free memory list, let's have a look at the status of the memory list itself, as we need to be familiar with its structure to understand the rest of the process.

You might recall from the [fifth article]({filename}exploring-the-amiga-5.markdown) that the `MemList` structure is created `0x142` bytes after ExecBase, and that the structure is the following

``` c
    0xe +-------+ 0x150
        |       |
    0xd +-------+ 0x14f (LH_pad)
        | 0xa   |
    0xc +-------+ 0x14e (LH_TYPE)
        | 0x142 |
    0x8 +-------+ 0x14a (LH_TAILPRED)
        | 0x0   |
    0x4 +-------+ 0x146 (LH_TAIL)
        | 0x146 |
    0x0 +-------+ 0x142 (LH_HEAD)
```

This means that we can access the memory list with `lea 0x142(a6),a0`, as `0x142` is the relative address, i.e. assuming ExecBase is 0. Once the absolute address is in one of the address registers we can access the first node through `(a0)`, which is the Motorola Assembly version of the C pointer dereference operation (Address Register Indirect Mode). With `(a0)` we do not use the content of `a0`, but the content of the memory location which address is contained in `a0`.

So if `a0` is `0x142` in the previous figure, `(a0)` is `0x0` (`0x142` contains `0x146`, `0x146` contains `0x0`). It's thus convenient to think of `a0` as the pointer, and of `(a0)` as the value.

It is also interesting to note that the structure at `0x146` is very similar to the `LN` structure. Recall that the latter is defined in `include_i/exec/nodes.i` as

``` m68k
 STRUCTURE    LN,0    ; List Node
    APTR    LN_SUCC ; Pointer to next (successor)
    APTR    LN_PRED ; Pointer to previous (predecessor)
    UBYTE   LN_TYPE
    BYTE    LN_PRI  ; Priority, for sorting
    APTR    LN_NAME ; ID string, null terminated
    LABEL   LN_SIZE ; Note: word aligned
```

and as you can see `LH_TAIL`, `LH_TAILPRED`, and `LH_TYPE` can act as a proper node of a linked list. This is done on purpose (obviously), as the tail of the list has to be processed by the code that manages linked lists.

# Adding free memory to the system list

At the end of the [previous post]({filename}exploring-the-amiga-6.markdown) we left Exec just after its code prepared the parameters of the free memory that was available on the system. As we saw in that post, this operation is always executed for the chip memory, and optionally for the fast memory if the hardware expansion is installed.

The `AddMemList` function at `0x1a26` is called with the following prototype

``` txt
size = AddMemList(size, attributes, pri, base, name)
D0                D0    D1          D2   A0    A1
```

with the purpose of adding the given memory `size`, starting from the `base` address, to the system pool. The memory `attributes` will be store in the memory block and the priority (`pri`) will be used to find the insertion point. The name is also added to the memory node to identify it.

We can split the memory addition in two different parts. `AddMemList` will create a node that contain the parameters of the memory region, then will call `Enqueue` to add it to the `MemList` linked list. When Exec bootstraps the system the memory list is empty, but these functions must work in a generic case. Actually when the system has a memory expansion (fast memory), this is the first that is added to the list, but it's immediately followed by the chip memory.

To help you to follow what happens in the function, this is a depiction of the memory area that we want to add to the system pool at the end of `AddMemList`, just before the call to `Enqueue`. The area contains a header made of three structures, `LN` (linked list node), `MH` (memory header), and `MC` (memory chunk)

``` c
   SIZE +---------------+
        | Free memory   |
        +---------------+
        | ...           |
        +---------------+
        | Free memory   |
   0x2c +---------------+ <-+
        | MC_SIZE       |   |
   0x28 +---------------+   |
        | MC_BYTES      |   | MC
   0x24 +---------------+   |
        | MC_NEXT       |   |
   0x20 +---------------+ <-+
        | MH_FREE       |   |
   0x1c +---------------+   |
        | MH_UPPER      |   |
   0x18 +---------------+   |
        | MH_LOWER      |   | MH
   0x14 +---------------+   |
        | MH_FIRST      |   |
   0x10 +---------------+   |
        | MH_ATTRIBUTES |   |
    0xe +---------------+ <-+
        | LN_NAME       |   |
    0xa +---------------+   |
        | LN_PRI        |   |
    0x9 +---------------+   |
        | LN_TYPE       |   | LN
    0x8 +---------------+   |
        | LN_PRED       |   |
    0x4 +---------------+   |
        | LN_SUCC       |   |
    0x0 +---------------+ <-+
```

It is interesting to note that, since we are managing the system memory, we cannot store information about it other than in the memory itself. The presence of management structures, then, is reducing the amount of free memory. This has to taken into account when designing a memory management system, but will not be considered in this article.

When we create the structure shown in figure, we need to ensure that the actual free memory is a multiple of a long word (8 bytes), as this is generally desirable. The memory space we are dealing with starts from 0 in the picture, but can actually be anywhere in the system memory, so it is not guaranteed that either the beginning or the end of the free memory space are aligned with long words.

The code of `AddMemList` performs the alignment in two steps. First it aligns the address of `MC` to the upper nearest long word (multiple of 8), then aligns the total size to the lower nearest long word. For example, if the starting point was 0 and the total size 64 bytes, we would leave all the values untouched:`LN_SUCC` at `0x0`, `MC_NEXT` at `0x20`, and the total size of the chunk (structure 'MC' + free memory) would be 32 bytes.

``` c
   0x40 +---------------+ <-+
        | Free memory   |   |
        +---------------+   | 32 bytes
        | MC            |   |
   0x20 +---------------+ <-+
        | MH            |   |
        +---------------+   | 32 bytes
        | LN            |   |
    0x0 +---------------+ <-+
```

If the starting point was 1 and the total size 65 bytes, however, the result would be: `LN_SUCC` at `0x1`, `MC_NEXT` at `0x28` (upper long word), and the total size of the memory would be 56 bytes (65-7 gives 58, rounded down to a multiple of 8), which means a chunk of 24 bytes

``` c
   0x42 +---------------+ <-+
        | Ignored       |   | 2 bytes
   0x40 +---------------+ <-+
        | Free memory   |   |
        +---------------+   | 24 bytes
        | MC            |   |
   0x28 +---------------+ <-+
        | EMPTY         |   | 7 bytes
   0x21 +---------------+ <-+
        | MH            |   |
        +---------------+   | 32 bytes
        | LN            |   |
    0x1 +---------------+ <-+
```

# `AddMemList` internals

According to the Exec vectors table, `AddMemList` can be found at `00001a26`, and ends at `00001a78`. It is immediately followed by a padding word `0000` and the vectors table itself

``` m68k
00001a26: 2149 000a                 move.l  a1,0xa(a0)
00001a2a: 43e8 0020                 lea     0x20(a0),a1
00001a2e: 117c 000a 0008            move.b  #0xa,0x8(a0)
00001a34: 1142 0009                 move.b  d2,0x9(a0)
00001a38: 3141 000e                 move.w  d1,0xe(a0)
00001a3c: 2209                      move.l  a1,d1
00001a3e: 5e81                      addq.l  #0x7,d1
00001a40: 0201 00f8                 andi.b  #-0x8,d1
00001a44: c389                      exg     d1,a1
00001a46: 9289                      sub.l   a1,d1
00001a48: d081                      add.l   d1,d0
00001a4a: 0200 00f8                 andi.b  #-0x8,d0
00001a4e: 0480 0000 0020            subi.l  #0x20,d0
00001a54: 2149 0010                 move.l  a1,0x10(a0)
00001a58: 2149 0014                 move.l  a1,0x14(a0)
00001a5c: 2209                      move.l  a1,d1
00001a5e: d280                      add.l   d0,d1
00001a60: 2141 0018                 move.l  d1,0x18(a0)
00001a64: 2140 001c                 move.l  d0,0x1c(a0)
00001a68: 2340 0004                 move.l  d0,0x4(a1)
00001a6c: 4291                      clr.l   (a1)
00001a6e: 2248                      movea.l a0,a1
00001a70: 41ee 0142                 lea     0x142(a6),a0
00001a74: 6100 fc48                 bsr.w   0x16be
00001a78: 4e75                      rts     
```

Let's comment the function line by line. I will refer to the fields shown in the picture above by name, mentioning the offsets for clarity.

``` m68k
00001a26: 2149 000a                 move.l  a1,0xa(a0)
```

This code stores the address of the memory area name in `LN_NAME` (`0xa(a0)`).

``` m68k
00001a2a: 43e8 0020                 lea     0x20(a0),a1
```

This loads the absolute address of the memory chunk (structure `MC`). The purpose of this is to align the memory chunk to a long word later in the code.

``` m68k
00001a2e: 117c 000a 0008            move.b  #0xa,0x8(a0)
00001a34: 1142 0009                 move.b  d2,0x9(a0)
00001a38: 3141 000e                 move.w  d1,0xe(a0)
```

This stores `0xa` in the `LN_TYPE` field (`0x8(a0)`). This corresponds to `NT_MEMORY` (see `include_i/exec/nodes.i`). It then copies the list priority into the `LN_PRI` field (`0x9(a0)`), and the memory attributes in the `MH_ATTRIBUTES` field (`0xe(a0)`).

``` m68k
00001a3c: 2209                      move.l  a1,d1
00001a3e: 5e81                      addq.l  #0x7,d1
00001a40: 0201 00f8                 andi.b  #-0x8,d1
00001a4e: 0480 0000 0020            subi.l  #0x20,d0
```

These three lines of code align the address of `MC` (`a1`) to a long word, adding 7 and removing the least significant 3 bits. Then the size of the headers (`0x20`) is removed from the total size of the memory region that was passed as an argument.

``` m68k
00001a54: 2149 0010                 move.l  a1,0x10(a0)
00001a58: 2149 0014                 move.l  a1,0x14(a0)
```

Store the first free memory location (`a1`) in `MH_FIRST` (`0x10(a0)`) and in `MH_LOWER` (`0x14(a0)`).

``` m68k
00001a5c: 2209                      move.l  a1,d1
00001a5e: d280                      add.l   d0,d1
00001a60: 2141 0018                 move.l  d1,0x18(a0)
00001a64: 2140 001c                 move.l  d0,0x1c(a0)
```

The size of the free memory is then added to the first free address and stored in `MH_UPPER` (`0x18(a0)`). The size of the free memory is stored in `MH_FREE` (`0x1c(a0)`).

``` m68k
00001a68: 2340 0004                 move.l  d0,0x4(a1)
00001a6c: 4291                      clr.l   (a1)
```

This code creates the memory chunk in the free memory area. The size of the chunk is stored in the `MC_BYTES` field (`0x4(a1)`), and the `MC_NEXT` field is cleared.

``` m68k
00001a6e: 2248                      movea.l a0,a1
00001a70: 41ee 0142                 lea     0x142(a6),a0
00001a74: 6100 fc48                 bsr.w   0x16be
00001a78: 4e75                      rts     
```

The rest of the code prepares the call to the protected version of `Enqueue`, copying the address of the node in `a1` , the absolute address of `MemList` in `a0`, and branching to the subroutine. When `Enqueue` returns, `AddMemList` terminates and returns as well.

# Resources

* Motorola M68000 Family Programmer's Reference Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
* Amiga System Programmers Guide, Abacus ([pdf here](https://archive.org/details/Amiga_System_Programmers_Guide_1988_Abacus))

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.