Title: Exploring the Amiga - Part 8
Date: 2019-02-19 14:00:00 +0100
Category: Retro
Tags: assembly, amiga, retroprogramming
Authors: Leonardo Giordani
Slug: exploring-the-amiga-8
Series: Exploring the Amiga
Image: exploring-the-amiga-8
Style: retro
Summary: 

# Branching

As branching is one of the most important ways to control the program flow it is worth discussing how it works in the Motorola 68000 processor, and review what options we have.

Contrary to what happens in higher level languages, in Assembly we do not have a way to represent logical expressions. In a language like C we can compare two integers with something like `a > b` which tells us if the first number is greater than the second. But in Assembly there is no such syntax, so we have to rely on processor flags; these are bits that the processor sets according to the result of some operation, be it a direct comparison of two values or another instruction that manages integer numbers.

The Motorola 68000 flags are kept in the 5 least significant bits of the Status Register (SR). The latter is not available during the normal operations in user mode, but these 5 bits, called Condition Code Register (CCR) are always accessible. The 5 bits are named `X`, `N`, `Z`, `V`, and `C`. Of these, the ones we need to take into consideration for branching are the rightmost 4.

* `N` (Negative) is set to the value of the most significant bit of the result of an instruction, to show that a negative number was produced.
* `Z` (Zero) is set if the result is zero.
* `V` (Overflow) is set when an arithmetic operation results in a number that cannot be represented with the size of the operand.
* `C` (Carry) is set when an addition or a subtraction need to carry a bit (carry out or borrow).

To branch according to the value of these flags we can use the `Bcc` family of instruction, where `cc` is a two-letter mnemonic that represents the condition that is under test. For example the condition `MI` stands for `MInus` and tests if the `N` flag is set. Thus, `bmi <address>` will branch to `<address>` if the result of the previous instruction was negative, because that instruction set the `N` flag.

There are multiple families of instructions that use the CCR: `Bcc`, `DBcc`, `Scc`, and `TRAPcc`. The following table lists all the possible conditions we can test on the Motorola 68000 and their meaning

| Instruction | Full name        | Tested condition  | Notes |
|-------------|------------------|-------------------|-------|
| CC          | Carry Clear      | C == 0            |       |
| CS          | Carry Set        | C == 1            |       |
| EQ          | EQual            | Z == 1            |       |
| F           | False            | Always false      | Not available for Bcc |
| GE          | Greater or Equal | N == V            |       |
| GT          | Greater Than     | N == V and Z == 0 |       |
| HI          | HIgher than      | C == 0 and Z == 0 |       |
| LE          | Less or Equal    | Z == 1 or N != V  |       |
| LS          | Lower or Same    | C == 1 or Z == 1  |       |
| LT          | Less Than        | N != V            |       |
| MI          | MInus            | N == 1            |       |
| NE          | Not Equal        | Z == 0            |       |
| PL          | PLus             | N == 0            |       |
| T           | True             | Always true       | Not available for Bcc |
| VC          | V Clear          | V == 0            |       |
| VS          | V Set            | V == 1            |       |

The reason why the `T` and `F` conditions are not available for `Bcc` instructions is simple: `bt` would mean "branch always", and the `bra` instruction does exactly this, while `bf` would mean "never branch" and it's arguably useless.

Let's see an example of use with a standard comparison instruction: `cmp`

``` m68k
cmp.w   d0,d1
beq.b   0x1522
```

Here, the processor compares `d0` and `d1` computing the subtraction `d1 - d0` and setting the CCR flags according to the nature of the result. The processors manual (Programmer's Reference Manual, section 4-75, page 179) tells us that the flags are affected according to these rules

* `X` — Not affected.
* `N` — Set if the result is negative; cleared otherwise.
* `Z` — Set if the result is zero; cleared otherwise.
* `V` — Set if an overflow occurs; cleared otherwise.
* `C` — Set if a borrow occurs; cleared otherwise.

The `beq` instruction on the second line checks only the `Z` flag, however, so what we are checking here is if `d0` and `d1` have the same value.

A less straightforward example involves `move`

``` m68k
move.w  0x1c(a1),d0
beq.b   0x151e
```

The first line moves the value at `a1 + 0x1c` into `d0`. The `move` instruction page (Programmer's Reference Manual, section 4-116, page 220) says that it affects only the `N` and `Z` flags

* `X` — Not affected.
* `N` — Set if the result is negative; cleared otherwise.
* `Z` — Set if the result is zero; cleared otherwise.
* `V` — Always cleared.
* `C` — Always cleared.

Here, the tested result is the value that has been moved. So, the next line branches only if the address `a1 + 0x1c` (and later `d0`) contains a zero.

Pay attention that `GE`, `GT`, `LE`, and `LT` read `N`, so they should be used only with signed integers Unsigned integers, instead, should be compared using `HI` and `LS`.

# `Enqueue`

In the previous article we discussed the structure of a memory node created by `AddMemList` and I briefly mentioned `Enqueue` and a protected version of it. `Enqueue` is the function that Exec uses to add a node into a linked list, following a simple schema based on priorities, but it turns out `AddMemList` does not use it directly, resorting to a version of it wrapped by two other functions, namely `Forbid` and `Permit` that are connected to task rescheduling. For the time being, however, we may forget the wrappers and jump directly to `Enqueue` and learn how the Amiga operating system was managing linked lists.

If the list we are managing is a singly linked list, where each node points to the successor only, we are forced to find the node _after which_ we want to insert the new one. This is mandatory, as we have to change its outgoing pointer, redirecting it to the new inserted node. In a doubly linked list such the ones used by the Amiga system this is not a problem, as from any point in the list we can start traversing it either forward or backward.

The following drawing is a simple representation of what happens in the Kickstart code when the `Enqueue` function is executed. `Ins` is the node we want to insert, `Pred` is the node after which we will insert `Ins`, and `Next` is the node before which we will insert `Ins`.

``` c
                                  TAILPRED  TAIL
                                     |       |
                                     v       v
BEFORE:       HEAD -> Node -> Pred -> Next -> Node -> 0x0

              
                                         TAILPRED  TAIL
                                            |       |
                                            v       v
AFTER:        HEAD -> Node -> Pred           Next -> Node -> 0x0
                                  |         ^
                                  |         |
                                  +-> Ins --+
```

As you can see there are several pointers that need to be changed. Both the `LN_SUCC` of `Pred` and `Ins`, but also the `LN_PRED` of `Next` and `Ins`.

`Enqueue` has a very simple prototype

``` text
Enqueue(list, node)
        aO    a1
```

where `a0` and `a1` are pointers respectively to the list header and to the node we are going to insert. It is worth recalling how the list header status is at the time when the first insertion happens, that is when either the chip or expansion memory are added to the system memory pool, managed through `MemList`. The latter is a list header `LH` structure and the actual values are the following

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

With this structure in mind, let's dive into the source code of `Enqueue`. The function is defined at `0xfc1670`

``` m68k
00001670: 1229 0009                 move.b  0x9(a1),d1
00001674: 2010                      move.l  (a0),d0
00001676: 2040                      movea.l d0,a0
00001678: 2010                      move.l  (a0),d0
0000167a: 6706                      beq.b   0x1682
0000167c: b228 0009                 cmp.b   0x9(a0),d1
00001680: 6ff4                      ble.b   0x1676
00001682: 2028 0004                 move.l  0x4(a0),d0
00001686: 2149 0004                 move.l  a1,0x4(a0)
0000168a: 2288                      move.l  a0,(a1)
0000168c: 2340 0004                 move.l  d0,0x4(a1)
00001690: 2040                      movea.l d0,a0
00001692: 2089                      move.l  a1,(a0)
00001694: 4e75                      rts     
```

And it can be roughly divided into three sections, according to the internal jumps.

``` m68k
Init:
00001670: 1229 0009                 move.b  0x9(a1),d1
00001674: 2010                      move.l  (a0),d0

FindPos:
00001676: 2040                      movea.l d0,a0
00001678: 2010                      move.l  (a0),d0
0000167a: 6706                      beq.b   InsertNode
0000167c: b228 0009                 cmp.b   0x9(a0),d1
00001680: 6ff4                      ble.b   FindPos

InsertNode:
00001682: 2028 0004                 move.l  0x4(a0),d0
00001686: 2149 0004                 move.l  a1,0x4(a0)
0000168a: 2288                      move.l  a0,(a1)
0000168c: 2340 0004                 move.l  d0,0x4(a1)
00001690: 2040                      movea.l d0,a0
00001692: 2089                      move.l  a1,(a0)
00001694: 4e75                      rts     
```

The first section, `Init`, prepares the execution of the rest of the function. The rest bla

As I did for other functions in the previous articles, I'm going to dissect this line by line. Let's start from the `Init` part.

## Init

``` m68k
Init:
00001670: 1229 0009                 move.b  0x9(a1),d1
00001674: 2010                      move.l  (a0),d0
```

Since `a1` points to the node to be inserted, `0x9(a1)` is the `LN_PRI` field of that node, that is the priority. The first line thus stores it in `d1` because it will be used to search for the insertion point, as the list is maintained in priority order.

The second line is the core or the node traversal algorithm. Since `a0` points to a `LH` structure, it is also the address of the first field `LH_HEAD`. `(a0)`, thus, is the dereferencing of that address, which means the address of the first node in the list. Given the figures I showed before for `MemList`, `a0` is `0x142` so a `move a0,d0` would store `0x142` (the value of `a0`) in `d0`. `(a0)`, instead, is the content of that address in memory, namely `0x146`, so `move (a0),d0` stores `0x146` (the content of `0x142`) in `d0`.

Putting aside the intricacies of the addressing modes, the second line stores the address of the first node in the list, which is the part of the header that starts with `LH_TAIL`. The header thus acts as the first node of the list.

## FindPos

``` m68k
FindPos:
00001676: 2040                      movea.l d0,a0
00001678: 2010                      move.l  (a0),d0
0000167a: 6706                      beq.b   InsertNode
0000167c: b228 0009                 cmp.b   0x9(a0),d1
00001680: 6ff4                      ble.b   FindPos
```

The first two lines repeat the same algorithm. The address of the current node (`d0`) is moved to `a0` and the dereferencing operation `(a0)` puts in `d0` the address of the following node. The reason why we do it in two lines is that the Address Register Indirect Mode can be used only with `An` registers. At this point `d0` contains the address of the second node, the successor of the header.

If the list contains at least one node, `d0` contains its address. But if the list is empty at this point `d0` contains `0x0`, and this is the condition tested by the `beq.b` instruction. If `d0` is empty we reached the end of the list, which means that there was no better place to insert the node, and we jump to the actual node insertion code, `InsertNode`. If the value is not zero, the current node has a proper successor, so let's check it's priority to see if we need to go on or if we can stop here. The code compares the priorities of the current node and of the `Ins` node, and if the latter is less than the former we can loop back to `FindPos` and move to the next node. Remember that priorities are expressed with negative numbers only, so "less than" actually means "higher priority".

## InsertNode

``` m68k
InsertNode:
00001682: 2028 0004                 move.l  0x4(a0),d0
00001686: 2149 0004                 move.l  a1,0x4(a0)
0000168a: 2288                      move.l  a0,(a1)
0000168c: 2340 0004                 move.l  d0,0x4(a1)
00001690: 2040                      movea.l d0,a0
00001692: 2089                      move.l  a1,(a0)
00001694: 4e75                      rts     
```

In either case, when we reach the tail or when the priority of the next node is lower than the one of the new node, we reach `InsertNode`. At this point `a0` points to `Next` and we can access `Pred` through `0x4(a0)` (that is `LN_SUCC` of `Next`).

``` m68k
00001682: 2028 0004                 move.l  0x4(a0),d0
00001686: 2149 0004                 move.l  a1,0x4(a0)
```

This first stores the aforementioned address of `Pred` in `d0`, then replaces it with the value of `a1`. The result is that the predecessor of `Next` becomes `Ins`.

```m68k
0000168a: 2288                      move.l  a0,(a1)
0000168c: 2340 0004                 move.l  d0,0x4(a1)
```

This moves `a0`, the address of `Next`, into the first field of `Ins`, that is `Next` becomes the successor of `Ins`. The second line moves `d0` (the address of `Pred`) into the `LN_PRED` of `Ins`.

``` m68k
00001690: 2040                      movea.l d0,a0
00001692: 2089                      move.l  a1,(a0)
00001694: 4e75                      rts     
```

Last, the address of `Ins` becomes the `LN_SUCC` of `Pred`, so we move `d0` into `a0` because, as I already mentioned, the Address Register Indirect Mode can be used only with `An` registers. After this the function returns to the caller.

# `Remove`

Since we reviewed the code of `Enqueue` it makes sense to have a look at the opposite function, `Remove`. A protected version of this exists as well, but here I will just show the code of the pure function without the wrapper.

Removing a node is simpler than adding it, as all we have to do is to make `Pred` point to `Next` and vice versa, so the function is much shorter. It's worth noting that while the function accepts the address of the node in `a1` the value in this register is eventually overwritten, so the address has to be kept elsewhere when the function is called.

``` m68k
0000163c: 2051                      movea.l (a1),a0
0000163e: 2269 0004                 movea.l 0x4(a1),a1
00001642: 2288                      move.l  a0,(a1)
00001644: 2149 0004                 move.l  a1,0x4(a0)
00001648: 4e75 
```

The first two lines store the address of `Next` in `a0` and of `Pred` in `a1` (overwriting the input value). The third line makes `Next` the successor of `Pred` and the fourth line makes `Pred` the predecessor of `Next`. Then the function returns to the caller.

# `AddLibrary`

In the [6th post]({filename}exploring-the-amiga-6.markdown) of this series we left Kickstart just after it finished adding the physical memory to the system pool. The last instruction we mentioned was a call to `AddLibrary`, and this is then the next function I will explore.

The code of the function is at `fc1448`, and required some work before I was able to read it (see the section "Manual decompilation").

``` m68k
00001448: 41ee 017a                 lea 0x17a(a6),a0
0000144c: 6100 0270                 bsr.w 0x16be
00001450: 6100 0082                 bsr.w 0x14d4
00001454: 4e75                      rts     
```

The first line loads the absolute address of an object `0x17a` bytes after the ExecBase address, and looking up this displacement in the library structure published in both the fifth and sixth instalment we find, rather unsurprisingly, that this is the address of `LibList`.

The following lines call the protected version of `Enqueue` and `SumLibrary`, after which the routine returns to the caller. The call to `Enqueue` follows what I explained at the beginning of this post, where this time `a0` points to the library system list and `a1` points to the base address of Exec, set just before the call to `AddLibrary`. So the Exec library itself is added to the system libraries through this routine.

`SumLibrary`, as the name suggests, computes the checksum of a library, or checks the existing one.

# Manual decompilation

When I was following the call to `AddLibrary` from the main body of Kickstart I was surprised to find this code

``` m68k
00001446: 0000 41ee                 ori.b   #-0x12,d0
0000144a: 017a 6100                 bchg    d0,0x754c(pc)
0000144e: 0270 6100 0082            andi.w  #0x6100,(-0x7e,a0,d0.w)
00001454: 4e75                      rts     
```

which at first glance doesn't look like a basic function, as the instructions are too convoluted. Furthermore, the branch uses the address `0x1448`, which is thus supposed to be the beginning of the function. A quick look at the hexadecimal values reveals the truth: at `0x1446` Kickstart contains a padding word `0000` that confused the decompiler. To see the code of the function I had to manually decompile the machine code, and since the process is very interesting I decided to show it in detail here.

When you try to manually decompile some machine code you need a cheat sheet and the processor manual (see the resources section), which can help you to quickly track down the meaning of the single bits. The values we are interested in are

``` m68k
00001448: 41ee
0000144a: 017a
0000144c: 6100
0000144e: 0270
00001450: 6100
00001452: 0082
00001454: 4e75
```

since `fc1456` is listed as the address of the `RemLibrary` function. THe binary representation of these values is

``` m68k
00001448: 0100 0001 1110 1110
0000144a: 0000 0001 0111 1010
0000144c: 0110 0001 0000 0000
0000144e: 0000 0010 0111 0000
00001450: 0110 0001 0000 0000
00001452: 0000 0000 1000 0010
00001454: 0100 1110 0111 0101
```

Now, the first 4 bits of any Motorola 68k instruction are the instruction code. While multiple instructions share the same 4 bits (for example `andi` and `subi`), those bits are never used for addressing or to specify modes, so they are a good starting point.

The instruction at `0x1448` starts with `0100` followed by a `0`, and this narrows the selection to either `lea` or `chk`. Both instructions use the following 3 bits to specify a register (`An` for `lea`, `Dn` for `chk`), but then the first has a fixed group `111`, while the second has a group `110`. This means that we are looking at a `lea`. The three bits I mentioned are `000`, which translates in `a0` as a target. The last 6 bits are the addressing mode and the register, and the cheat sheet tells us that `101 110` corresponds to Address Register Indirect with Displacement Mode on `a6`. `101` is labelled as `(d16, An)`, while `110` is the number 6. The following word is thus the `d16` displacement from `a6`, which means that `41ee 017a` translates to ` lea 0x17a(a6),a0`.

The second instruction starts at `0x144c` with `0110`, which is the signature of all the branch commands: `bra`, `bsr`, and all the condition-based ones like `bgt`, `blt`, and so on. Since the following 4 bits are `0001` we know this is a `bsr`, branch to subroutine. Now, in this instruction the 8 least significant bits tell us what the displacement is, and thus the type of the operand (Programmer's Manual, section 4-59, page 163). In this case they are all `0`, which means a word displacement, which is in the next 16 bits. Pay attention that, as we discussed for `lea` in the [first post]({filename}exploring-the-amiga-1.markdown) the Program Counter contains the address of the first displacement word. In this case the displacement is `0x270` at address `0x144e`, so the branch address is the sum of the two, that is `0x16be`.

The third instruction is again a `bsr` and following the same process we find out that the branch address is the sum between `0x82` and `0x1452`, that is `0x14d4`. The final instruction is an `rts`, as we expected, and as the decompiler correctly told us.

The most important thing to keep in mind when manually reading instructions is the position of the Program Counter. As we already saw two times, with `lea` and with `bsr`, the PC moves as soon as the 16-bit instruction has been read, which means that when a displacement is given we have to use the address of the displacement itself as a base for our calculations.

# Resources

* Motorola M68000 Family Programmer's Reference Manual [PDF here](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
* Motorola 68000 Opcodes Cheat Sheet http://goldencrystal.free.fr/M68kOpcodes-v2.3.pdf

# Feedback

Feel free to reach me on [Twitter](https://twitter.com/thedigicat) if you have questions. The [GitHub issues](http://github.com/TheDigitalCatOnline/thedigitalcatonline.github.com/issues) page is the best place to submit corrections.