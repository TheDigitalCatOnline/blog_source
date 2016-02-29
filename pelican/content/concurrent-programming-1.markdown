Title: Concurrent programming - 1
Date: 2013-01-31 16:47 +0200
Category: Programming
Tags: C, operating systems, concurrent programming
Authors: Leonardo Giordani
Slug: concurrent-programming-1
Version: 2
Series: "Concurrent programming"
Summary:

## Abstract

_I already published this series of articles on concurrent programming on LinuxFocus from November 2002 and now I carefully reviewed them. Feel free to notify me any error you will find._

The purpose of this article (and the following ones in this series) is to introduce the reader to the concept of multitasking and to its implementation in the Linux operating system. Thus, we will start from the theoretical concepts at the base of multitasking and from those we will move towards advanced concept like multithreading and interprocess communication. We will practice some code and some Linux internals along this path, and I will try constantly to exemplify how real world operating system realize the concepts I am introducing.

## Prerequisites

Prerequisites for the understanding of the article are:

* Minimal knowledge of the text shell use (bash)
* Basic knowledge of C language (syntax, loops, libraries)

Even if I wrote the articles using a Linux machine, any Unix-like operating system, most notably Mac OS X, shares the concepts I will present. However, pay attention to the fact that while any Unix-like system behaves the same way (more or less), its internal structures and code can be significantly different. On the other hand OSs of the DOS and Windows family share some concepts with Unix-like ones but behave very differently.

One of the biggest positive aspects of dealing with an open source operating system like Linux is the possibility to look at the code: this allow us to uncover the internal working of a system that runs on the most important data center of the planet.

## Introduction

One of the most decisive turning points in the history of operating systems was the concept of multiprogramming, a technique for interlacing the execution of several programs in order to gain a more constant use of the system's resources. Let us think about a simple workstation, where a user can execute at the same time a word processor, an audio player, a print queue, a web browser and more, and we can understand the importance of this technique. As we will discover this little list is only a minimal part of the set of programs that are currently executing on the machine, even though they are the most visible ones.

## The concept of process

In order to interlace programs, a remarkable change of the operating system is necessary; in order to avoid conflicts between running programs an unavoidable choice is to encapsulate each of them with all the information needed for their execution.

Before we explore what happens in the operating system let us give some technical nomenclature: given a running program, at a given time the **code**
is the set of instructions which it is made of while the **memory space** is the part of machine memory taken up by its code and data. The **processor status** is the value of the microprocessor's parameters, such as the registers, the flags or the Program Counter (the address of the next instruction to be executed).

Given a tern of objects made of code, memory space and processor status we can define a **running program**. If at a given time during the operation of the machine we save this information and replace them with the same set taken from another running program, the flow of the latter will continue from the point at which it was stopped: doing this once with the first program and once with the second realizes the interlacing previously described. We name **process** (or **task**) the tern of objects we used, a concept which has to do with that of program, yet being an extension of it.

Let us explain what was happening to the workstation we spoke about in the introduction: only one task is in execution at each moment (there is only one microprocessor with just one core and it cannot do two things at the same time), and the machine executes part of its code. After a certain amount of time named **quantum** the running process is suspended, its tern of objects is saved and replaced by those of another waiting process, whose code will be executed for a quantum of time, and so on. This mechanism is what we call multitasking (or multiprocessing).

As stated before the introduction of multitasking causes a set of problems, most of which are not trivial, such as the management of the waiting processes queues (called **scheduling**); nevertheless those problems have to do with the architecture of each operating system, and are not the topic of the current article.

## Processes in Linux and Unix

Let us discover something about the processes running on the machine. The command which gives us such information is ps which is an acronym for Process Status. Opening a normal text shell and typing the ps command we will obtain an output such as

``` bash
$ ps
 PID TTY          TIME CMD
2241 ttyp4    00:00:00 bash
2346 ttyp4    00:00:00 ps
```

I state in advance that this list is not complete, but let us concentrate ourself on this for the moment: ps has given us the list of each process running on the current terminal. We recognize in the last column the name the process is started with (such as "firefox" for Mozilla Firefox and "gcc" for the GNU Compiler Collection). Obviously "ps" appears in the list because it was running when it compiled the list of running processes. The other listed process is the Bourne Again Shell, the shell running on the terminal.

Let us leave out for the moment the information about TIME and TTY and let us look at PID, the Process IDentifier. The pid is a strictly positive number (not zero) the operating system assigns univocally to each running process; once the process terminated the pid can be reused, but the OS guarantees that the pid of a process remains the same during its execution and that it is unique. All this implies that the output each of you will
obtain from the ps command will probably be different from that in the above example.

Just to check this latter sentence, let us open another shell without closing the first one and type the ps command: this time the output gives the same list of processes but with
different pid numbers, showing that they are two different processes even if the program they executes is the same.

We can also obtain a list of all processes running on a Linux box: the ps command man page says that the switch -e means "select all processes". Let us type "ps -e" in a terminal and ps will print out a long list formatted as previously seen. In order to analyze in a comfortable way this list we can redirect the output of ps in the ps.log file:

``` bash
$ ps -e > ps.log
```

Now we can read this file editing it with our preferred editor (or simply with the less command); as stated at the beginning of this article the number of running processes is higher than we would expect; on the machine I'm currently writing on they are around 170.

That list contains not only processes started by us (through the command line or our graphical environment of choice), but also a set of processes, some of which with strange names: the number and the identity of the listed processes depends on the configuration of your system, but there are some common things.

First of all, no matter how you configured the system, the process with pid equal to 1 is always "init", the so called father of all the processes. Wait, wait, I hear you screaming “What about fatherhood among processes?”. This concept will become clear later, when we will talk about forking and the way the system spawns new processes. By now just note that the fact that init owns the pid number 1 tells us that it is the first process executed by a Unix-like operating system.

Another thing we can easily note is the presence of many processes, whose name ends with a "d": they are the so called "daemons" and are some of the most important processes of the system. Since this is just an introductory article we will not diffusely talk about deamons now: just say they are programs that sit waiting for specific events and when one of those events occur the daemons waiting for it perform some actions. A typical example of a daemon is sshd, which establishes or refuses SSH connections upon external requests.

The existence of daemons makes even clearer why multitasking is vital for a modern operating system: since there are many peripherals and connections to manage, through multitasking we can run many small specialized programs (call them deamons, services, components), each of which knows only how to manage that specific object. This is a typical concept of the Unix world, summarized by the sentence “do one thing and do it well”.

## Conclusion

This article introduced you to the meaning of multitasking and to the reason of its implementation. You started to familiarize with the concept of process and the related jargon and discovered a little about the internal working of the operating system by looking at all the processes running in it. 

Next article will dig into the fundamental concept of process forking, both from the theoretical point of view and with real C examples. We will also later talk about interprocess communication, multithreading, multiple CPUs and cores and scheduling.

Feel free to ask questions or point errors through comments or social networks.

## Next article

[Concurrent Programming 2](/blog/2013/02/04/concurrent-programming-2)
