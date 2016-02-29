Title: Concurrent programming - 2
Date: 2013-02-04 10:35 +0200
Category: Programming
Tags: C, operating systems, concurrent programming
Authors: Leonardo Giordani
Slug: concurrent-programming-2
Version: 2
Series: "Concurrent programming"
Summary:

## Abstract

In the past article I introduced the concept of process and how important it is for our operating system: in this new issue of the Concurrent Programming series we will go on and begin to write multitasking code; we will start from one of the basic concepts of the whole picture, the forking operation. Afterwards, we will start to introduce the problem of process synchronization and communication, which we will deeply face in a later instalment.

## The C standard library

The C standard library, nicknamed libc, is a collection of functions for the C language which has been standardized by ANSI and later by ISO. An implementation of it can be found in any major compiler or operating system, especially Unix-like ones.

In the GNU ecosystem, which Linux distributions are grounded on, the library is implemented by glibc (GNU libc). The library reached version 2.17 in December 2012 and is free software released under LGPL, a less restrictive version of the GPL which most notably allows the use of the library by non-GPL software, even proprietary one.

You can find and navigate glibc sources [here](http://sourceware.org/git/?p=glibc.git;a=summary)

## C language and PIDs 

Processes, like every resource in a computer, are managed by the kernel. This is a part of the operating system that runs continuously in background, exposing functions to other programs that allow them to obtain, manage and release resources such as memory, disk space and CPU time. One of the things the kernel exposes to the hosted programs is the process management, which stands for structures that represent the current status of processes in the system. Using the libc the programmer can easily access those structures in a read-only way.

Let us start with the simple concept of PID, the Process IDentifier we described in the past issue of this series. In the libc, the `pid_t` type is defined as an integer capable of containing a pid. To show this we will now take a little tour of the glibc source code: we will cover the whole path this time only, just to highlight both the chances given us by the source code and the complexity of a project like glibc.

We can find the initial definition of `pid_t` [here](http://sourceware.org/git/?p=glibc.git;a=blob;f=posix/sys/types.h;h=33c2176d0f0b38f1a81bf8c76d38f08d2ab38675;hb=HEAD)

``` c
#ifndef __pid_t_defined
typedef __pid_t pid_t;
#define __pid_t_defined
#endif
```

As you can see the definition of the type is protected by multiple header inclusion, as usual in the C world. The underlying `__pid_t` type is then defined [here](http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/types.h;h=dc7b784f965749c44257bf030508d6642c07dec7;hb=HEAD)

``` c
#define __STD_TYPE typedef
#define __S32_TYPE int
__STD_TYPE __PID_T_TYPE __pid_t; /* Type of process identifications.  */
```

Notice that even the typedef keyword is defined by a macro (remember that the C preprocessor is a little more than a text macro replacement tool); this definition in particular avoids compiler warning when using the `long long` type which is not supported by all versions of the C standard. `S32` stands for “signed 32 bits” as you can read in the code documentation.

The last macro `__PID_T_TYPE` can be found [here](http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/typesizes.h;h=8268b90276700a382db37331be1d8ef5a516403d;hb=HEAD)

``` c
#define __PID_T_TYPE            __S32_TYPE
```

The heavy use of defines and compiler features made by glibc code allows the library to be compiled on very different architectures.

## Telling the PID

Let us now discover the function which give us the knowledge of the pid of the process containing our program

``` c
pid_t getpid (void);
```

which is defined with `pid_t` in `unistd.h` and `sys/types.h`. The libc simply uses the system call called `getpid` and its real implementation can be found in the [kernel source](http://git.kernel.org/?p=linux/kernel/git/torvalds/linux.git;a=blob;f=kernel/timer.c;h=367d008584823a6fe01ed013cda8c3693fcfd761;hb=HEAD)

``` c 
SYSCALL_DEFINE0(getpid)
{
	return task_tgid_vnr(current);
}
```

We can now write a program whose aim is to print on the standard output its own pid. With an editor of your choice write the following code

``` c
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
int main() {
  pid_t pid;

  pid = getpid();
  printf("The pid assigned to the process is %d\n", pid);

  return 0;
}
```
[source code](/code/print_pid.c)

Save the program as print_pid.c and compile it

``` bash
gcc -o print_pid print_pid.c
```

If this operation returns errors make sure you installed gcc and libc headers; refer to your distribution knowledge base to solve this task.

The compiler will build an executable named `print_pid` (since the current directory is probably not in the system path you can run it with `./print_pid`). Executing the program we will have no great surprises: it prints out a positive number and if you executes it more than once this number will likely increase progressively; this is not mandatory, however, because another process could be created between two executions of `print_pid`. Try for example to execute `print_pid`, then `ps` and then `print_pid` again.

## Forking

Now it is time to meet one of the most important concepts in the field of operating systems, that is the concept of forking.

Forking in general means that one thing splits in two and each part acts like an independent being. At the very moment of the split the two entities are the same, exactly the same, except for the fact that they live in two different spaces. This computer science concept can be retrieved in nature when a simple organism generates a clone of itself. After the cloning the two organisms are the exact copy, but they can be told apart since they are two things (the right one and the left one for example). Back to the operating system world we find the very same problem when a process forks; after the fork operation we have two processes with two different PIDs, but the two contain the same program at the same execution point.

So, when a program (running as process A) creates another process (B) the two are identical, that is they have the same code, the memory full of the same data (not the same memory, since the memory regions are different) and the same processor status. From this point on the two can continue in two different ways, just like our cloning organisms, for example depending on the user's input or some other data. The process A is called the **parent** process while B is the **child** process; now you can begin to understand better the name _parent of all the processes_ given to init. The function which creates a new process is

``` c
pid_t fork(void)
```

The number returned is a pid, but it deserves a particular attention. We said that the first process duplicates itself splitting in a parent and a child, which will then execute interlacing themselves with the other running processes, doing different works; but immediately after the duplication which process will be executed, the parent or the child?

Well, the answer is as simple as unuseful: one of the two. The decision about which process has to be executed is taken by a part of the operating system called scheduler, and it pays no attention if a process is the parent or the child, following an algorithm based on other parameters.

If we are writing processes that implement a service (such as serving network connections) we need nothing more: the two processes are the same and they behave the same way, so by cloning the process we just incremented the number of service providers. This is usually done by Web servers such as Apache, which spawns a copy of itself to serve incoming connections.

## Parent and child

Sometimes, anyway, it is fundamental to know what process is in execution: after the fork both processes are at the same execution point (that is just after the `fork()` call) so how can them tell if they are the parent or the child one?

In order to clarify this question let us look at the following algorithm:

1. fork
2. if you are the child then execute ...
3. if you are the parent then execute ...

which could represent in a sort of metalanguage the code of our program. The question can be expressed this way: “You have been cloned. Who are you, the original one or the clone?”

The answer is once again very simple: the `fork()` function returns 0 to the child process and the child's PID to the parent. So it is enough to test if the returned PID is zero and we will know what process is executing the current code. Putting it in C language we obtain

``` c
int main() {
	pid_t pid;
	pid = fork();

	if (pid == 0)
	{
		code of the child process
	}
	else {
		code of the parent process
	}
}
```

It's time to write the first real example of multitasking code: you can save it in a `fork_demo.c` file and compile it as we did before.

The program will fork itself, and both the parent and the child will write something on the screen; the final output will be the interlaced output of the two (if all goes right).

``` c
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main() {
	pid_t pid;
	int i;
  
	pid = fork();
  
	if (pid == 0){
		for (i = 0; i < 8; i++){
			printf("-child-\n");
		}
		return 0;
	}
  
	for (i = 0; i < 8; i++){
		printf("+parent+\n");
	}

	return 0;
}
```
[source code](/code/fork_demo.c)

Lines number 01-03 contain the includes for the necessary libraries (standard I/O, multitasking).

The main function returns an integer (as always in GNU), which normally is zero if the program reached the end without errors or an error code if something goes wrong; let us state this time all will run without errors (we can later add some error control code, when the basic concepts will be clear).

Then we define the data type containing a pid (line 06) and an integer working as counter for loops (line 07). At line 09 we call `fork()` and it will return zero to the program executed in the child process and the pid of the child process to the parent; the test is at line 11. Now the code at lines 12-15 will be executed in the child process while the rest (lines 18-22) will be executed in the parent. Note that we omitted the `else` statement since the child code ends with a `return`; thus there is no way for the child to execute the code of the parent.

The two parts simply write 8 times on the standard output the string `-child-` or `+parent+`, depending on which process executes it, and then end returning 0. Executing the program will perhaps let you unsatisfied: the result is likely not to be a real mix between the two strings, and this due to the speed of execution of such a short loop. The scheduler runs the parent or the child for a quantum of time which is enough for the process to end the loop; thus probably your output will be a list of `+parent+` strings followed by a list of `-child-` ones, or the contrary.

To highlight the interlacing effect we can insert a random delay before each `printf()` call: we can do this with `sleep()` and `rand()` functions

``` c
sleep(rand()%4)
```

this make the program sleep for a random number of seconds between 0 and 3. Now the child code looks like

``` c
for (i = 0; i < 8; i++){
	sleep (rand()%4);
	printf("-child-\n");
}
```

and the same for the parent's code. Save it as `fork_demo2.c`, compile and execute. It is slower now, and we notice a difference in the output string order, and more differences between an execution and the previous one. For example my output is

``` bash
$ ./fork_demo2
-child-
+parent+
+parent+
-child-
-child-
+parent+
+parent+
-child-
-child-
+parent+
+parent+
-child-
-child-
-child-
+parent+
+parent+
```

As always when dealing with multitasking code, your output is probably different from mine and will also be different between consecutive executions. Try also to change the parameters of the program, such as the sleep time or the number of loops.

## Why forking

Thinking carefully about the parent and child example a question arises: why should I write code that forks, with all the intricacies it entails, when I could simply write two different programs and run them, letting the operating system to deal with their simultaneous execution?

This is a very interesting question, and the answer is very simple, though the underlying concept is rather complex. When you run a program the shell or the GUI follows a pattern called **fork-and-exec**: first it forks, then the child process executes the desired program. The reason of this behaviour is that the `exec` family of system calls replace the running program with a new program but do not change the PID. So the tasks are divided between system calls: `fork` creates a new process while `exec` loads the given program into the newly created process.

## DOS and Windows

Operating systems of the DOS and Windows family do not provide a `fork` mechanism but a `spawn` family of system calls, which directly implement the fork-and-exec technique; this is a good example of the different implementation OSs make of common computer science concepts: while both implement multitasking and let many processes run concurrently, the way this target is reached can vary significantly.

## Conclusion

Starting from the knowledge of processes you stepped into the fundamental Unix concept of forking, learning to tell apart parent and child processes and running your first multitasking-based program. You also started to understand how complex can some operating system issues be, and that real world OSs can provide very different solutions to them.

In the next article we will start to look after processes synchronization and data sharing between processes.

## Previous article

[Concurrent Programming 1](/blog/2013/01/31/concurrent-programming-1)

## Next article

[Concurrent Programming 3](/blog/2013/02/06/concurrent-programming-3)
