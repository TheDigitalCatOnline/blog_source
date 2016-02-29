Title: Concurrent programming - 3
Date: 2013-02-06 12:25 +0200
Category: Programming
Tags: C, operating systems, concurrent programming
Authors: Leonardo Giordani
Slug: concurrent-programming-3
Series: "Concurrent programming"
Version: 2
Summary:

## Abstract

In the first article, we stepped into the world of multitasking, going over its meaning and the reasons behind its existence. In the second article, we met the fundamental fork operation and wrote our first multitasking code. In this article, we will go ahead and introduce ourselves in the topic of synchronization: the problem we face now indeed is to release the full power of multitasking, that is to share the work between processes.

In the first part of the article, I want to make a step back to the basics of multitasking to clarify the single/multiple CPU theme. Then we will start talking about shared data between processes, first looking at the problems arising and then writing some code.

## Simultaneous execution

We should never forget that the main reason behind multitasking on a single CPU is to give the impression of simultaneous execution, not to speed up execution.

Let's clarify this statement. A single CPU can execute only one operation at a time, so running two processes means to run the first one for a little time, then switch to execute the second one and so on. As already stated, this interlacing operation is what we call multitasking, and it is clear that such a technique slows down the interlaced processes: since each of the two processes runs for half the time (the other half being spent by the other process), the result is a process that runs half the speed.

If you add to this that a switch operation does not come for free, because some time is spent by the operating system replacing the current process with the other one, you suddenly realize that multitasking is a double-edged sword: on one side, it gives us the chance to run multiple programs at the same time, on the other side, it slows down the execution of the whole system.

This issue can only be solved by using multiple CPUs or multiple cores (the two are not exactly the same), either on the same physical machine or on different ones.

So, if you are writing an algorithm to process an image in foreground, that is for a user that is waiting in front of the screen, you could split it profitably in many parts, each of which will process a tile of the whole image. This way the user could have a glance at the whole result while it is being computed. The same technique counts if, for example, you are computing statistics about a set of data: since some computations last longer than others you could split them among different processes so that the user can immediately obtain the faster ones.

If, on the contrary, you are writing code that runs in background without the user being there waiting for the results, you should never split it in different processes if you are working on a single CPU. That way you are just tangling up the code and wasting the CPU cycles to switch between processes without any real benefit.

## Working together

Being our processes running at the very same time on different CPUs or just interlacing on a single one, a completely new set of problems arises if the two have to work on the same data.

Just imagine this very common scenario: two processes share a common storage area in memory where partial results of their computations can be temporarily saved. So each process writes data on the storage area, then runs computing other data and last loads previously saved data to finalize its work. What happens if, in the meantime, the second process wrote some of its temporary data  on the same area? The first process has no way to figure out that the loaded data is not correct, and this can lead to catastrophic results. The worst thing about such a bug, as quite always in the multitasking environment, is that such a problem can arise or not, depending on input, times, status of the running machine and so on. This class of problems is usually known as race condition and is one of the most subtle and dangerous bug forms since it is difficult to spot, to reproduce and sometimes to test automatically.

A little search about the Therac-25 accident (1987) can clearly exemplify the extent of the problem that such a bug can cause.

## Basic synchronization

The simplest form of synchronization is waiting; that is, a process sits down until another process finishes running, then runs its own code. The system call that allows a process to wait for another to end is

``` c
pid_t waitpid (pid_t PID, int *STATUS_PTR, int OPTIONS)
```

where `PID` is the PID of the process we are waiting for, `STATUS_PTR` a pointer to an integer which will contain the status of the child process (`NULL` if the information is not needed) and `OPTIONS` a set of options we have not to care about for now.

This is an example of a program in which the parent creates a child process and waits until it ends up

``` c
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main()
{
  pid_t pid;
  int i;
  
  pid = fork();
  
  if (pid == 0){
    for (i = 0; i < 8; i++){
      printf("-child-\n");
    }
    return 0;
  }

  printf("+parent+ Waiting for the child to terminate...\n"); 
  waitpid (pid, NULL, 0);
  printf("+parent+ ...ended\n");

  return 0;
}
```

Let's save the code as `fork_demo3.c`, compile it and execute. The output I get from this program is the following

``` bash
$ ./fork_demo3
+parent+ Waiting for the child to terminate...
-child-
-child-
-child-
-child-
-child-
-child-
-child-
-child-
+parent+ ...ended
```

Looking at it you can realize a very important thing about `waitpid()`: it is a so-called **blocking system call**; that is something that keeps running until a certain condition is met. In this case, `waitpid()` keeps doing nothing until the child ends. So waiting is a blocking synchronization technique.

## Relationships

Speaking about parents and children there are two more type of processes we can find in the system: **orphans** and **zombies**. A process is orphaned when the parent terminates while it is still running, according to the real meaning of the word orphan; in such a case the orphan process is adopted by `init`, the first process in the system, thus becoming one of its children.

Zombie processes, on the contrary, are already terminated, but they have not been waited by the parent. In other words, they are not running, but the operating system still holds their data to allow the parent to collect the exit status.

A simple demonstration of the zombie status can be done with the following code

``` c
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main()
{
  pid_t pid;
  int i;
  
  pid = fork();
  
  if (pid == 0){
    printf("-child-\n");
    return 0;
  }

  for (i = 0; i < 14; i++){
    printf("+parent+\n");
    sleep(2);
  }

  return 0;
}
```

Save it as `zombie_demo.c`, compile and run. Just after the child has printed its string open another terminal and run `ps xa`. The last lines of your output will be something like

``` bash
 4399 pts/1    S+     0:00 ./zombie_demo
 4400 pts/1    Z+     0:00 [zombie_demo] <defunct>
```

As you can see the first process listed is the parent, while the second, the child, is marked as defunct and its status is zombie (Z).

## Conclusion

Multitasking is a fundamental technique, which must be however used with care; as any other technology, it cannot solve every problem, neither its introduction comes completely for free.

Synchronization between processes is something which must be carefully considered and implemented since errors in this field can lead to devious bugs.

Last you met two new types of processes, orphans and zombies, and learned why they can be found in the system.

In the next article, we will study some classical synchronization problems and introduce the basic InterProcess Communication (IPC) structures and techniques Unix systems make available to programmers.

## Previous article

[Concurrent Programming 2](/blog/2013/02/04/concurrent-programming-2)

## Next article

[Concurrent Programming 4](/blog/2013/02/13/concurrent-programming-4)
