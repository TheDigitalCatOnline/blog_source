Title: Concurrent programming - 5
Date: 2013-02-28 17:49 +0200
Category: Programming
Tags: C, operating systems, concurrent programming
Authors: Leonardo Giordani
Slug: concurrent-programming-5
Series: "Concurrent programming"
Summary:

## Abstract

In the past articles we introduced the concept of concurrent programming and studied a first solution to the problem of intercommunication: semaphores. As explained, the use of semaphores allows us to manage access to shared resources, and this is a first and very simple form of synchronization. In this issue, we will go one step further introducing a richer tool for concurrent programming: _message queues_.

## Limits of semaphores

Ruling access to a resource through semaphores is a good form of synchronization, even if it is very limited. Indeed it prevents two or more processes simultaneously modifying shared data but does not allow them to exchange information.

Moreover, the basic use of semaphores does not guarantee the absence of dangerous situations. As an example remember that, in a multitasking environment, we do not know in advance which process will be executed and when. This means that a resource could be inaccessible for a process since it is always blocked by other processes.

Thus, while being a good tool in the multitasking programmer's bag, semaphores are not the final solution to his or her problems. So we now introduce a new tool which plays a big role in multitasking and distributed systems called **message queues**.

## Message queues theory

Since message queues are one of the most important structures in computer science, let us dive into them at a slow pace. So first of all, what is a **message** in this context? Simply stated, a message is some **data** with a given **format**, which is a complex form to say everything. Indeed in the context of communication between processes, or computers, a message is a stream of bits which must be formatted according to a previously agreed format, where format means a set of rules the stream of bit must obey.

For example, we could simply state that a message is a raw extended [ASCII](http://en.wikipedia.org/wiki/ASCII) string: this means that the stream must contain a multiple of 8 bits (since each extended ASCII character is represented by 8 bits). Our format is very simple, in this case, but it is enough to state that a message made of 377 bit is invalid. We can also make a message encompass entire files, and there is in general no limit to the size.

Now we have a definition of message. What is a **queue**? A queue is a list structure, where a given number of _homogeneous_ objects can be stored. An object can be inserted in the list (**push**ed) and extracted from it (**pop**ped). Usually a queue in the messaging environment is a FIFO (First In First Out) one, meaning that the first object pushed is the first object popped. There is a plenty of mathematical and computer science theory about queues, buffers, FIFO, LIFO and other list types, and we will not review such topics here. Just a notice, to open further research by the reader: the stack, one of the most important concepts in programming, is also a memory buffer, or a queue, where the computer stores function calls and local variables.

So message queues are buffers where arbitrary (but homogeneous) objects called messages can be stored for a later retrieval.

Every process can create one or more queues and every process can send a message to one of them. The only prerequisite is that the unique identifier of the queue must be known. Since we are now working with processes spawned by a fork operation, each process can know the identifier of a queue if the queue has been created before executing `fork()`.

The knowledge of a queue identifier makes it is also possible to read messages from it. Messages can be accessed sequentially, reading the messages in chronological order (from the oldest, the first, to the most recent, the last arrived), but selectively, that is considering only the messages of a certain type: this last feature give us a sort of control on the priority of the messages we read.

Queues, therefore, can be used to implement several communication scenarios, from the simplest ones where all processes send messages to and read messages from the same queue, to more complex solutions such as a full mail system between processes.

Irrespective of the communication framework we set for our processes, synchronization can now be performed through a richer tool. Processes needing synchronization can exchange messages to establish the correct timings schedule when performing actions. Messages, thus, do not replace semaphores, but, as already seen, cover functionalities that they cannot provide.

Before we can switch to implement message queues in C language, it is necessary to speak about another problem related to messages: the need of a _communication protocol_.

## Creating a protocol

A **protocol** is a set of rules which control the interaction of elements in a set. Every time you must regulate the exchange of information between two or more actors in a scenario you need a protocol, even when dealing with humans and not computers.

In [the past article](/blog/2013/02/13/concurrent-programming-4) we implemented a very simple protocol when instructing processes to access a resource according to the status of a semaphore. This latter is also modified as part of the protocol itself.

As already stated in a past article, there is no difference between interprocess communication on the same machine or in a distributed environment (multiple machines): indeed, every network protocol (TCP/IP, DNS, SMTP, just to cite some of the most famous) is built on a message exchange architecture.

This is a simple example of a protocol based on message exchange: two processes, A and B, are executing concurrently and processing different data. Once they end the processing they have to merge their results. Here, synchronization problems arise: what process actually does the merging? If the process in charge of the merging operation is the process A, for example, the merging protocol can be described by these time charts, one for each process

#### PROCESS B

1. Work with your data
2. When you finish send a message to A
3. When A answers, begin sending it your results

#### PROCESS A

1. Work with your data
2. Wait for a message from B
3. Answer the message
4. Receive data and merge them with yours

Choosing the merger process is in this case totally arbitrary. This, however, can be a very important part of a protocol.

This protocol is extensible ina simple way to the case of n processes: every process but A works with its own data and then send a message to A. When A answers, the other process sends its results: the only process modified in this generalization is A since it has to receive messages from multiple processes instead of just one.

## System V Message Queues

Now it is the time to speak about implementing these concepts in a Linux operating system environment. As already said we have a set of primitives that allow us to manage the structures related to message queues, and they are very similar to those dedicated to semaphore management, already described in [the past article](/blog/2013/02/13/concurrent-programming-4).

The structure used to describe a message is `msgbuf` and is declared in `linux/msg.h`

``` c
/* message buffer for msgsnd and msgrcv calls */
struct msgbuf {
   long mtype;         /* type of message */
   char mtext[1];      /* message text */
};
```

The field `mtype` represents the type of the message and is a strictly positive number: the correspondence between numbers and message types has to be set in advance and is part of the protocol definition.

The second field represents the content of the message. With the standard definition, no real message can be held inside it because the space is too little (just one byte). This is just a placeholder since the real structure used to describe messages can be redefined in order to contain complex data. An example of redefinition is the following

``` c
struct message {
   long mtype;         /* message type */
   long sender;        /* sender id */
   long receiver;        /* receiver id */        
   struct info data;   /* message content */
};
```

As you can see this is not a redefinition in the sense of object inheritance, being C a pure procedural language. The redefined structure must obey two simple rules: the first field must be a `long mtype` and the maximum size dimension shall be 8192 bytes.

This last limit is hard coded in the Linux kernel, i.e. there is a C `#define` directive in `linux/msg.h` which states

``` c
#define MSGMAX  8192   /* <= INT_MAX */   /* max size of message (bytes) */
```

In the same file, there are other interesting limits such as `MSGMNB`, the maximum total size of a queue, which in Linux is set by default to 16384. That means that a queue containing 2 messages is already full.

A short notice about kernel parameters such as `MSGMAX`, `MSGMNB` and others: kernel parameters can obviously be changed by modifying the source code and recompiling the kernel, that is generating a custom Linux kernel. Varying some parameters, however, can be dangerous for the portability of your programs. Indeed, if you increase the maximum size of a message to, say, 16384 bytes, in order to host big messages, your application will crash on a kernel without that modification, where the maximum size is still 8192. Kernel parameters can also be changed at runtime through the `/proc` filesystem and the sysctl interface: these topics are very rich and important and thus deserve a larger space. For the time being, I let the reader research about them.

#### Create a queue

To create a new queue a process should call the `msgget()` function

``` c
int msgget(key_t key, int msgflg)
```

which receives as arguments an IPC key (see [issue 4](/blog/2013/02/13/concurrent-programming-4)) and some flags, which by now can be set to `IPC_CREAT | 0660` (create the queue if it does not exist
and grant access to the owner and group users). The returned integer is called queue identifier and is unique in the system.

#### Send messages

To send a message to a queue we call the `msgsnd()` primitive

``` c
int msgsnd(int msqid, struct msgbuf *msgp, int msgsz, int msgflg)
```

where `msqid` is the identifier of the queue, `msgp` is a pointer to the message we have to send, `msgz` the size of the message in bytes (excluding the length of the `mtype` field, which is 4 bytes) and `msgflg` a flag related to the waiting policy.

Here, `msgp` is a pointer to a `struct msgbuf`. However, since that structure has been redefined, our actual call will contain a pointer to a variable of the redefined type.

The length of the message in bytes can be easily be found as

``` c
length = sizeof(struct message) - sizeof(long);
```

where `struct message` is our redefinition of `struct msgbuf`.

Waiting policies are similar to those introduced for semaphores. This time the policy is used when the queue is full. If `msgflg` is set to `IPC_NOWAIT`, the sender process will not wait for some available space and will exit with an error code.

#### Read messages

To read messages contained in a queue we use the `msgrcv()` system call

``` c
int msgrcv(int msqid, struct msgbuf *msgp, int msgsz, long mtype, int msgflg)
```

where the `msgp` pointer identifies the variable where we will copy the message read from the queue and mtype identifies the subset of messages we want to consider. As before, `msgp` should be a pointer to a variable of our redefined type and both `msgz` and `msgflg` have the same meaning of their counterparts in `msgsnd()`.

#### Delete a queue

Last, a queue can be removed calling the `msgctl()` primitive

``` c
int msgctl(int msqid, int cmd, struct msqid_ds *buf);
```

where the command `cmd` to remove the queue is `IPC_RMID`. Other two commands, `IPC_STAT` and `IPC_SET`, are available through `msgctl()`, but they are not interesting now.

## Putting it all together

Let's test all these concepts with a simple program which creates a message queue, sends a message to it, reads the message and cancels the queue. After the message has been read, a comparison between the original values and the ones in the message is performed to check that the system is working.

``` c
#include <stdio.h>
#include <stdlib.h>
#include <linux/ipc.h>
#include <linux/msg.h>

/* Redefines the struct msgbuf */
typedef struct mymsgbuf
{
  long mtype;
  int int_num;
  float float_num;
  char ch;
} message_t;

int main()
{
  int qid;
  key_t msgkey;

  message_t sent;
  message_t received;

  int length;

  /* Initialize the seed of the pseudo-random number generator */
  srand (time (0));

  /* Length of the message */
  length = sizeof(message_t) - sizeof(long);

  msgkey = ftok(".", getpid());

  /* Create the queue*/
  qid = msgget(msgkey, IPC_CREAT | 0660);

  printf("QID = %d\n", qid);
   
  /* Build a message */
  sent.mtype = 1;
  sent.int_num = rand();
  sent.float_num = (float)(rand())/3;
  sent.ch = abs(rand())%26 + 97;

  /* Send the message */
  msgsnd(qid, &sent, length, 0);
  printf("MESSAGE SENT\n");

  /* Receive the message */
  msgrcv(qid, &received, length, sent.mtype, 0);
  printf("MESSAGE RECEIVED\n");

  /* Control that received and sent messages are equal */
  printf("Interger number = %d (sent %d) -- ", received.int_num, sent.int_num);
  if(received.int_num == sent.int_num) {
    printf(" OK\n");
  }
  else {
    printf("ERROR\n");
  }

  printf("Float number = %f (sent %f) -- ", received.float_num, sent.float_num);
  if(received.float_num == sent.float_num) {
    printf(" OK\n");
  }
  else {
    printf("ERROR\n");
  }

  printf("Char = '%c' (sent '%c') -- ", received.ch, sent.ch);
  if(received.ch == sent.ch) {
    printf(" OK\n");
  }
  else {
    printf("ERROR\n");
  }

  /* Destroy the queue */
  msgctl(qid, IPC_RMID, 0);
}
```
[source code](/code/queues1.c)

Now we can create two processes and let them communicate through a message queue. Remembering forking concepts explained in the [issue 2](/blog/2013/02/04/concurrent-programming-2) you can recall that the child process, when created, receives a copy of the memory of its parent. This means that creating the queue before the fork operation results in both the parent and the child knowing the right queue identifier and thus capable of access it.

The following code creates a queue, then forks the execution. The child generates a random number, prints it on the standard output and sends them to the parent, which in turn prints it on the screen.

``` c
#include <stdio.h>
#include <stdlib.h>
#include <linux/ipc.h>
#include <linux/msg.h>
#include <sys/types.h>

/* Redefines the message structure */
typedef struct mymsgbuf
{
  long mtype;
  int num;
} message_t;

int main()
{
  int qid;
  key_t msgkey;
  pid_t pid;
   
  message_t buf;

  int length;
  int i;

  length = sizeof(message_t) - sizeof(long);

  msgkey = ftok(".", getpid());

  qid = msgget(msgkey, IPC_CREAT | 0660);

  if(!(pid = fork())){
    printf("CHILD - Queue ID = %d\n", qid);

    srand (time (0));
   
    for(i = 0; i < 10; i++){
      sleep (rand()%4);
      buf.mtype = 1;
      buf.num = rand()%100;
      msgsnd(qid, &buf, length, 0);      
      printf("CHILD - Sent message number %d: %d\n", i+1, buf.num);
    }
   
    return 0;
  }

  printf("PARENT - Queue ID = %d\n", qid);

  for(i = 0; i < 10; i++){
    sleep (rand()%4);
    msgrcv(qid, &buf, length, 1, 0);    
    printf("PARENT - Received message number %d: %d\n", i+1, buf.num);
  }

  printf("PARENT - Waiting for the child to terminate...\n"); 
  waitpid (pid, NULL, 0);
  printf("PARENT - Child ended\n");

  msgctl(qid, IPC_RMID, 0);

  return 0;
}
```
[source code](/code/queues2.c)

Compiling and running the program you can check the correct working of the shared message queue.

## Conclusions

In this article, we introduced a new IPC structure called message queue and the related concepts of message and protocol. These concepts are very important and are going to be the foundation stone of a project we will realize step by step in the future articles: a simple telephone switch simulator.

## Previous article

[Concurrent Programming 4](/blog/2013/02/13/concurrent-programming-4)

## Next articles

[Concurrent Programming 6](/blog/2013/04/23/concurrent-programming-6)