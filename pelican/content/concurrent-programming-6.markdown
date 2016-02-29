Title: Concurrent programming - 6
Date: 2013-04-23 08:29 +0200
Category: Programming
Tags: C, operating systems, concurrent programming
Authors: Leonardo Giordani
Slug: concurrent-programming-6
Series: "Concurrent programming"
Summary:

## Abstract

Issue 5 of this series ended with a small program where two processes exchanged ten numbers through a message queue, thus being a synchronized producer-consumer couple. This time I am going to show and comment the code of a very simple communication simulator written in C. The code leverages IPC queues to allow multiple processes to talk each other while running concurrently.

## The simulator

The program simulates a messaging switch, where multiple users (child processes) connect to a central switch (parent process) and send text messages (SMS-like) to other users through it. The switch receives messages and route them if the recipient is reachable (process is running). Moreover, the switch can perform a timing operation on a user, checking how much time the user needs to answer a message, or select it for termination. Last, the switch counts how many times a user sends a message to an unreachable user and when a threshold is reached terminates it. Switch and user decisions are taken extracting pseudo-random numbers and comparing them with thresholds given on the command line. Remember that the simulation wants to demonstrate a concrete use of message queues and does not claim to be a complete communication system between processes.

## 10,000 feet overview

I splitted the simulator in 3 parts, namely two stacked layers of function and the main application code. The first layer implements the message structure, provides functions to interact with it (getters and setters), exports basic functions to manage queues and to send and receive messages. The second layer exports functions that implement the protocol, i.e. the actions users and switch can perform. Last, the main function contains the actual logic of the whole simulation. Due to the instructional purpose of the simulation some of the solution implemented are far from optimal or even correct (e.g. random numbers management); I suggest you to try to evolve the simulation, adding new services or messages between users.

## Debugging nightmares

Be ready to spend some time debugging your multiprocessing applications! If you ever debugged some code, you know that the most friendly bugs are by far those which are reproducible: you only need to add some debugging output or step through the code with the debugger and the bug is spotted. Multiprocessing and network application are at the very opposite corner: most of the time the bugs in that sort of applications are very difficult to reproduce, when not impossible. The concurrent execution of multiple processes, indeed, makes every execution unique, since the actual execution sequence is random (random means that it is the result of so many factors that the result is unpredictable and thus irreproducible). Network programs suffers from the same problems (concurrent execution), worsened by network lags that add a factor of randomness.

## Code and analysis

I am not going to include the whole code of the layers directly in the post text as they are a straightforward implementation of what was presented in the previous articles; the code of the main application is, on the contrary, analyzed block by block. The links to the source files are at the end of the article, together with compilation instructions.

## Layer 1

#### Message structure

The structure of the message is

``` c
typedef struct
{
 int sender;
 int recipient;
 char text[160];
 int service;
 int service_data;
} message_t;

typedef struct
{
 long mtype;
 message_t mtext;
} messagebuf_t;
```

Here, `sender` and `recipient` are numbers that identify the switch and the users (0 is the switch, then users are numbered increasingly when they connect to the switch); `text` is the content of a message a user sends to another user, and is 160 characters long to mimic SMS behaviour. Last `service` is the identifier of some system operation, like a request the switch sends to the users; `service_data` carries the optional data the service needs to communicate. Actual services are implemented in layer 2 and I am going to describe them later.

#### Other functions

Layer 1 exposes many functions: some are simple set and get functions to deal with the message structure while five simplify access to IPC structures; `build_key()` makes an IPC key from a given character, `create_queue()` and `remove_queue()` manage IPC queues and last `send_message()` and `receive_message()` give a simple way to route messages.

A very simple error management code has been introduced here: C language does not allow to use exceptions, so errors have to be managed by functions or returned through the `return` statement. A solid error management in C is outside the scope of this article, so here you will find the bare minimum.

The error management of `receive_message()` needs a little explanation.

``` c
int receive_message(int qid, long type, messagebuf_t *qbuf){
  int result, length;
  length = sizeof(messagebuf_t) - sizeof(long);
  
  if((result = msgrcv(qid, (struct msgbuf *)qbuf, length, type, IPC_NOWAIT)) == -1){
    if(errno == ENOMSG){
      return 0;
    }
    else{
      perror("msgrcv");
      exit(1);
    }
  }
  
  return result;
}
```

Here, `msgrcv()` fails even when there are no messages of the given type, producing a `ENOMSG` error. This is the reason why that case has been ignored through the `if(errno == ENOMSG)` construct.

## Layer 2

#### The protocol

Layer2 implements the actual communication protocol between the switch and the users. Users send text messages to other users, and those messages are routed by the switch. Users and switch can also send service messages; those are messages encompassing information used to manage the system. 

Every user, when spawned, initializes a queue, connects to the switch and communicates the queue id. Since the queues can contain old unread messages, it is necessary to empty them before the use. Each user enters a loop where it sleeps a while, receives service messages, sends messages and last receives text messages. At the same time, the switch collects queue ids, routes text messages from the sender to the recipient and sometimes sends a service request to a user.

The services the switch can request from users are to terminate and to test the answering time. Both can be requested on a probability base, but the first is also forced when the user sends too many messages to unreachable recipients.

The answering time test is performed in a simple way: the switch requests the service and records the time of the request. The user answers with a message that contains the time at which it received the request and the switch computes the difference between the two. Pay attention that both the timing service and the random number extraction used to request services on a probability base are not exact; the code works but is good only for a demonstration system.

I defined some useful constants in the header file: `MAX_SLEEP` is the maximum number of seconds a user waits before performing an action while `TYPE_SERVICE` and `TYPE_TEXT` identify the type of message. The defines which name starts with `SERVICE_` list all the possible services: `SERVICE_TERMINATE` forces a user to quit; `SERVICE_TIME` makes it perform a timing operation; `SERVICE_CONNECT` and `SERVICE_DISCONNECT` tell the switch a new user has connected or disconnected; `SERVICE_QID` bears the identifier of the user queue to the switch; `SERVICE_UNREACHABLE_DESTINATION` communicates a user that the recipient of a message is no more online.

#### Queues

Two functions are dedicated to queues, `init_queue()` and `close_queue()`. The first builds an IPC key from a given number (previously converting it to a char) and runs the `create_queue()` function from layer 1. Because of the char conversion the range of integers it can accept is 0-255, and for simplicity's sake there is no check in the whole code that a queue key has not yet been assigned. For this example, I am simply leveraging that different numbers return different keys and, thus, queues.

The second function closes the queue running the `remove_queue()` function from layer 1. The underlying system calls are not used directly to allow the future introduction of checks on the assigned queues.

#### User functions

Users have five functions that implement their part of the protocol. Two functions communicate to the switch that the user connected or disconnected, namely `user_send_connect()` and `user_send_disconnect()`; both carry the `sender` id and the switch id `sw`, which however in the main program is always 0. The `user_send_qid()` function communicates to the switch the queue id of the user; `user_send_text_message()` sends a string of text to another user and `user_send_time()` answers the timing service.

#### Switch functions

The switch can execute `switch_send_text_message()` to deliver a text message sent by a user; `switch_send_terminate()` to ask a user to terminate; `switch_send_time()` to ask a user to perform a timing service.

#### Code analysis

Now I will briefly review the whole application code to better explain the different parts of the system. Please remember that this is a demonstration system so many choices have been made for simplicity's sake.

``` c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/ipc.h>
#include <linux/msg.h>
#include <time.h>
#include <signal.h>
#include <wait.h>
#include "layer1.h"
#include "layer2.h"

#define MINCHILDS 1
#define MAXCHILDS 15

#define MAXFAILS 10
```

After the list of includes, you will find three defines that rule the number of child processes the system can spawn and the maximum number of messages a user can send to unreachable recipients before the switch asks it to terminate.

``` c
int random_number(int max)
{
  double r,x;
  r = (double) random();
  x = r * (double) max / RAND_MAX;
  return((int) x);
}

void usage(char *argv[])
{
  printf("Telephone switch simulator\n");
  printf("%s <number of users> <service probability> <text message probability>\n", argv[0]);
  printf("\n");
  printf("     <number of users> - Number of users alive in the system (%d - %d)\n", MINCHILDS, MAXCHILDS);
  printf("     <service probability> - The probability that the switch requires a service from the user (0-100)\n");
  printf("     <text message probability> - The probability the a user sends a message to another user (0-100)\n\n");
}
```

The `random_number()` function is used to extract a random number between 0 and a maximum `max`; `RAND_MAX` is a define of the standard library and represents the maximum number the `random()` function can return and in the GNU C library it is 2^31 (2147483647); here it is used to calculate a proportion with the maximum value given by the caller. The `usage()` function helps the user remembering the command line arguments; as you can see, the program receives 3 mandatory input values: the number of users that the switch can spawn; the probability that the switch requests a service to a user when this latter sends a message; the probability that a user sends a text message to another user.

``` c
int main(int argc, char *argv[])
{
  pid_t pid;
  int i;

  int users_number;
  int service_probability;
  int text_message_probability;

  int status;
  int deadproc = 0; /* A counter of the already terminated user processes */
  int qid;
  int sw; /* Qid of the switch */
  int dest; /* Destination of the message */
  int olddest; /* Destination of the previous message */

  int queues[MAXCHILDS + 1]; /* Queue identifiers - 0 is the qid of the switch */

  int msg_sender;
  int msg_recipient;
  char msg_text[160];
  int msg_service;
  int msg_service_data;

  int t;
  int timing[MAXCHILDS + 1][2];

  int unreachable_destinations[MAXCHILDS + 1];

  char *padding = "                                                                      ";
  char text[160];

  messagebuf_t msg, in;

```
Many of these variables are just helpers that simplify the code; `queues` holds the queue identifier of each process; `timing` holds the information about user timing service results; `unreachable_destinations` contains how many times each user sent a message to an unreachable recipient; `dest` and `olddest` are used to avoid a user to send a message to the recipient of the previous one.  The `padding` variable is a quick and dirty way to create two columns, the left one filled by switch messages and the right one by user ones.

Remember that in C you have to declare variables at the beginning of the program while in C++ you can declare them anywhere. This means that child processes, a copy of the parent, carry in memory some variables such as `queues` and `timings` that are used only by the switch. This is both a waste of resources and a dangerous situation, so remember that in general statically allocated variables are not a good choice for concurrent programs.

``` c
  /* Command line argument parsing */
  if(argc != 4){
    usage(argv);
    exit(0);
  }

  users_number = strtol(argv[1], NULL, 10);  
  service_probability = strtol(argv[2], NULL, 10);
  text_message_probability = strtol(argv[3], NULL, 10);
  

  if((users_number < MINCHILDS) || (users_number > MAXCHILDS)){
    usage(argv);
    exit(1);
  }

  if((service_probability < 0) || (service_probability > 100)){
    usage(argv);
    exit(0);
  }

  if((text_message_probability < 0) || (text_message_probability > 100)){
    usage(argv);
    exit(0);
  }

  printf("Number of users: %d\n", users_number);
  printf("Probability of a service request: %d%%\n", service_probability);
  printf("Probability of a text message: %d%%\n", text_message_probability);
  printf("\n");

  /* Initialize the random number generator */
  srandom(time(NULL));
```

All these lines contain initialization code and checks for the values passed on the command line.

``` c
  /* Switch queue initialization */
  sw = init_queue(255);
  
  /* Read the last messages we have in the queue */
  while(receive_message(sw, TYPE_TEXT, &in)){
    printf("%d -- S -- Receiving old text messages\n", (int) time(NULL), i);
  }

  /* Read the last messages we have in the queue */
  while(receive_message(sw, TYPE_SERVICE, &in)){
    printf("%d -- S -- Receiving old service messge\n", (int) time(NULL), i);
  }

  /* All queues are "uninitialized" (set equal to switch queue) */
  for(i = 0; i <= users_number; i++){
    queues[i] = sw;
    unreachable_destinations[i] = 0;
  }
```

The switch initializes its queue; this has to be done before spawning users since the `sw` variable will be copied in each child process and used to communicate with the switch. The queue is initialized with the number 255 just to be sure that no child process initializes the same queue. As explained, IPC queues provide no mechanism to ensure uniqueness of the instanced queues, so we have to establish our own system; in this simple example we spawn a maximum of 15 users, so we could also use `16` or `MAXCHILDS+1` to initialize the switch queue. Since queues are shared structures and nothing prevents the system to assign to a process a previously used queue we must ensure that the queue is empty, so the switch reads and discards all text and service messages found in its queue. Last, the `queues` and `unreachable_destinations` arrays are initialized.


``` c
  /* Create users */
  for(i = 1; i <= users_number; i++){
    pid = fork();

    if (pid == 0){
      srandom(time(NULL) + 1000*i);

      /* Initialize queue  */
      qid = init_queue(i);
      
      /* Read the last messages we have in the queue */
      while(receive_message(qid, TYPE_TEXT, &in)){
        printf("%s%d -- U %02d -- Receiving old text messages\n", padding, (int) time(NULL), i);
      }

      /* Read the last messages we have in the queue */
      while(receive_message(qid, TYPE_SERVICE, &in)){
        printf("%s%d -- U %02d -- Receiving old service messge\n", padding, (int) time(NULL), i);
      }

      /* Let the switch know we are alive */
      user_send_connect(i, sw);
      
      /* Let the switch know how to reach us */
      user_send_qid(i, qid, sw);
```

This code spawns the users and from here the code splits in two, and the code of the user processes is inside the if construct. As you can see, the user acts like the switch at the very beginning, initializing its own queue and flushing the possible messages it contains. After this, the user sends the switch a message to communicate that it is alive and sends its qid to allow the user to communicate.

``` c
      /* Enter the main loop */
      while(1){
	    sleep(rand()%MAX_SLEEP);
		
		/* Check if the switch requested a service */
		if(receive_message(qid, TYPE_SERVICE, &in)){
		  msg_service = get_service(&in);
		  
		  switch(msg_service){
		  
    	  case SERVICE_TERMINATE:
    	    /* Send an acknowledgement to the switch */
    	    user_send_disconnect(i, getpid(), sw);
	    
    	    /* Read the last messages we have in the queue */
    	    while(receive_message(qid, TYPE_TEXT, &in)){
    	      msg_sender = get_sender(&in);
    	      get_text(&in, msg_text);
    	      printf("%s%d -- U %02d -- Message received\n", padding, (int) time(NULL), i);
			  printf("%s                      Sender: %d\n", padding, msg_sender);
			  printf("%s                      Text: %s\n", padding, msg_text);
    	    }
	    
    	    /* Remove the queue */
    	    close_queue(qid);
    	    printf("%s%d -- U %02d -- Termination\n", padding, (int) time(NULL), i);
    	    exit(0);
    	    break;

    	  case SERVICE_TIME:
    	    user_send_time(i, sw);
    	    printf("%s%d -- U %02d -- Timing\n", padding, (int) time(NULL), i);
    	    break;
		  }
    	}
```

The user loops infinitely and at each loop sleeps a random number of seconds. After that, it checks its message queue for service messages (`receive_message()` with the parameter `TYPE_SERVICE`); if the requested service is `SERVICE_TERMINATE`, an acknowledgement is sent to the switch (`user_send_disconnect()`) so that the user is marked as offline and no more messages are sent with it as recipient. The user then reads all remaining text messages in its queue and closes the queue. Last with `exit()`the user process terminates. If the requested service is `SERVICE_TIME`, the user simply sends the current time back to the switch through `user_send_time()`.

``` c
    	/* Send a message */
    	if(random_number(100) < text_message_probability){
    	  dest = random_number(users_number + 1);

    	  /* Do not send a message to the switch, to yourself and to the previous recipient */
    	  while((dest == 0) || (dest == i) || (dest == olddest)){
    	    dest = random_number(users_number + 1);
    	  }
    	  olddest = dest;

    	  printf("%s%d -- U %02d -- Message to user %d\n", padding, (int) time(NULL), i, dest);
          sprintf(text, "A message from me (%d) to you (%d)", i, dest);
    	  user_send_text_message(i, dest, text, sw);
    	}
	
    	/* Check the incoming box for simple messages */
    	if(receive_message(qid, TYPE_TEXT, &in)){
    	  msg_sender = get_sender(&in);
    	  get_text(&in, msg_text);
	      printf("%s%d -- U %02d -- Message received\n", padding, (int) time(NULL), i);
	      printf("%s                      Sender: %d\n", padding, msg_sender);
	      printf("%s                      Text: %s\n", padding, msg_text);
    	}
      }
    }
  }
```

The user extracts a random number and tests it against the probability given on the command line; if the test gives a positive result the user extracts a random user, avoiding the switch (`dest == 0`), itself (`dest == i`) and the recipient of the previous message it sent (`dest == olddest`). The new recipient is saved in `olddest`, and the message is sent through `user_send_text_message()`. This part ends the user code.

``` c
  /* Switch (parent process) */ 
  while(1){
    /* Check if some user is answering to service messages */
    if(receive_message(sw, TYPE_SERVICE, &in)){
      msg_service = get_service(&in);
      msg_sender = get_sender(&in);

      switch(msg_service){
      case SERVICE_CONNECT:
        /* A new user has connected */
        printf("%d -- S -- Service: connection\n", (int) time(NULL));
        printf("                   User: %d\n", msg_sender);
        break;

      case SERVICE_DISCONNECT:
        /* The user is terminating */
		printf("%d -- S -- Service: disconnection\n", (int) time(NULL));
		printf("                   User: %d\n", msg_sender);

	    deadproc++;
	    break;

      case SERVICE_QID:
	    /* The user is sending us its queue id */
	    msg_service_data = get_service_data(&in);
	    printf("%d -- S -- Service: queue\n", (int) time(NULL));
		printf("                   User: %d\n", msg_sender);
		printf("                   Qid: %d\n", msg_service_data);
		queues[msg_sender] = msg_service_data;
		break;

      case SERVICE_TIME:
	    msg_service_data = get_service_data(&in);

        /* Timing informations */
        timing[msg_sender][1] = msg_service_data - timing[msg_sender][1];

        printf("%d -- S -- Service: timing\n", (int) time(NULL));
        printf("                   User: %d\n", msg_sender);
        printf("                   Timing: %d\n", timing[msg_sender][1]);

        /* The user is no more blocked by a timing operation */
        timing[msg_sender][0] = 0;
        break;
      }
    }
```

The switch code has a structure similar to that of the user code. It loops infinitely with a `while(1)` statement, and inside this it checks for incoming messages and acts accordingly. The first thing it does is to check for service messages. If the service is `SERVICE_CONNECT` a user is communicating that it is alive, and the switch simply print out a log of this event; when a user disconnects it sends a `SERVICE_DISCONNECT` message and the switch increases the number of dead processes to monitor the number of active user processes; if the service is a `SERVICE_QID` a user is sending the switch its queue identifier and this is stored in the `queues` array; last, the `SERVICE_TIME` messages mean that a user is answering a timing request; its answer is compared with the recorded send time and the result is printed.

``` c

    /* Check if some user has connected */
    if(receive_message(sw, TYPE_TEXT, &in)){

      msg_recipient = get_recipient(&in);
      msg_sender = get_sender(&in);
      get_text(&in, msg_text);
      
      /* If the destination is connected */
      if(queues[msg_recipient] != sw){
	    /* Send the message (forward it) */
     	switch_send_text_message(msg_sender, msg_text, queues[msg_recipient]);
	
	    printf("%d -- S -- Routing message\n", (int) time(NULL));
		printf("                   Sender: %d -- Destination: %d\n", msg_sender, msg_recipient);
		printf("                   Text: %s\n", msg_text);
      }
	  else{
		unreachable_destinations[msg_sender] += 1;
        
		if (unreachable_destinations[msg_sender] > MAXFAILS) {
          continue;
        }

        printf("%d -- S -- Unreachable destination\n", (int) time(NULL));
        printf("                   Sender: %d -- Destination: %d\n", msg_sender, msg_recipient);
        printf("                   Text: %s\n", msg_text);
        printf("                   Threshold: %d/%d\n", unreachable_destinations[msg_sender], MAXFAILS);

        if (unreachable_destinations[msg_sender] == MAXFAILS) {
          printf("%d -- S -- User %d reached max unreachable destinations\n", (int) time(NULL), msg_sender);

          switch_send_terminate(queues[msg_sender]);

          /* Remove its queue from the list */
          queues[msg_sender] = sw;
        }
      }
```

Here, the switch checks the queue for incoming text messages users send to other users. If the recipient is connected (reachable) the message is routed through `switch_send_text_message()`; otherwise the counter of failed dispatches `unreachable_destinations` is incremented for that user; when the maximum number of failures `MAXFAILS` is reached the switch sends that user a termination request.

``` c
      /* Randomly request a service to the sender of the last message */
      if((random_number(100)  < service_probability) && (queues[msg_sender] != sw)){
	    if (random_number(100) < 40){
	      /* The user must terminate */
	      printf("%d -- S -- User %d chosen for termination\n", (int) time(NULL), msg_sender);

	      switch_send_terminate(queues[msg_sender]);

	      /* Remove its queue from the list */
	      queues[msg_sender] = sw;
	    }
	    else {
	      /* Check if we are already timing that user */
	      if(!timing[msg_sender][0]){
	        timing[msg_sender][0] = 1;
	        timing[msg_sender][1] = (int) time(NULL);
	        printf("%d -- S -- User %d chosen for timing...\n", timing[msg_sender][1], msg_sender);
	        switch_send_time(queues[msg_sender]);
	      }
	    }
      }
    }
```

Each user that sends a message through the switch can be selected as the destination of a service request. Here, the switch tests a random number against the probability given on the command line, ensuring that the users is not already unreachable. The probability of a termination service is hardcoded (40%), the other case being the timing service; when this latter is requested, the switch sends a message to the user recording the dispatch time so that it can be later compared with the user answer.

``` c
    else{
      if(deadproc == users_number){
	    /* All childs have been terminated, just wait for the last to complete its jobs */
	    waitpid(pid, &status, 0);
		
		/* Remove the switch queue */
		remove_queue(sw);
		
		printf("\n");
		printf("No more active users. Switch turns off.\n");
		
		/* Terminate the program */
		exit(0);
      }
    }
  }
}
```

When there are no incoming messages the switch checks if the number of dead processes is equal to the number of users; when this happens the switch waits for each child process (to avoid zombie processes), removes its queue and terminates.

## Files and compilation

The 5 files of this small program can be downloaded here:

* [layer1.h](/code/ipc_demo/layer1.h)
* [layer1.c](/code/ipc_demo/layer1.c)
* [layer2.h](/code/ipc_demo/layer2.h)
* [layer2.c](/code/ipc_demo/layer2.c)
* [main.c](/code/ipc_demo/main.c)

and can be compiled with the following command line

``` bash
gcc -o ipc_demo main.c layer1.c layer2.c
```

A typical execution can be obtained running the program with the following parameters

``` bash
./ipc_demo 8 30 80
```

Remember that the output lines of the processes are mixed and generally not in order; indeed, you can find the answer of a user printed before the switch request. Timestamps can help you find the right order, but the resolution of the `time()` function is a second, and in such a time span many messages can be sent.

## Conclusions

This article ends by now this little series on concurrent programming in C and IPC structures. As you can see C is not the best language to implement concurrent programming concepts, due to its very low level nature. However, since many OSs are written in C (and/or C++) knowledge of the way this language can provide concurrent execution is useful.

There is much left to say about concurrent programming structures: atomicity, mutual exclusions, threads, monitors are just some of the most important. I will post new articles on such topics in the future, perhaps showing their implementation in other languages.

## Previous article

[Concurrent Programming 5](/blog/2013/02/28/concurrent-programming-5)
