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
