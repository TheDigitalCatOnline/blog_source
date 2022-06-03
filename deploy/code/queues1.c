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
