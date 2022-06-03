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
