#include <stdio.h>
#include <stdlib.h>
#include <linux/types.h>
#include <linux/ipc.h>
#include <linux/sem.h>
   
int main(void)
{
  key_t key;
  int semid;
  union semun arg;

  key = ftok("/etc/fstab", getpid());

  /* create a semaphore set with only 1 semaphore: */
  semid = semget(key, 1, 0666 | IPC_CREAT);
  
  arg.val = 1;
  semctl(semid, 0, SETVAL, arg);
  
  return 0;
}
