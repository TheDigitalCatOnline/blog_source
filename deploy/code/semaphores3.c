#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <linux/types.h>
#include <linux/ipc.h>
#include <linux/sem.h>

int main(int argc, char *argv[])
{
  /* IPC structures */
  pid_t pid;
  key_t key;
  int semid;
  union semun arg;
  struct sembuf lock_res = {0, -1, 0};
  struct sembuf rel_res = {0, 1, 0};
  struct sembuf push[2] = {1, -1, IPC_NOWAIT, 2, 1, IPC_NOWAIT};
  struct sembuf pop[2] = {1, 1, IPC_NOWAIT, 2, -1, IPC_NOWAIT};

  int i, j;
  int len;
  int num_proc = 5;
  int num_write_actions = 20;
  int num_read_actions = 100;
  
  if(argc < 2){
    printf("Usage: %s <size>\n", argv[0]);
    exit(0);
  }

  len = strtol(argv[1], NULL, 10);
  
  key = ftok("/etc/fstab", getpid());

  /* Create a set with 3 semaphores */
  semid = semget(key, 3, 0666 | IPC_CREAT);

  /* Initialize semaphore #0 to 1 - Resource controller */
  arg.val = 1;
  semctl(semid, 0, SETVAL, arg);

  /* Initialize semaphore #1 to buf_length - Overflow controller */
  /* Sem value represents free space in buffer */
  arg.val = len;
  semctl(semid, 1, SETVAL, arg);

  /* Initialize semaphore #2 to buf_length - Underflow controller */
  /* Sem value represents the number of elements in buffer */
  arg.val = 0;
  semctl(semid, 2, SETVAL, arg);

  /* Fork */
  for (i = 0; i < num_proc; i++){
    pid = fork();
    if (!pid){
      /* Child process code*/
      for (j = 0; j < num_write_actions; j++){
	sleep(rand()%6);

	/* Try to lock the buffer - sem #0 */
	if (semop(semid, &lock_res, 1) == -1){
	  perror("semop:lock_res (write)");
	}

	/* Lock a free cell - sem #1 */
	/* Push an element - sem #2 */
	if (semop(semid, &push, 2) != -1){
	  printf("---> Child process %d: Element written\n", getpid());
	}
	else{
	  printf("---> Child process %d: BUFFER FULL\n", getpid());
	}

	/* Release the buffer */
	semop(semid, &rel_res, 1);
      }

      exit(0);
    }
  }
  
  for (i = 0;i < num_read_actions; i++){
    sleep(rand()%3);

    /* Try to lock the buffer - sem #0 */
    if (semop(semid, &lock_res, 1) == -1){
      perror("semop:lock_res (read)");
    }

    /* Unlock a free cell - sem #1 */
    /* Pop an element - sem #2 */
    if (semop(semid, &pop, 2) != -1){
      printf("<--- Parent process %d: Element read\n", getpid());
    }
    else {
      printf("<--- Parent process %d: BUFFER EMPTY\n", getpid());
    }

    /* Release the buffer */
    semop(semid, &rel_res, 1);
  }
  
  /* Destroy semaphores */
  semctl(semid, 0, IPC_RMID);

  return 0;
}
