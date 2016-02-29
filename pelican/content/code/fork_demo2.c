#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main() {
  pid_t pid;
  int i;

  pid = fork();

  if (pid == 0){
      for (i = 0; i < 8; i++){
          sleep (rand()%4);
	  printf("-child-\n");
      }
      return 0;
  }

  for (i = 0; i < 8; i++){
      sleep (rand()%4);
      printf("+parent+\n");
  }

  return 0;
}
