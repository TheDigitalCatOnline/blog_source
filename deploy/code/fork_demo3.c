#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main()
{
  pid_t pid;
  int i;
  
  pid = fork();
  
  if (pid == 0){
    for (i = 0; i < 14; i++){
      printf("-child-\n");
    }
    return 0;
  }

  printf("+parent+ Waiting for the child to terminate...\n"); 
  waitpid (pid, NULL, 0);
  printf("+parent+ ...ended\n");

  return 0;
}
