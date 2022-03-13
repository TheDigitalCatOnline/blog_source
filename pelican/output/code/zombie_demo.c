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
