#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>

int main()
{
  pid_t pid;
  
  pid = getpid();
  printf("The pid assigned to the process is %d\n", pid);

  return 0;
}
