#include <stdio.h>

int main(argc, argv)
     int argc;
     char ** argv;
{
  printf("%lu %lu %lu\n", sizeof(int), sizeof(long), sizeof(long *));
  return 0;
}
