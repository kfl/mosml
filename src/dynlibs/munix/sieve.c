// Find all primes less than 2 * argv[1]
// Then repeatedly read a number from stdin and print 1 or 0 on stdout 
// according as this number is or isn't a prime.
// sestoft@dina.kvl.dk 1995, 1999-11-07

// Compile with		gcc -O2 -o sieve sieve.c

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) { 
  int SIZE = atoi(argv[1]);
  char* isprim = (char*)malloc(SIZE * sizeof(char));
  {
    int i, j, step;
    for (i = 0; i < SIZE; i++)
      isprim[i] = 1;
    for (i = 0; i < SIZE; i++)
      if (isprim[i])
	for (j = 3*i+3, step = 2*i+3; j < SIZE; j += step)
	  isprim[j] = 0;
  }
  // Assertion:  isprim[i] == 1 iff 2i+3 is a prime, for 0 <= i < SIZE
  { 
    int p, q;
    while (scanf("%d", &p) != EOF) {
      printf("%d\n", p == 2 || p % 2 !=0 && (q = (p-3)/2) < SIZE && isprim[q]);
      fflush(NULL);
    }
    return 0;
  }
}

