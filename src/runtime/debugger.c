#ifdef DEBUG

#include <stdio.h>
#include "debugger.h"
#include "instruct.h"
#include "memory.h"
#include "mlvalues.h"
#include "opnames.h"
#include "stacks.h"
#include "unalignd.h"

bytecode_t log_buffer[LOG_BUFFER_SIZE];
bytecode_t * log_ptr;
int trace_flag;

/* Displaying a heap object */

long max_print = 100;
long max_print_depth = 10;

long print_cnt;

static void print_val(v, d)
     value v;
     long d;
{
  long n;
  value * p;

  if (d <= 0) {
    printf(".");
    return;
  }
  print_cnt--;
  if (print_cnt <= 0) {
    if (print_cnt == 0) printf("...");
    return;
  }
  if (Is_long(v))
    printf("%ld", Long_val(v));
  else if (!Is_in_heap (v) && !Is_young (v))
    printf("0x%lx", v);
  else switch(Tag_val(v)) {
    case String_tag:
      printf("\"%s\"", String_val(v));
      break;
    case Double_tag:
      printf("%g", Double_val(v));
      break;
    case Abstract_tag:
      printf("<abstract>");
      break;
    case Final_tag:
      printf("<finalized>");
      break;
    default:
      n = Tag_val(v);
      if (n < 26){
	printf ("%c", n + 'A');
      }else{
        printf("tag%ld", n);
      }
      n = Wosize_val(v);
      if (n > 0) {
        printf("(");
        p = &Field(v, 0);
        while (n > 1) {
          print_val(*p, d-1);
          printf(", ");
          p++;
          n--;
        }
        print_val(*p, d-1);
        printf(")");
      }
      break;
  }
}

void print_value(v)
	value v;
{
  print_cnt = max_print;
  print_val(v, max_print_depth);
  printf("\n");
}

extern bytecode_t start_code;

void print_pc(pc)
     bytecode_t pc;
{
  printf("%6d  ", pc - start_code);
}

/* Disassembling one instruction */

bytecode_t disasm_instr(pc)
	bytecode_t pc;
{
  return pc;
}

void disasm(pc)
     bytecode_t pc;
{
  int i;

  for (i = 0; i < 20; i++)
    pc = disasm_instr(pc);
}

void post_mortem(n)
	int n;
{
  bytecode_t * p;

  if (n > LOG_BUFFER_SIZE) n = LOG_BUFFER_SIZE;
  for (p = log_buffer +
             (unsigned) (log_ptr - log_buffer - n) % LOG_BUFFER_SIZE;
       n > 0;
       n--) {
    disasm_instr(*p);
    p++;
    if (p >= log_buffer + LOG_BUFFER_SIZE) p = log_buffer;
  }
}

void failed_assert (expr, file, line)
     char *expr, *file;
     int line;
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
	   expr, file, line);
  exit (100);
}

static unsigned long seed = 0x12345;

unsigned long not_random ()
{
  seed = seed * 65537 + 12345;
  return seed;
}


#endif /* DEBUG */
