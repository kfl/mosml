#ifndef _major_gc_
#define _major_gc_


#include "freelist.h"
#include "misc.h"

typedef struct {
  asize_t size;
  char *next;
} heap_chunk_head;

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

#define Phase_mark 0
#define Phase_weak 1
#define Phase_sweep 2

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern char *page_table;
extern asize_t page_table_size;
extern char *gc_sweep_hp;

#define In_heap 1
#define Not_in_heap 0
#ifndef SIXTEEN
#define Page(p) (((addr) (p) - (addr) heap_start) >> Page_log)
#define Is_in_heap(p) \
  ((addr)(p) >= (addr)heap_start && (addr)(p) < (addr)heap_end \
   && page_table [Page (p)] == In_heap)
#else
#define Page(p) \
  (((unsigned long)(p) >> (16 + Page_log - 4)) + ((unsigned)(p) >> Page_log))
#define Is_in_heap(p) (page_table [Page (p)] == In_heap)
#endif

void init_major_heap (asize_t);
asize_t round_heap_chunk_size (asize_t);
void darken (value);
void major_collection_slice (void);
void major_collection (void);
void finish_major_cycle (void);


#endif /* _major_gc_ */
