/* Structured input/output */

#ifndef __intext__
#define __intext__

#include "misc.h"
#include "mlvalues.h"
#include "io.h"

/* Magic numbers used to discriminate between the extern formats */

#define Base_magic_number 0x8495A6B9
#define Big_endian_32_magic_number Base_magic_number
#define Little_endian_32_magic_number (Base_magic_number + 1)
#define Big_endian_64_magic_number (Base_magic_number + 2)
#define Little_endian_64_magic_number (Base_magic_number + 3)
#define Compact_magic_number (Base_magic_number + 4)
#define First_valid_magic_number Base_magic_number
#define Last_valid_magic_number (Base_magic_number + 4)

#ifdef SIXTYFOUR
# ifdef MOSML_BIG_ENDIAN
#  define Extern_magic_number Big_endian_64_magic_number
# else
#  define Extern_magic_number Little_endian_64_magic_number
# endif
#else
# ifdef MOSML_BIG_ENDIAN
#  define Extern_magic_number Big_endian_32_magic_number
# else
#  define Extern_magic_number Little_endian_32_magic_number
# endif
#endif

/* Codes for the compact format */

#define PREFIX_SMALL_BLOCK 0x80
#define PREFIX_SMALL_INT 0x40
#define PREFIX_SMALL_STRING 0x20
#define CODE_INT8 0x0
#define CODE_INT16 0x1
#define CODE_INT32 0x2
#define CODE_INT64 0x3
#define CODE_SHARED8 0x4
#define CODE_SHARED16 0x5
#define CODE_SHARED32 0x6
#define CODE_BLOCK32 0x8
#define CODE_STRING8 0x9
#define CODE_STRING32 0xA
#define CODE_DOUBLE 0xB

/* Initial sizes of data structures for extern */

#ifndef INITIAL_EXTERN_SIZE
#define INITIAL_EXTERN_SIZE 4096
#endif
#ifndef INITIAL_EXTERN_TABLE_SIZE
#define INITIAL_EXTERN_TABLE_SIZE 2039
#endif

/* The hashtable of objects already emitted */

typedef unsigned long byteoffset_t;

struct extern_obj {
  value obj;
  byteoffset_t ofs;
};

extern struct extern_obj * extern_table;
extern asize_t extern_table_size, extern_table_used;

extern byteoffset_t * extern_block;
extern asize_t extern_size, extern_pos;

#ifdef SIXTYFOUR
#define Hash(v) (((asize_t) ((v) >> 3)) % extern_table_size)
#else
#define Hash(v) (((asize_t) ((v) >> 2)) % extern_table_size)
#endif

void alloc_extern_table (void);
void resize_extern_table (void);

/* The entry points */

value extern_val (struct channel *, value);
value extern_compact_val (struct channel *, value);
value intern_val (struct channel *);
value intern_compact_val (struct channel *);

byteoffset_t emit_all(value root);
void adjust_pointers(value * start, mlsize_t size, color_t color);

#endif

