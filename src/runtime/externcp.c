/* Structured output, compact format */

#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "reverse.h"
#include "str.h"

static void output_number(struct channel * chan, int code, long val, int nbits)
{
  int i;
  putch(chan, code);
  for (i = nbits - 8; i >= 0; i -= 8) putch(chan, val >> i);
}

static byteoffset_t obj_counter;    /* Number of objects emitted so far */
static unsigned long size_32;  /* Size in words of 32-bit block for struct. */
static unsigned long size_64;  /* Size in words of 64-bit block for struct. */

static void emit_compact(struct channel * chan, value v)
{
 tailcall:
  if (Is_long(v)) {
    long n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      putch(chan, PREFIX_SMALL_INT + n);
    } else if (n >= -(1 << 7) && n < (1 << 7)) {
      output_number(chan, CODE_INT8, n, 8);
    } else if (n >= -(1 << 15) && n < (1 << 15)) {
      output_number(chan, CODE_INT16, n, 16);
#ifdef SIXTYFOUR
    } else if (n < -(1L << 31) || n >= (1L << 31)) {
      output_number(chan, CODE_INT64, n, 64);
#endif
    } else
      output_number(chan, CODE_INT32, n, 32);
  } else {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);
      asize_t h;
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      if (tag < 16) {
        putch(chan, PREFIX_SMALL_BLOCK + tag);
      } else {
        output_number(chan, CODE_BLOCK32, hd, 32);
      }
    } else {
      /* Check if already seen */
      if (2 * obj_counter >= extern_table_size) resize_extern_table();
      h = Hash(v);
      while (extern_table[h].obj != 0) {
        if (extern_table[h].obj == v) {
          byteoffset_t d = obj_counter - extern_table[h].ofs;
          if (d < 0x100) {
            output_number(chan, CODE_SHARED8, d, 8);
          } else if (d < 0x10000) {
            output_number(chan, CODE_SHARED16, d, 16);
          } else {
            output_number(chan, CODE_SHARED32, d, 32);
          }
          return;
        }
        h++;
        if (h >= extern_table_size) h = 0;
      }
      /* Not seen yet. Record the object and output its contents. */
      extern_table[h].obj = v;
      extern_table[h].ofs = obj_counter;
      obj_counter++;
      switch(tag) {
      case String_tag: {
        mlsize_t len = string_length(v);
        if (len < 0x20) {
          putch(chan, PREFIX_SMALL_STRING + len);
        } else if (len < 0x100) {
          output_number(chan, CODE_STRING8, len, 8);
        } else {
          output_number(chan, CODE_STRING32, len, 32);
        }
        putblock(chan, String_val(v), len);
        size_32 += 1 + (len + 4) / 4;
        size_64 += 1 + (len + 8) / 8;
        break;
      }
      case Double_tag: {
        double buffer;
        if (sizeof(double) != 8)
          invalid_argument("output_compact_value: non-standard floats");
        putch(chan, CODE_DOUBLE);
        buffer = Double_val(v);
#ifndef MOSML_BIG_ENDIAN
        Reverse_double(&buffer);
#endif
        putblock(chan, (char *) &buffer, 8);
        size_32 += 1 + sizeof(double) / 4;
        size_64 += 1 + sizeof(double) / 8;
        break;
      }
      case Abstract_tag:
      case Final_tag:
        invalid_argument("output_compact_value: abstract value");
        break;
      case Closure_tag:
        invalid_argument("output_compact_value: functional value");
        break;
      default: {
        mlsize_t i;
        if (tag < 16 && sz < 8) {
          putch(chan, PREFIX_SMALL_BLOCK + tag + (sz << 4));
        } else {
          output_number(chan, CODE_BLOCK32, hd, 32);
        }
        size_32 += 1 + sz;
        size_64 += 1 + sz;
        for (i = 0; i < sz - 1; i++) emit_compact(chan, Field(v, i));
        v = Field(v, i);
        goto tailcall;
      }
      }
    }
  }
}

value extern_compact_val(struct channel * chan, value v) /* ML */
{
  value start_loc, final_loc;
  putword(chan, Compact_magic_number);
  start_loc = pos_out(chan);
  putword(chan, 0);
  putword(chan, 0);
  putword(chan, 0);
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  emit_compact(chan, v);
#ifdef SIXTYFOUR
  if (size_32 >= (1L << 32) || size_64 >= (1L << 32)) {
    /* The object is so big its size cannot be written in the header.
       Besides, some of the block sizes or string lengths or shared offsets
       it contains may have overflowed the 32 bits used to write them. */
    failwith("output_compact_value: object too big");
  }
#endif
  final_loc = pos_out(chan);
  seek_out(chan, start_loc);
  putword(chan, obj_counter);
  putword(chan, size_32);
  putword(chan, size_64);
  seek_out(chan, final_loc);
  stat_free((char *) extern_table);
  return Val_unit;
}
