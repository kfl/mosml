/* Structured output, fast format */

#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"

struct extern_obj * extern_table;
asize_t extern_table_size, extern_table_used;

void alloc_extern_table()
{
  asize_t i;

  extern_table = (struct extern_obj *)
    stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++)
    extern_table[i].obj = 0;
}

void resize_extern_table()
{
  asize_t oldsize;
  struct extern_obj * oldtable;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    h = Hash(oldtable[i].obj);
    while (extern_table[h].obj != 0) {
      h++;
      if (h >= extern_table_size) h = 0;
    }
    extern_table[h].obj = oldtable[i].obj;
    extern_table[h].ofs = oldtable[i].ofs;
  }
  stat_free((char *) oldtable);
}

byteoffset_t * extern_block;
asize_t extern_size, extern_pos;

static void resize_result()
{
  extern_size = 2 * extern_size;
  extern_block = (byteoffset_t *)
    stat_resize((char *) extern_block, extern_size * sizeof(byteoffset_t));
}

static byteoffset_t emit(v)
     value v;
{
  mlsize_t size;
  asize_t h;
  byteoffset_t res;
  value * p;
  byteoffset_t * q;
  asize_t end_pos;

  if (Is_long(v)) return (byteoffset_t) v;
  size = Wosize_val(v);
  if (size == 0) return (Tag_val(v) << 2) + 2;
  if (2 * extern_table_used >= extern_table_size) resize_extern_table();
  h = Hash(v);
  while (extern_table[h].obj != 0) {
    if (extern_table[h].obj == v) return extern_table[h].ofs;
    h++;
    if (h >= extern_table_size) h = 0;
  }
  end_pos = extern_pos + 1 + size;
  while (end_pos >= extern_size) resize_result();
  /* Consistently write the header's color, i.e. gc bits, to the file: */
  extern_block[extern_pos++] = Make_header(size, Tag_val(v), Black);
  res = extern_pos * sizeof(byteoffset_t);
  extern_table[h].obj = v;
  extern_table[h].ofs = res;
  extern_table_used++;
  for (p = &Field(v, 0), q = &extern_block[extern_pos]; size > 0; size--) {
    *q++ = *p++;
  }
  extern_pos = end_pos;
  return res;
}

byteoffset_t emit_all(value root)
{
  asize_t read_pos;
  byteoffset_t res;
  header_t hd;
  mlsize_t sz;
  byteoffset_t ofs;

  read_pos = extern_pos;
  res = emit(root);
  while (read_pos < extern_pos) {
    hd = (header_t) extern_block[read_pos++];
    sz = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
    case Double_tag:
      read_pos += sz;
      break;
    case Abstract_tag:
    case Final_tag:
      invalid_argument("output_value: abstract value");
      break;
    case Closure_tag:
      invalid_argument("output_value: functional value");
      break;
    default:
      while (sz > 0) {
        ofs = emit((value) extern_block[read_pos]);
        extern_block[read_pos] = ofs;
        read_pos++;
        sz--;
      }
      break;
    }
  }
  return res;
}

value extern_val(chan, v)       /* ML */
     struct channel * chan;
     value v;
{
  byteoffset_t res;

  extern_size = INITIAL_EXTERN_SIZE;
  extern_block =
    (byteoffset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  extern_table_used = 0;
  res = emit_all(v);
  stat_free((char *) extern_table);
  putword(chan, Extern_magic_number);
  putword(chan, extern_pos);
  if (extern_pos == 0)
    putword(chan, res);
  else
    putblock(chan, (char *) extern_block, extern_pos * sizeof(unsigned long));
  stat_free((char *) extern_block);
  return Val_unit;
}
