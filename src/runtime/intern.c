/* Structured input, fast format */

#include <string.h>
#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"
#include "reverse.h"

/* Transform offsets relative to the beginning of the block 
   back into pointers. */

void adjust_pointers(value * start, mlsize_t size, color_t color)
{
  value * p, * q;
  mlsize_t sz;
  header_t hd;
  tag_t tag;
  value v;
  mlsize_t bosize;

  p = start;
  q = p + size;
  bosize = Bsize_wsize(size);
  while (p < q) {
    hd = *p;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    *p++ = Make_header(sz, tag, color);
    if (tag >= No_scan_tag)
      p += sz;
    else
      for( ; sz > 0; sz--, p++) {
        v = *p;
        switch(v & 3) {
        case 0:                 /* 0 -> A bloc represented by its offset. */
          Assert(v >= 0 && v <= bosize && (v & 3) == 0);
          *p = (value) ((byteoffset_t) start + v);
          break;
        case 2:                 /* 2 -> An atom. */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *p = Atom(v);
          break;
        default:                /* 1 or 3 -> An integer. */
          break;
        }
      }
  }
}

/* Reverse all words in a block, in case of endianness clash.
   Works with words of the natural word size. */

void rev_pointers(value * p, mlsize_t size)
{
  value * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_word(p);
    hd = (header_t) *p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n--, p++) {
        Reverse_word(p);
      }
    }
  }
}

#ifdef SIXTYFOUR

/* Routines to convert 32-bit externed objects to 64-bit memory blocks. */

typedef int32 value32;

/* Reverse all words in a block, in case of endianness clash.
   Works with 32-bit words. */

void rev_pointers_32(value32 * p, mlsize_t size)
{
  value32 * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_int32(p);
    hd = (header_t) *p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n--, p++) {
        Reverse_int32(p);
      }
    }
  }
}

/* Compute the size of the expansion of a 32-bit externed block to a
   64-bit block. The size is returned in 64-bit words. */

static mlsize_t size_after_expansion(value32 * p, mlsize_t len)
     /* len is the length in 32-bit words */
{
  mlsize_t res;
  value32 * q;
  header_t hd;
  mlsize_t n;

  for (q = p + len, res = 0; p < q; /*nothing*/) {
    hd = (header_t) *p++;
    res++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:            /* round to the next 64-bit word */
      res += (n * sizeof(value32) + sizeof(value) - 1) / sizeof(value);
      break;
    case Double_tag:
      res += sizeof(double) / sizeof(value);
      break;
    default:
      res += n;                 /* all fields will be extended 32 -> 64 */
      break;
    }
    p += n;
  }
  return res;
}

/* Convert a 32-bit externed block to a 64-bit block. The resulting block
   is a valid 64-bit object. */

static void expand_block(value32 * source, value * dest, mlsize_t source_len, mlsize_t dest_len, color_t color)
{
  value32 * p, * q;
  value * d, * e;
  header_t hd;
  mlsize_t sz;
  tag_t tag;
  uint32 * forward_addr;
  uint32 dest_ofs;
  value v;

  /* First pass: copy the objects and set up forwarding pointers.
     The pointers contained inside blocks are not resolved. */

  for (p = source, q = source + source_len, d = dest; p < q; /*nothing*/) {
    hd = (header_t) *p++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    forward_addr = (uint32 *) p;
    dest_ofs = d + 1 - dest;
    switch(tag) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = sz * sizeof(value32) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (sz * sizeof(value32) + sizeof(value) - 1) / sizeof(value);
        *d++ = Make_header(new_sz, String_tag, color);
        Field(d, new_sz - 1) = 0;
        bcopy((char *)p, (char *)d, len);
        ofs_last_byte = new_sz * sizeof(value) - 1;
        Byte(d, ofs_last_byte) = ofs_last_byte - len;
        p += sz;
        d += new_sz;
        break;
      }
    case Double_tag:
      *d++ = Make_header(Double_wosize, Double_tag, color);
      /* Cannot do *((double *) d) = *((double *) p) directly
         because p might not be 64-aligned. */
      Assert(sizeof(double) == sizeof(value));
      ((value32 *) d)[0] = p[0];
      ((value32 *) d)[1] = p[1];
      p += sizeof(double) / sizeof(value32);
      d += 1;
      break;
    default:
      *d++ = Make_header(sz, tag, color);
      for (/*nothing*/; sz > 0; sz--, p++, d++) {
        if ((*p & 1) == 0) {
          *d = *((uint32 *) p);         /* copy, zero expansion */
        } else {
          *d = *((int32 *) p);          /* copy, sign expansion */
        }
      }
      break;
    }
    *forward_addr = dest_ofs;   /* store the forwarding pointer */
  }
  Assert(d == dest + dest_len);

  /* Second pass: resolve pointers contained inside blocks,
     replacing them by the corresponding forwarding pointer. */

  for (d = dest, e = dest + dest_len; d < e; /*nothing*/) {
    hd = (header_t) *d++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    if (tag >= No_scan_tag) {
      d += sz;
    } else {
      for (/*nothing*/; sz > 0; sz--, d++) {
        v = *d;
        switch(v & 3) {
        case 0:                 /* 0: a block represented by its offset */
          Assert(v >= 0 && v < source_len * sizeof(value32) && (v & 3) == 0);
          *d = (value) (dest + *((uint32 *)((char *) source + v)));
          break;
        case 2:                 /* 2: an atom */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *d = Atom(v);
          break;
        default:                /* 1 or 3: an integer */
          break;
        }
      }
    }
  }
}

#else /* !SIXTYFOUR */

/* Routines to convert 64-bit externed objects to 32-bit memory blocks. */

struct value64_struct {
#ifdef MOSML_BIG_ENDIAN
  value msw, lsw;
#else
  value lsw, msw;
#endif
};
typedef struct value64_struct value64;

/* Reverse all words in a block, in case of endianness clash.
   Works with 64-bit words.
   Returns (-1) if a header too large is encountered, 0 otherwise. */

int rev_pointers_64(value64 * p, mlsize_t size)
     /* size is the size in 64-bit words */
{
  value64 * q;
  header_t hd;
  mlsize_t n;

  q = p + size;
  while (p < q) {
    Reverse_int64(p);
    hd = (header_t)(p->lsw);
    if (p->msw != 0) return -1;
    p++;
    n = Wosize_hd(hd);
    switch(Tag_hd(hd)) {
    case String_tag:
      p += n;
      break;
    case Double_tag:
      Reverse_double(p);
      p += n;
      break;
    default:
      for( ; n > 0; n --, p++) {
        Reverse_int64(p);
      }
    }
  }
  return 0;
}

/* Compute the size of the shrinkage of a 64-bit externed block to a
   32-bit block. The size is returned in 32-bit words.
   Return 0 if a block cannot be shrunk because its size is too big. */

static mlsize_t size_after_shrinkage(value64 * p, mlsize_t len)
     /* len is the length in 64-bit words */
{
  mlsize_t res;
  value64 * q;
  header_t hd;
  mlsize_t n;

  for (q = p + len, res = 0; p < q; /*nothing*/) {
    hd = (header_t)(p->lsw);
    if (p->msw != 0) return 0;
    p++;
    n = Wosize_hd(hd);
    res++;
    switch(Tag_hd(hd)) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = n * sizeof(value64) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (len + sizeof(value)) / sizeof(value);
        res += new_sz;
        break;
      }
    case Double_tag:
      res += sizeof(double) / sizeof(value);
      break;
    default:
      res += n;                 /* all fields will be shrunk 64 -> 32 */
      break;
    }
    p += n;
  }
  return res;
}

/* Convert a 64-bit externed block to a 32-bit block. The resulting block
   is a valid 32-bit object.
   Return -1 if the block cannot be shrunk because some integer literals
   or relative displacements are too large, 0 otherwise. */

static int shrink_block(value64 * source, value * dest, mlsize_t source_len, mlsize_t dest_len, color_t color)
{
  value64 * p, * q;
  value * d, * e;
  header_t hd;
  mlsize_t sz;
  tag_t tag;
  byteoffset_t * forward_addr;
  byteoffset_t dest_ofs;
  value v;

  /* First pass: copy the objects and set up forwarding pointers.
     The pointers contained inside blocks are not resolved. */

  for (p = source, q = source + source_len, d = dest; p < q; /*nothing*/) {
    hd = (header_t)(p->lsw);
    p++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    forward_addr = (byteoffset_t *) p;
    dest_ofs = d + 1 - dest;
    switch(tag) {
    case String_tag:
      { mlsize_t ofs_last_byte, len, new_sz;
        ofs_last_byte = sz * sizeof(value64) - 1;
        len = ofs_last_byte - Byte(p, ofs_last_byte);
        new_sz = (len + sizeof(value)) / sizeof(value);
        *d++ = Make_header(new_sz, String_tag, color);
        Field(d, new_sz - 1) = 0;
        bcopy(p, d, len);
        ofs_last_byte = new_sz * sizeof(value) - 1;
        Byte(d, ofs_last_byte) = ofs_last_byte - len;
        p += sz;
        d += new_sz;
        break;
      }
    case Double_tag:
      *d++ = Make_header(Double_wosize, Double_tag, color);
      Store_double_val((value)d, Double_val((value)p));
      p += sizeof(double) / sizeof(value64);
      d += sizeof(double) / sizeof(value);
      break;
    default:
      *d++ = Make_header(sz, tag, color);
      for (/*nothing*/; sz > 0; sz--, p++, d++) {
        value lsw = p->lsw;
        value msw = p->msw;
        if ((lsw & 1) == 0) {      /* If relative displacement: */
          if (msw != 0) return -1; /* Check unsigned displacement fits in 32 */
        } else {                   /* Otherwise, it's a signed integer */
          if ((lsw >= 0 && msw != 0) || (lsw < 0 && msw != -1)) return -1;
        }
        *d = lsw;
      }
    }
    *forward_addr = dest_ofs;   /* store the forwarding pointer */
  }
  Assert(d == dest + dest_len);

  /* Second pass: resolve pointers contained inside blocks,
     replacing them by the corresponding forwarding pointer. */

  for (d = dest, e = dest + dest_len; d < e; /*nothing*/) {
    hd = (header_t) *d++;
    sz = Wosize_hd(hd);
    tag = Tag_hd(hd);
    if (tag >= No_scan_tag) {
      d += sz;
    } else {
      for (/*nothing*/; sz > 0; sz--, d++) {
        v = *d;
        switch(v & 3) {
        case 0:                 /* 0: a block represented by its offset */
          Assert(v >= 0 && v < source_len * sizeof(value64) && (v & 7) == 0);
          *d = (value) (dest + *((byteoffset_t *)((char *) source + v)));
          break;
        case 2:                 /* 2: an atom */
          v = v >> 2;
          Assert(v >= 0 && v < 256);
          *d = Atom(v);
          break;
        default:                /* 1 or 3: an integer */
          break;
        }
      }
    }
  }
  return 0;
}

#endif /* SIXTYFOUR */

#ifdef MOSML_BIG_ENDIAN
#define Wrong_endian_32_magic_number Little_endian_32_magic_number
#define Wrong_endian_64_magic_number Little_endian_64_magic_number
#else
#define Wrong_endian_32_magic_number Big_endian_32_magic_number
#define Wrong_endian_64_magic_number Big_endian_64_magic_number
#endif

static value intern_fast_val(struct channel * chan, unsigned long magic)
{
  value res;
  mlsize_t whsize, wosize;
  unsigned long bhsize;
  color_t color;
  header_t hd;

  whsize = getword(chan);
  if (whsize == 0) {
    res = (value) getword(chan);
    if (Is_long(res))
      return res;
    else
      return Atom(res >> 2);
  }
  bhsize = Bsize_wsize (whsize);
  wosize = Wosize_whsize (whsize);
#ifdef SIXTYFOUR
  if (magic == Little_endian_32_magic_number ||
      magic == Big_endian_32_magic_number) {
    /* Expansion 32 -> 64 required */
    mlsize_t whsize32;
    value32 * block;
    whsize32 = whsize;
    block = (value32 *) stat_alloc(whsize32 * sizeof(value32));
    if (really_getblock(chan, (char *) block,
                        whsize32 * sizeof(value32)) == 0) {
      stat_free((char *) block);
      failwith ("intern : truncated object");
    }
    if (magic == Wrong_endian_32_magic_number)
      rev_pointers_32(block, whsize32);
    whsize = size_after_expansion(block, whsize32);
    wosize = Wosize_whsize(whsize);
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    expand_block(block, Hp_val(res), whsize32, whsize, color);
    stat_free((char *) block);
  } else {
    /* Block has natural word size (64) */
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (really_getblock(chan, Hp_val(res), bhsize) == 0) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      failwith ("intern : truncated object");
    }
    if (magic == Wrong_endian_64_magic_number)
      rev_pointers((value*)(Hp_val (res)), whsize);
    adjust_pointers((value*)(Hp_val (res)), whsize, color);
  }
#else /* !SIXTYFOUR */
  if (magic == Little_endian_64_magic_number ||
      magic == Big_endian_64_magic_number) {
    /* Shrinkage 64 -> 32 required */
    mlsize_t whsize64;
    value64 * block;
    whsize64 = whsize;
    block = (value64 *) stat_alloc(whsize64 * sizeof(value64));
    if (really_getblock(chan, (char *) block, 
                        whsize64 * sizeof(value64)) == 0) {
      stat_free((char *) block);
      failwith ("intern : truncated object");
    }
    if (magic == Wrong_endian_64_magic_number) {
      if (rev_pointers_64(block, whsize64) == -1) {
        stat_free((char *) block);
        failwith("intern: 64-bit object too big");
      }
    }
    whsize = size_after_shrinkage(block, whsize64);
    if (whsize == -1) {
      stat_free((char *) block);
      failwith("intern: 64-bit component not representable");
    }
    wosize = Wosize_whsize(whsize);
    if (wosize > Max_wosize)
      failwith("intern: structure too big");
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (shrink_block(block, (value*)(Hp_val(res)), whsize64, whsize, color) == -1) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      stat_free((char *) block);
      failwith("intern: 64-bit component not representable");
    }
    stat_free((char *) block);
  } else {
    /* Block has natural word size (32) */
    if (wosize > Max_wosize)
      failwith("intern: structure too big");
    res = alloc_shr(wosize, String_tag);
    hd = Hd_val (res);
    color = Color_hd (hd);
    Assert (color == White || color == Black);
    if (really_getblock(chan, Hp_val(res), bhsize) == 0) {
      Hd_val (res) = hd;                      /* Avoid confusing the GC. */
      failwith ("intern : truncated object");
    }
    if (magic == Wrong_endian_32_magic_number)
      rev_pointers((value*)(Hp_val (res)), whsize);
    adjust_pointers((value*)(Hp_val (res)), whsize, color);
  }
#endif /* !SIXTYFOUR */
  return res;
}

value intern_val(struct channel * chan)          /* ML */
{
  unsigned long magic;

  magic = (uint32) getword(chan);
  if (magic < First_valid_magic_number || magic > Last_valid_magic_number)
    failwith("intern: bad object");
  if (magic == Compact_magic_number)
    return intern_compact_val(chan);
  else
    return intern_fast_val(chan, magic);
}
