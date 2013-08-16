---
layout: index
---

The current version 2.10 of Moscow ML

  * implements the full Standard ML language, as revised 1997, 
    including Modules and some extensions
  * yet is backwards compatible with versions prior to 2.00
  * implements large parts of the new SML Basis Library
  * implements separate compilation 
  * can produce compact stand-alone executables (a la Caml Light)
  * supports quotations and antiquotations, useful for metaprogramming
  * supports dynamic linking of external functions under Linux (x86, ARM,
    and Alpha), FreeBSD, NetBSD, Solaris, Digital Unix, HP-UX,
    MacOS, and MS Windows'95/98/NT/XP/Vista and possibly also 7 and 8. 

## New in version 2.10 of Moscow ML

  * Support for dynamic linking and callback from C also under MacOS X
  * There may now be 2 G globals (string literals etc) instead of 64 K
  * Bugs fixed:
    * The HTML files generated for mosmllib now have valid URLs
    * Substring.app more efficient
    * Recursive structure compilation (elabRecSigExp) fixed
    * Several errors in Mosmlcookie
    * Misplaced double quote in Msp.ahrefa 
    * Exception Option wasn't available at top-level
    * Linker did not check stamps of linked-in units
    * Hash function now much faster on very long strings
    * Double alignment constraints now correct with gcc-3.2 and Solaris
    * Bug in Polyhash.filter
    * Socket library returned only first 16 bytes of UDP datagram
  * SML Basis Library changes:
    * String and Substring: added concatWith, isSuffix, isSubstring, full
    * ListPair: added zipEq, appEq, mapEq, foldlEq, foldrEq, allEq
    * Added ArraySlice and VectorSlice structures, also for Char and Word8
    * Added find, findi, all, exists, collate to vector, array and 
      slice structures
    * Added collate to List
    * Added update to vector structures
    * Path.{mkRelative,mkAbsolute} now take record arguments
    * Negative Time.time values allowed
    * OS.Process: new functions sleep and isSuccess; status not eqtype
    * Listsort: added eqclasses, merge, mergeUniq
    * Path: added functions isRoot, fromUnixPath, toUnixPath, and
      exception InvalidArc (bug report by Henning Niss)
    * TextIO: inputLine now have type instream -> string option
    * Array: added type abbreviation vector (bug report by Andrzej
      Wasowski)
    * FileSys: type access renamed to access_mode (bug report by
      Henning Niss)
    * Byte: unpackString{,Vec} now uses vector slices (bug report by
      Henning Niss)
    * Unix: added functions fromStatus, textInstreamOf, binInstreamOf,
      textOutstreamOf, binOutstreamOf, exit, and added phantom types
      to proc (bug report by Henning Niss)
    * Timer: added function checkCPUTimes
    * Word and Word8: added functions toLarge, toLargeX, and fromLarge
      (bug report by Martin Elsman)
    * General: added exception Span and made the type of the function
      'before' less general (bug report by Henning Niss)
  * Added modules to mosmllib:
    * Buffer: mutable string buffers for fast and efficient
      concatenation of strings
    * Hashset: sets implemented by hash-tables
    * Rbset: ordered sets implemented by red-black trees
    * Redblackmap: maps implemented by red-black trees
  * Moscow ML is now developed in the open at github.


## System Requirements

Compilation under Unix is best done using GNU make, gcc, and Perl.  A
binary installation requires 5 MB disk space; a source installation
requires 25 MB disk space.

## Authors and credits

Moscow ML was created by Sergei Romanenko at the Keldysh Institute of Applied Mathematics, Russian Academy of Sciences, Moscow, Claudio Russo (then at Edinburgh University, now at Microsoft Research, Cambridge UK), Niels Kokholm at the IT University of Copenhagen (Moscow ML for .Net), Ken Friis Larsen at Department of Computer Science, University of Copenhagen, Denmark, and Peter Sestoft at the IT University of Copenhagen formerly at the Royal Veterinary and Agricultural University, Copenhagen, Denmark.

Moscow ML uses the entire runtime system and many other ideas from the Caml Light implementation created by Xavier Leroy and Damien Doligez.

Doug Currie created the MacOS port and considerably improved the bytecode interpreter.

## Availability


  * The Moscow ML home page is at <http://mosml.org>
  * The Moscow ML github page is at <https://github.com/kfl/mosml>
  * More to come ...
