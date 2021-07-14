# Unreleased

* SML Basis Library changes:
  * Added the structures `PackRealBig` and `PackRealLittle`
  * Added stop-gap implementation of the structure `Int64` and
    `Word64`. Stop-gap meaning that they don't fully follow the Basis
    Library specification, in particular there is no support for
    overloaded constant literals nor overloaded operations.

* Fixed some bugs in the configure part of the build systems, which
  meant that some features was sometimes disabled

* Updated the various test-suites so that they can be used to easily
  check if the compiler and mosmllib works.


# Version 2.10.1 (August 2014)

* ML Server Pages added to examples

* Build system cleaned up a bit


# Version 2.10 (August 2013)

* Support for dynamic linking and callback from C also under MacOS X

* There may now be 2 G globals (string literals etc) instead of 64 K

* Bugs fixed:
  * The HTML files generated for mosmllib now have valid URLs
  * `Substring.app` more efficient
  * Recursive structure compilation (elabRecSigExp) fixed
  * Several errors in `Mosmlcookie`
  * Misplaced double quote in `Msp.ahrefa`
  * Exception `Option` wasn't available at top-level
  * Linker did not check stamps of linked-in units
  * Hash function now much faster on very long strings
  * Double alignment constraints now correct with gcc-3.2 and Solaris
  * Bug in `Polyhash.filter`
  * `Socket` library returned only first 16 bytes of UDP datagram

* SML Basis Library changes:
  * `String` and `Substring`: added `concatWith`, `isSuffix`,
    `isSubstring`, `full`
  * `ListPair`: added `zipEq`, `appEq`, `mapEq`, `foldlEq`, `foldrEq`,
    `allEq`
  * Added `ArraySlice` and `VectorSlice` structures, also for `Char` and `Word8`
  * Added `find`, `findi`, `all`, `exists`, `collate` to `vector`, `array` and
    `slice` structures
  * Added `collate` to `List`
  * Added `update` to vector structures
  * `Path.{mkRelative,mkAbsolute}` now take record arguments
  * Negative `Time.time` values allowed
  * `OS.Process`: new functions `sleep` and `isSuccess`; `status` not `eqtype`
  * `Listsort`: added `eqclasses`, `merge`, `mergeUniq`
  * `Path`: added functions `isRoot`, `fromUnixPath`, `toUnixPath`, and
    exception `InvalidArc` (bug report by Henning Niss)
  * `TextIO`: `inputLine` now have type `instream -> string option`
  * `Array`: added type abbreviation `vector` (bug report by Andrzej
    Wasowski)
  * `FileSys`: type `access` renamed to `access_mode` (bug report by
    Henning Niss)
  * `Byte`: `unpackString{,Vec}` now uses vector slices (bug report by
    Henning Niss)
  * `Unix`: added functions `fromStatus`, `textInstreamOf`, `binInstreamOf`,
    `textOutstreamOf`, `binOutstreamOf`, `exit`, and added phantom types
    to proc (bug report by Henning Niss)
  * `Timer`: added function `checkCPUTimes`
  * `Word` and `Word8`: added functions `toLarge`, `toLargeX`, and `fromLarge`
    (bug report by Martin Elsman)
  * `General`: added exception Span and made the type of the function
    `before` less general (bug report by Henning Niss)

* Added modules to mosmllib:
  * `Buffer`: mutable string buffers for fast and efficient
    concatenation of strings
  * `Hashset`: sets implemented by hash-tables
  * `Rbset`: ordered sets implemented by red-black trees
  * `Redblackmap`: maps implemented by red-black trees

* Moscow ML is now developed in the open at github.


# Version 2.00 (June 2000)

* The full SML Modules language (structures, signatures, and functors)
  is now supported, thanks to Claudio Russo.  Also, several extensions
  to the SML Modules language are provided:
      - higher-order functors: functors may be defined within structures
        and functors
      - first-class modules: structures and functors may be packed and
        then handled as Core language values, which may then be unpacked
        as structures or functors again
      - recursive modules: signatures and structures may be recursively
        defined

* Value polymorphism has become friendlier: non-generalizable free
  type variables are left free, and become instantiated (once only)
  when the bound variable is used

* Added facilities for creating and communicating with subprocesses
  (structure `Unix` and `Signal` from SML Basis Library).

* Added facilities for efficient functional generation of HTML code
  (structure `Msp`); also supports the writing of ML Server Page scripts.

* Added facilities setting and accessing 'cookies' in CGI scripts
  (structure `Mosmlcookie`), thanks to Hans Molin, Uppsala, Sweden.

* The `Gdimage` structure now produces PNG images (using Thomas
  Boutell's gd library).


# Version 1.44 (August 1999)

## Added

* Access to GNU gdbm persistent hashtables (structures `Gdbm`, `Polygdbm`)

* Interface to the PostgreSQL database server (structure `Postgres`)

* Interface to the MySQL database server (structure `Mysql`)

* Interface to POSIX 1003.2 regular expressions (structure `Regex`)

* Interface to sockets (structure `Socket`)

* Faster bytecode execution (when compiled with GCC or egcs)

* Registration of ML and C functions simplify callbacks
