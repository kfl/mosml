(* OS.Path -- SML Basis Library *)

exception Path

val parentArc    : string
val currentArc   : string

val fromString   : string -> {isAbs : bool, vol : string, arcs : string list}
val toString     : {isAbs : bool, vol : string, arcs : string list} -> string

val getVolume    : string -> string 
val validVolume  : {isAbs : bool, vol : string} -> bool
val getParent    : string -> string

val isAbsolute   : string -> bool
val isRelative   : string -> bool
val mkAbsolute   : string * string -> string
val mkRelative   : string * string -> string

val concat       : string * string -> string

val mkCanonical  : string -> string
val isCanonical  : string -> bool

val splitDirFile : string -> {dir : string, file : string}
val joinDirFile  : {dir : string, file : string} -> string
val dir          : string -> string
val file         : string -> string

val splitBaseExt : string -> {base : string, ext : string option}
val joinBaseExt  : {base : string, ext: string option} -> string
val base         : string -> string    
val ext          : string -> string option

(* 
   This module provides OS-independent functions for manipulating
   strings that represent file names and paths in a directory
   structure.  None of these functions accesses the actual filesystem.
   
   Definitions: 

   * An arc denotes a directory or file.  Under Unix or DOS, an arc may
   have form "..", ".", "", or "abc", or similar.

   * An absolute path has a root: Unix examples include "/", "/a/b";
   DOS examples include "\", "\a\b", "A:\a\b".  

   * A relative path is one without a root: Unix examples include
   "..", "a/b"; DOS examples include "..", "a\b", "A:a\b".

   * A path has an associated volume.  Under Unix, there is only one
   volume, whose name is "".  Under DOS, the volume names are "",
   "A:", "C:", and similar.

   * A canonical path contains no occurrences of the empty arc "" or
   the current arc ".", and contains or the parent arc ".." only at
   the beginning and only if the path is relative.  

   * All functions (except concat) preserve canonical paths.  That is,
   if all arguments are canonical, then so will the result be.

   * All functions are defined so that they work sensibly on canonical 
   paths.

   * There are three groups of functions, corresponding to three ways
   to look at paths, exemplified by the following paths:

        Unix:    d/e/f/a.b.c       and     /d/e/f/a.b.c 
        DOS:     A:d\e\f\a.b.c     and     A:d\e\f\a.b.c     

   (1) A path consists of a sequence of arcs, possibly preceded by a
       volume and a root:

                          vol  [--- arcs ---]        vol  root  [--- arcs ---]
        ---------------------------------------------------------------------- 
        Unix examples:         d  e  f  a.b.c               /   d  e  f  a.b.c
        DOS examples:     A:   d  e  f  a.b          A:     \   d  e  f  a.b

   (2) A path consists of a directory part and a (last) file name part:

                          directory   file            directory  file 
        ------------------------------------------------------------------
        Unix examples:    d/e/f       a.b.c           /d/e/f     a.b.c
        DOS examples:     A:d\e\f     a.b             A:\d\e\f   a.b

   (3) A path consists of a base and an extension:

                          base       extension       base        extension
        ------------------------------------------------------------------
        Unix examples:    d/e/f/a.b      c           /d/e/f/a.b      c
        DOS examples:     A:d\e\f\a      b           A:\d\e\f\a      b


   GROUP 0: General functions on paths:

   [parentArc] is the arc denoting a parent directory: ".." under 
   DOS and Unix.

   [currentArc] is the arc denoting the current directory: "." under 
   DOS and Unix.

   [isRelative p] returns true if p is a relative path.

   [isAbsolute p] returns true if p is an absolute path.  
   Equals not (isRelative p).

   [validVolume {isAbs, vol}] returns true if vol is a valid volume
   name for an absolute path (if isAbs=true) resp. for a relative path
   (if isAbs=false).  Under Unix, the only valid volume name is "";
   under MS DOS and MS Windows the valid volume names are "", "a:",
   "b:", ..., and "A:", "B:", ...

   [getParent p] returns a string denoting the parent directory of p.
   It holds that getParent p = p if and only if p is a root. 

   [concat (p1, p2)] returns the path consisting of p1 followed by p2.
   Does not preserve canonical paths: concat("a/b", "../c") equals
   "a/b/../c".  This is because "a/b/../c" and "a/c" may not be
   equivalent in the presence of symbolic links.  Raises Path if p2 is
   not a relative path.

   [mkAbsolute(p1, p2)] returns the absolute path made by taking path
   p2, then p1.  That is, returns p1 if p1 is absolute; otherwise
   returns the canonicalized concatenation of p2 and p1.  Raises Path
   if p2 is not absolute (even if p1 is absolute).

   [mkRelative(p1, p2)] returns p1 relative to p2.  That is, returns
   p1 if p1 is already relative; otherwise returns the relative path
   leading from p2 to p1.  Raises Path if p2 is not absolute (and even
   if p1 is relative), or if p1 and p2 are both absolute but have
   different roots.

   [mkCanonical p] returns a canonical path which is equivalent to p.
   Redundant occurrences of the parent arc, the current arc, and the
   empty arc are removed.  The canonical path will never be the empty
   string; the empty path is converted to the current directory path
   ("." under Unix and DOS).  

   [isCanonical p] is equal to (p = mkCanonical p).


   GROUP 1: Manipulating volumes and arcs:

   [fromString p] returns {isAbs=false, vol, arcs} if the path p is
   relative, and {isAbs=true, vol, arcs} if the path p is absolute.
   In both cases vol is the volume name and arcs is the list of
   (possibly empty) arcs of the path.  Under Unix, the volume name is
   always the empty string ""; under DOS it will have form "A:", "C:",
   or similar.

   [toString path] reconstitutes a path from its root (if any) and
   arcs.  Raises Path if applied to a relative path whose first arc is
   empty.  It holds that toString(fromString p) = p, except that in MS
   DOS, slashes "/" in p will be replaced by backslashes "\".  It
   holds that fromString (toString p) = p when no exception is raised.
   It holds that isRelative(toString {isAbs=false, vol, arcs}) = true
   when no exception is raised.

   [getVolume p] returns the volume name of the path p, if given.
   Under Unix and MacOS, this is always the empty string "", and under
   MS DOS and MS Windows, it may have form "A:", "B:", ...


   GROUP 2: Manipulating directory paths and file names:

   [splitDirFile p] returns {dir, file} where file is the last arc in
   p, and dir is the path preceding that arc.  A typical use is to
   split a path into the directory part (dir) and the filename (file).

   [joinDirFile {dir, file}] returns the path p obtained by extending
   the path dir with the arc file.

   [dir p] equals #dir (splitDirFile p).

   [file p] equals #file (splitDirFile p).


   GROUP 3: Manipulating file names and extensions:

   [splitBaseExt s] returns {base, ext} where ext = NONE if s has no
   extension, and ext = SOME e if s has extension e; base is the part
   of s preceding the extension.  A path s is considered having no
   extension if its last arc contains no extension separator
   (typically ".") or contains an extension separator only as its
   leftmost character, or contains an extension separator as its
   right-most character.  Hence none of "a.b/cd", "a/.login", "a.",
   "..", "." and "." has an extension.

   [joinBaseExt {base, ext}] returns an arc composed of the base name
   and the extension (if different from NONE).  It is a left inverse
   of splitBaseExt, so joinBaseExt (splitBaseExt s) = s, but the
   opposite does not hold (since the extension may be empty, or may
   contain extension separators).

   [ext s] equals #ext (splitBaseExt s).

   [base s] equals #base (splitBaseExt s).
*)
