(* OS.FileSys -- SML Basis Library *)

type dirstream

val openDir   : string -> dirstream
val readDir   : dirstream -> string option
val rewindDir : dirstream -> unit
val closeDir  : dirstream -> unit

val chDir     : string -> unit
val getDir    : unit -> string
val mkDir     : string -> unit
val rmDir     : string -> unit
val isDir     : string -> bool

val realPath  : string -> string
val fullPath  : string -> string
val isLink    : string -> bool
val readLink  : string -> string

val modTime   : string -> Time.time
val setTime   : string * Time.time option -> unit
val remove    : string -> unit
val rename    : {old: string, new: string} -> unit

datatype access = A_READ | A_WRITE | A_EXEC
val access    : string * access list -> bool

val fileSize  : string -> int

val tmpName   : unit -> string

eqtype file_id
val fileId    : string -> file_id
val hash      : file_id -> word
val compare   : file_id * file_id -> order

(* 
   These functions operate on the file system.  They raise OS.SysErr
   in case of errors.

   [openDir p] opens directory p and returns a directory stream for
   use by readDir, rewindDir, and closeDir.  Subsequent calls to
   readDir will return the directory entries in some unspecified
   order.

   [readDir dstr] returns SOME(s), consuming an entry s from the
   directory stream if it is non-empty; returns NONE if it is empty
   (when all directory entries have been read).  Only entries distinct
   from the parent arc and the current arc (that is, .. and . in Unix,
   DOS, and Windows; see the Path structure) will be returned.  

   [rewindDir dstr] resets the directory stream as if it had just been
   opened.

   [closeDir dstr] closes the directory stream.  All subsequent
   operations on the stream will raise OS.SysErr.

   [chDir p] changes the current working directory to p.  This affects
   calls to the functions use, load, compile in the interactive
   system, as well as all functions defined in this library.  If p
   specifies a volume name, then this command also changes the current
   volume (relevant under DOS, Windows, OS/2, etc.).  

   [getDir ()] returns the name of the current working directory.

   [mkDir p] creates directory p on the file system.

   [rmDir p] removes directory p from the file system.

   [isDir p] tests whether p is a directory.

   [fullPath p] returns a canonical form of path p, where all
   occurrences of the arcs ".", "..", "" have been expanded or
   removed, and (under Unix) symbolic links have been fully expanded.
   Raises SysErr if a directory on the path, or the file or directory
   named, does not exist or is not accessible, or if there is a link
   loop.

   [realPath p] behaves as fullPath(p) if p is absolute.  If p is
   relative and on the same volume as the current working directory,
   it returns a canonical path relative to the current working
   directory, where superfluous occurrences of the arcs ".", "..", ""
   have been removed, and (under Unix) symbolic links have been fully
   expanded.  Raises SysErr if a directory on the path, or the file or
   directory named, does not exist or is not accessible, or if there
   is a link loop.  Raises Path if p is relative and on a different
   volume than the current working directory.

   [isLink p] returns true if p names a symbolic link.  Raises SysErr
   if the file does not exist or there is an access violation.  On
   operating systems without symbolic links, it returns false, or
   raises SysErr if the file does not exist or there is an access
   violation.

   [readLink p] returns the contents of the symbolic link p.  Raises
   SysErr if p does not exist or is not a symbolic link, or there is
   an access violation.  On operating systems without symbolic links,
   it raises SysErr.

   [modTime p] returns the modification time of file p.

   [setTime (p, tmopt)] sets the modification and access time of file
   p.  If tmopt is SOME t, then the time t is used; otherwise the
   current time, that is, Time.now(), is used.

   [remove p] deletes file p from the file system.

   [rename {old, new}] changes the name of file `old' to `new'.

   [access] is the type of access permissions:

   [A_READ] specifies read access.

   [A_WRITE] specifies write access.

   [A_EXEC] specifies permission to execute the file (or directory).

   [access (p, accs)] tests the access permissions of file p,
   expanding symbolic links as necessary.  If the list accs of
   required access permission is empty, it tests whether p exists.  If
   accs contains A_READ, A_WRITE, or A_EXEC, respectively, it tests
   whether the user process has read, write, or execute permission for
   the file.  
       Under Unix, the access test is done with the `real' user
   id and group id (as opposed to the `effective' user id and group
   id) of the user process.  Hence access("file", [A_READ]) may return
   false, yet the file may be readable by the process, in case the
   effective user id or group id has been changed by setuid.

   [fileSize p] return the size, in bytes, of the file p.  Raises SysErr 
   if p does not exist or its directory is not accessible.

   [tmpName ()] returns a file name suitable for creating a fresh
   temporary file.  Note that there is no guarantee that the file name
   will be unique, since a file of that name may be created between
   the call to tmpName and a subsequent call to openOut which creates
   the file.  The file name will be absolute, usually of the form
   /tmp/xxxxxxxx provided by POSIX tmpnam (3).

   [file_id] is the type of unique identities of file system objects
   (including device ids and volume ids, but possibly insensitive to
   volume changes on removable volumes, such as tapes and diskettes).
   The set of file ids is equipped with a total linear order.

   [fileId p] returns the file_id of the file system object named by
   path p.  It holds that fileId p1 = fileId p2 if and only if p1 and
   p2 name the same file system object.
   
   [hash fid] returns a hashvalue for fid, suitable for use in a
   hashtable of file ids (and hence files).  
   If fid1 = fid2 then hash fid1 = hash fid2.

   [compare (fid1, fid2)] returns LESS, EQUAL, or GREATER, according
   as fid1 precedes, equals, or follows fid2 in the total linear order
   on file ids.  This is suitable for e.g. an ordered binary tree of
   file ids (and hence files).  
*)
