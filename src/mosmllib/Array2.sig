(* Array2 -- SML Basis Library *)

eqtype 'a array

datatype traversal = RowMajor | ColMajor

val array      : int * int * '_a -> '_a array
val fromList   : '_a list list -> '_a array
val tabulate   : traversal -> int * int * (int * int -> '_a) -> '_a array

val dimensions : 'a array -> int * int
val nCols      : 'a array -> int
val nRows      : 'a array -> int

val sub        : 'a array * int * int -> 'a
val update     : 'a array * int * int * 'a -> unit

val row        : 'a array * int -> 'a Vector.vector
val column     : 'a array * int -> 'a Vector.vector

type 'a region = { base : 'a array, row : int, col : int, 
                   nrows : int option, ncols : int option}

val copy       : { src : 'a region, dst : 'a array, 
                   dst_row : int, dst_col : int } -> unit

val app        : traversal -> ('a -> unit) -> 'a array -> unit
val modify     : traversal -> ('a -> 'a) -> 'a array -> unit
val fold       : traversal -> ('a * 'b -> 'b) -> 'b -> 'a array -> 'b

val appi       : traversal -> (int * int * 'a -> unit) -> 'a region -> unit
val modifyi    : traversal -> (int * int * 'a -> 'a) -> 'a region -> unit
val foldi      : traversal -> (int * int * 'a * 'b -> 'b) -> 'b 
                 -> 'a region -> 'b

(* 
   ['ty array] is the type of two-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type 'ty.  
   Type 'ty array admits equality even if 'ty does not.  Arrays a1 and a2 
   are equal if both were created by the same call to one of the
   primitives array, fromList, and tabulate.

   [traversal] is the type of traversal orders: row major or column major.

   [RowMajor] specifies that an operation must be done in row-major
   order, that is, one row at a time, from top to bottom, and from
   left to right within each row.  Row-major traversal visits the
   elements of an (m,n)-array with m rows and n columns in this 
   order:
            (0,0), (0,1), (0,2), ..., (0,n-1), 
            (1,0), (1,1), (1,2), ..., (1,n-1), 
                    ...
   that is, in order of lexicographically increasing (i, j).  In
   Moscow ML, row-major traversal is usually faster than column-major
   traversal.

   [ColMajor] specifies that an operation must be done in column-major
   order, that is, one column at a time, from left to right, and from
   top to bottom within each column.  Column-major traversal visits
   the elements of an (m,n)-array with m rows and n columns in this
   order:
            (0,0), (1,0), (2,0), ..., (m-1,0), 
            (0,1), (1,1), (2,1), ..., (m-1,1), 
                    ...
   that is, in order of lexicographically increasing (j, i).  

   [array(m, n, x)] returns a new m * n matrix whose elements are all x.  
   Raises Size if n<0 or m<0.

   [fromList xss] returns a new array whose first row has elements
   xs1, second row has elements xs2, ..., where xss = [xs1,xs2,...,xsm].  
   Raises Size if the lists in xss do not all have the same length.

   [tabulate RowMajor (m, n, f)] returns a new m-by-n array whose 
   elements are f(0,0), f(0,1), ..., f(0, n-1), 
                f(1,0), f(1,1), ..., f(1, n-1),
                    ...
                f(m-1,0),    ...,    f(m-1, n-1)
   created in row-major order: f(0,0), f(0,1), ..., f(1,0), f(1,1), ...
   Raises Size if n<0 or m<0.

   [tabulate ColMajor (m, n, f)] returns a new m-by-n array whose 
   elements are as above, but created in the column-major order:
   f(0,0), f(1,0), ..., f(0, 1), f(1, 1), ...  Raises Size if n<0 or m<0.

   [dimensions a] returns the dimensions (m, n) of a, where m is the
   number of rows and n the number of columns.

   [nCols a] returns the number of n of columns of a.

   [nRows a] returns the number of m of rows of a.

   [sub(a, i, j)] returns the i'th row's j'th element, counting from 0.
   Raises Subscript if i<0 or j<0 or i>=m or j>=n 
   where (m,n) = dimensions a.

   [update(a, i, j, x)] destructively replaces the (i,j)'th element of a 
   by x. Raises Subscript if i<0 or j<0 or i>=m or j>=n 
   where (m,n) = dimensions a. 

   [row (a, i)] returns a vector containing the elements of the ith
   row of a.  Raises Subscript if i < 0 or i >= height a.

   [column (a, j)] returns a vector containing the elements of the jth
   column of a.  Raises Subscript if j < 0 or j >= width a.

   [app RowMajor f a] applies f to the elements a[0,0], a[0,1], ...,
   a[0,n-1], a[1,0], ..., a[m-1, n-1] of a, where (m, n) = dimensions a.

   [app ColMajor f a] applies f to the elements a[0,0], a[1,0], ...,
   a[n-1,0], a[0,1], a[1,1], ..., a[m-1, n-1] of a, where (m, n) =
   dimensions a.

   [modify RowMajor f a] applies f to the elements a[0,0], a[0,1],
   ..., a[0,n-1], a[1,0], ..., a[m-1, n-1] of a, updating each element
   with the result of the application, where (m, n) = dimensions a.

   [modify ColMajor f a] applies f to the elements a[0,0], a[1,0],
   ..., a[n-1,0], a[0,1], a[1,1], ..., a[m-1, n-1] of a, updating each
   element with the result of the application, where (m, n) =
   dimensions a.

   [fold RowMajor f b a] folds f left-right and top-down over the
   elements of a in row-major order.  That is, computes
        f(a[m-1, n-1], f(a[m-1, n-2], ..., f(a[0,1], f(a[0,0], b)) ...))
   where (m, n) = dimensions a.

   [fold ColMajor f b a] folds f left-right and top-down over the
   elements of a in column-major order.  That is, computes
        f(a[m-1, n-1], f(a[m-2, n-1], ..., f(a[1,0], f(a[0,0], b)) ...))
   where (m, n) = dimensions a.


   The following iterators generalize the above ones in two ways:

     * the indexes i and j are also being passed to the function;
     * the iterators work on a region (submatrix) of a matrix.          

   [region] is the type of records { base, row, col, nrows, ncols }
   determining the region or submatrix of array base whose upper left
   corner has index (row, col).

   If nrows = SOME r, then the region has r rows: row, row+1, ..., row+r-1.
   If nrows = NONE, then the region extends to the bottom of the matrix.
   The field ncols similarly determines the number of columns.

   A region is valid for an array with dimensions (m, n) if 
       (1) either nrows = NONE and 0 <= row <= m 
           or nrows = SOME r and 0 <= row <= row + r <= m 
   and (2) either ncols = NONE and 0 <= col <= n
           or ncols = SOME c and 0 <= col <= col + c <= n.

   [appi RowMajor f reg] applies f to (i, j, a[i, j]) in order of
   lexicographically increasing (i, j) within the region reg.  Raises
   Subscript if reg is not valid.  Note that app tr f a is equivalent
   to appi tr (f o #3) {base=a, row=0, col=0, nrows=NONE, ncols=NONE}

   [appi ColMajor f reg] applies f to (i, j, a[i, j]) in order of
   lexicographically increasing (j, i) within the region reg.  Raises
   Subscript if reg is not valid.  

   [modifyi RowMajor f reg)] applies f to (i, j, a[i, j]) in order of
   lexicographically increasing (i, j) within the region reg.  Raises
   Subscript if reg is not valid.  Note that modify tr f a is equivalent 
   to modifyi (f o #3) {base=a, row=0, col=0, nrows=NONE, ncols=NONE}).

   [modifyi ColMajor f reg)] applies f to (i, j, a[i, j]) in order of
   lexicographically increasing (j, i) within the region reg.  Raises
   Subscript if reg is not valid.  

   [foldi RowMajor f b a] folds f over (i, j, a[i, j]) in row-major
   order within the region reg, that is, for lexicographically
   increasing (i, j) in the region.  Raises Subscript if reg is not
   valid.

   [foldi ColMajor f b a] folds f over (i, j, a[i, j]) in column-major
   order within the region reg, that is, for lexicographically
   increasing (j, i) in the region.  Raises Subscript if reg is not
   valid.

   [copy { src, dst, dst_row, dst_col }] copies the region determined
   by src to array dst such that the upper leftmost corner of src is
   copied to dst[dst_row, dst_col].  Works correctly even when src and
   dst are the same and the source and destination regions overlap.
   Raises Subscript if the src region is invalid, or if src translated
   to (dst_row, dst_col) is invalid for dst.
*)
