structure X  = struct type 'a r = 'a ref end: sig type 'a r = 'a ref end;
type 'a r = 'a ref;
structure Y  = struct type 'a r = 'a r end: sig type 'a r = 'a ref end;
type 'a r = 'a Y.r
structure Z  = struct type 'a r = 'a r end: sig type 'a r = 'a ref end;



