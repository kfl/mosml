abstype 'a Stack =
    EMPTY
  | PUSH of 'a StackTop
withtype 'a StackTop = 'a * 'a Stack
with
  exception EmptyStack;
  val empty = EMPTY;
  fun top EMPTY = raise EmptyStack
    | top (PUSH(x, _)) = x;
  fun push x stack = PUSH(x, stack);
  fun pop EMPTY = raise EmptyStack
    | pop (PUSH(_, rest)) = rest;
end;

val st1 = push 3 (push 2 (push 1 empty));
val top1 = top st1;
val rest1 = pop st1;
