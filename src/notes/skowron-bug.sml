(* From ss181292@zodiac.mimuw.edu.pl Tue Jan 15 15:12:02 2002
Date: Wed, 26 Dec 2001 12:55:26 +0100
From: "[iso-8859-2] Stanis³aw Skowron" <ss181292@zodiac.mimuw.edu.pl>
To: roman@keldysh.ru, Claudio.Russo@cl.cam.ac.uk, sestoft@dina.kvl.dk
Subject: BUG report

    [ The following text is in the "iso-8859-2" character set. ]
    [ Your display is set for the "iso-8859-1" character set.  ]
    [ Some characters may be displayed incorrectly. ]

                            Stanislaw Skowron
                            student of MIM departament, Warsaw
University
                            ul. Slowicza 12
                            05-807 Podkowa Lesna
                            POLAND
                            ss181292@zodiac.mimuw.edu.pl

                            December 26, 2001


Sergei Romanenko (roman@keldysh.ru)
Keldysh Institute of Applied Mathematics,
Russian Academy of Sciences

Claudio V. Russo (Claudio.Russo@cl.cam.ac.uk),
University of Cambridge.

Peter Sestoft (sestoft@dina.kvl.dk),
Department of Mathematics and Physics,
Royal Veterinary and Agricultural University


Bug report
When using Your "Moscow ML" I've encountered one bug. It's connected
with pseudo-polimorphic results of application and fact, it's treated,
in one
particular case, as real polymorphic value. The consequence is
Segmentation
fault.
I've prepared a script that generates the Segmentation fault error. The
important places are commented.
I'm using "Moscow ML version 2.00 (June 2000)" under Linux platform.
    [ Part 2: "Attached Text" ]

    [ The following text is in the "iso-8859-2" character set. ]
    [ Your display is set for the "iso-8859-1" character set.  ]
    [ Some characters may be displayed incorrectly. ]

*)

(* signature of partial order with type of elements `t' and
   relation `lessthan' *)
signature ORDER=
	sig
		type t
		val lessthan:t->t->bool
	end
;


(* signature of functional dictionary with type of keys `keyt' *)
signature DICTIONARY=
	sig
		type keyt
		type 'a dictt

		exception EMPTY
		val add:'a dictt->keyt->'a->'a dictt
		val rm:'a dictt->keyt->'a dictt
		val at:'a dictt->keyt->'a
		val empty:'a dictt
		val iskey:'a dictt->keyt->bool
		val to_list:'a dictt->(keyt*'a) list
		val from_list:(keyt*'a) list->'a dictt
	end
;


(* signature of imperative dictionary with type of elements `key' *)
signature IMPDICT =
	sig
		type key
		type 'a dict
		exception NOTFOUND
		val empty : unit -> 'a dict
		val add : 'a dict -> key -> 'a -> unit
		val mem : 'a dict -> key -> bool
		val find : 'a dict -> key -> 'a
		val remove : 'a dict -> key -> unit
		val from_list: (key*'a) list -> 'a dict
		val to_list: 'a dict -> (key*'a) list
		val copy : 'a dict -> 'a dict
	end
;


(* partial order on strings *)
structure STRING:ORDER=
	struct
		type t = string
		fun lessthan s1 s2 = (String.compare (s1, s2))=LESS
	end
;


(* BST implementation of functional dictionary *)
functor BSTDict(O:ORDER):DICTIONARY=
	struct
		type keyt = O.t
		datatype 'a dictt =
			NIL | NODE of ('a dictt)*(keyt*'a)*('a dictt)

		exception EMPTY;
		exception UNIMPLEMENTED;

		fun add NIL k v = NODE (NIL, (k, v), NIL)
		  | add (NODE (ld, (nk, nv), pd)) k v =
		  	if (O.lessthan k nk)
			then NODE (add ld k v, (nk, nv), pd)
			else if (O.lessthan nk k)
				then NODE (ld, (nk, nv), add pd k v)
				else NODE (ld, (k, v), pd);
		fun appright NIL d = d
		  | appright (NODE (ld, (k, v), pd)) d =
		  	NODE (ld, (k, v), appright pd d)
		;
		fun rm NIL k = raise EMPTY
		  | rm (NODE (ld, (nk, nv), pd)) k=
		  	if (O.lessthan k nk)
			then NODE ((rm ld k) handle EMPTY=> ld, (nk, nv), pd)
			else if (O.lessthan nk k)
				then NODE (pd, (nk, nv), (rm pd k)
							handle EMPTY=>pd)
				else
			let val NODE (lld, (lk, lv), lpd) = ld
			in NODE (lld, (lk, lv), appright lpd pd) end
		;
		fun at NIL k = raise EMPTY
		  | at (NODE (ld, (nk, nv), pd)) k =
		  	if (O.lessthan k nk)
			then at ld k
			else if (O.lessthan nk k)
				then at pd k
				else nv
		;
		val empty = NIL;
		fun iskey NIL k = false
		  | iskey (NODE (ld, (nk, nv), pd)) k =
		  	if (O.lessthan k nk)
			then iskey ld k
			else if (O.lessthan nk k)
				then iskey pd k
				else true
		;

		fun to_list' NIL og = og
		  | to_list' (NODE (ld, (k, v), pd)) og =
			(to_list' ld ((k, v)::(to_list' pd og)))
		;
		fun to_list l = to_list' l [];
		fun from_list [] = empty
		  | from_list ((k,v)::t) = add (from_list t) k v
		;
	end
;


(* functor converting functional dictionary into imperative dictionary *)
functor fun2imp(D:DICTIONARY):IMPDICT=
	struct
		type key = D.keyt
		type 'a dict = ('a D.dictt) ref
		exception NOTFOUND
		fun empty () = ref D.empty
		fun add d k v = d:=D.add (!d) k v
		fun mem d k = D.iskey (!d) k
		fun find d k = D.at (!d) k
		fun remove d k = d:=D.rm (!d) k
		fun from_list l = ref (D.from_list l)
		fun to_list d = D.to_list (!d)
		fun copy d = ref (!d)
	end
;


(* functor converting imperative dictionary into functional dictionary *)
functor imp2fun(D:IMPDICT):DICTIONARY=
	struct
		type keyt = D.key
		type 'a dictt = 'a D.dict
		exception EMPTY

		fun add d k v =
			let val new = D.copy d
			in D.add new k v; new end
		fun rm d k =
			let val new = D.copy d
			in D.remove new k; new end
		fun at d k = D.find d k
		val empty = D.empty ()

(* WARNING: `D.empty ()' is NOT truely polimorphic value,
            while it should be ! *)

		fun iskey d k = D.mem d k
		fun to_list d = D.to_list d
		fun from_list l = D.from_list l
	end
;



structure STR = BSTDict(STRING);
(* functional dictionary with type of keys `string' *)

structure ISTR = fun2imp(STR);
(* imperative dictionary with type of keys `string' *)

structure STR' = imp2fun(ISTR);
(* functional dictionary based on previous one *)


(* because we know that values of our functional dictionary
   from STR' structure are in fact values of imperative dictionary
   from ISTR structure, we can write something like that *)

ISTR.add STR'.empty "int" 1;

(* but now the real value of STR'.empty loses its polymorphism !!!
   in signature it is still polymorphic *)


(* so we can write something like that *)

STR'.add STR'.empty "str" "str";

(* witch causes `Segmentation fault'
   because STR'.empty is in fact `int dictt',
   but it is considered to be `'a dictt' *)
