(* we use the following hack to provide Real.== *)
structure Real = struct open Real fun == (x:real,y) = x = y end;

(*None for Chapter 1*)
use "sample2.sml";
use "sample3.sml";
use "sample4.sml";
use "sample5.sml";
(*None for Chapter 6*)
use "sample7.sml";
use "sample8.sml";
use "sample9.sml";
use "sample10.sml";
use "test10.sml";
quit();
