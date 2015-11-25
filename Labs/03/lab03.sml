(* you can remove this defintion when you're done to make sure you didn't
 * miss any functions
 *)
exception Unimplemented

fun evenP(n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP(n-2)


fun fib (n : int) : int =
    case n
     of ~1 => 0
      | 0 => 1
      | 1 => 1
      | _ => fib(n-1) + fib(n-2)

(* Task 1.1 *)

(* Purpose: filters out all odd elements in a list without changing the order
 * Examples:
 * evens [0,0,4] ==> [0,0,4]
 * evens [] ==> []
 * evens [0,0,4,9,3,2] ==> [0,0,4,2]
 *)

fun evens (n : int list) : int list =
	case n of
	  []    => []
	| x::xs => case evenP(x) of
				  true  => x::evens(xs)
				| false => evens(xs)

(* Tests for evens *)
val [0,0,4] = evens [0,0,4]
val [] = evens []
val [0,0,4,2] = evens[0,0,4,9,3,2]

(* Task 3.1 *)

(* Purpose: computes faster fibonacci sequence
 * Examples:
 * fastfib 5 ==> (3,5)
 * fastfib 3 ==> (1,2)
 * fastfib 2 ==> (1,1)
 *)

(* Define a helper function - Tuples(Pairs) addition *)
fun fibadd ((a : int, b : int)) : int * int = (b, a + b)

fun fastfib (n : int) : int * int =
	case n of
	  0 => (0,1)
	| _ => fibadd(fastfib(n - 1))

(* Good way to do it

fun fastfib (n : int) : int * int =
	case n of
	  0 => (0,1)
	| _ => let 
			 val (p : int, q : int) = fastfib(n - 1)
		   in
		   	 (q, p + q)
		   end
	*)

(* Tests for fastfib *)
val (3,5) = fastfib 4
val (2,3) = fastfib 3
val (1,1) = fastfib 1

(* Task 4.1 *)

(* Purpose: merges two sorted lists into one sorted list
 * Examples:
 * merge ([],[]) ==> []
 * merge ([1,2],[3,4]) ==> [1,2,3,4]
 * merge ([1,3,5],[2,4,6]) ==> [1,2,3,4,5,6]
 *)

fun merge (n : int list * int list) : int list =
	let
	  val (l1,l2) = n
	in
	  case (l1,l2) of
	    ([],[]) => []
	  | (x::xs,[]) => l1
	  | ([],y::ys) => l2
	  | (x::xs,y::ys)  => case (x < y) of
	  	  	    true => x::merge(y::ys,xs)
	    	  | _    => y::merge(x::xs,ys)
	end


(* Tests for merge *)
val [1,2,3,4,5,6] = merge ([1,3,5],[2,4,6])
val [1,2,3,4] = merge ([1,2],[3,4])
val [] = merge ([],[])
