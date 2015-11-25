(* Purpose: find a sum from 0 to n
 * Examples:
 * summ 0 ==> 0
 * summ 2 ==> 3
 *)

fun summ (x : int) : int = 
	case x of
		0 => 0
	|	_ => x + summ(x - 1)

(* Tests for summ *)
val 0 = summ 0
val 3 = summ 2


(* Purpose: printing hahahahahahaha
 * Examples:
 * ha 0 ==> ""
 * ha 2 ==> "haha"
 *)

fun ha (x : int) : string = 
	case x of
	  0 => ""
	| _ => "ha" ^ ha(x - 1)

(* Tests for ha *)
val "" = ha 0
val "haha" = ha 2

(* Purpose: determine whether the number is odd
 * Examples:
 * oddP 0 ==> false
 * oddP 3 ==> true
 * oddP 12 ==> false
 * oddP 27 ==> true
*)
fun oddP (n : int) : bool =
    case n of
      0 => false
    | 1 => true
    | _ => oddP (n - 2)

(* Tests for oddP *)
val true = oddP 3
val false = oddP 0
val true = oddP 27
val false = oddP 12

(* Purpose: determine whether the number is divisible by three
 * Examples:
 * divisibilityByThree 1 ==> false
 * divisibilityByThree 3 ==> true
 * divisibilityByThree 11 ==> false
 * divisibilityByThree 27 ==> true
*)
fun divisibilityByThree (n : int) : bool =
	case n of
      0 => true
    | 1 => false
    | 2 => false
    | _ => divisibilityByThree (n - 3)

(* Tests for divisibilityByThree *)
val true = divisibilityByThree 0
val true = divisibilityByThree 6
val false = divisibilityByThree 11
val false = divisibilityByThree 25


(* Purpose: computing sum of a pair of natural numbers
 * Examples:
 * add (1,5) ==> 6
 * add (0,0) ==> 0
 * add (3,4) ==> 7
 * add (2,7) ==> 9
*)
fun add (x : int, y : int) : int =
	case x of
      0 => y
    | _ => 1 + add(x-1,y)

(* Tests for add *)
val 0 = add (0,0)
val 6 = add (4,2)
val 8 = add (3,5)
val 11 = add (6,5)
