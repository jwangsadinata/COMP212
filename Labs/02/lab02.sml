(* Purpose: double the number n
 * Examples:
 * double 0 ==> 0
 * double 2 ==> 4
 *)
fun double (n : int) : int =
    case n of
      0 => 0
    | _ => 2 + double (n - 1)

(* Tests for double *)
val 0 = double 0
val 4 = double 2

(* Purpose: determine whether the number is even
 * Examples:
 * evenP 0 ==> true
 * evenP 3 ==> false
 * evenP 12 ==> true
 * evenP 27 ==> false
*)
fun evenP (n : int) : bool =
    case n of
      0 => true
    | 1 => false
    | _ => evenP (n - 2)

(* Tests for evenP *)
val true = evenP 0
val false = evenP 1
val true = evenP 12
val false = evenP 27

