exception Unimplemented

(* map *)

fun map (f : 'a -> 'b, l : 'a list) : 'b list =
    case l of 
        [] => []
      | x :: xs => f x :: map (f , xs)

(* ---------------------------------------------------------------------- *)
(* filter *)

fun evenP (n : int) : bool = (n mod 2) = 0

fun evens (l : int list) : int list =
    case l of
	 [] => []
	| x :: xs => case evenP x of
			  true => x :: evens xs
			 |false => evens xs

fun keepUpper (l : char list) : char list =
    case l of 
        [] => []
      | x :: xs => (case Char.isUpper x of 
                        true => x :: keepUpper xs
                      | false => keepUpper xs)

(* TASK *)
fun filter (p : 'a -> bool, l : 'a list) : 'a list =
    case l of
      []      => []
    | x :: xs => (case p(x) of
                   true  => x :: filter(p,xs)
                 | false => filter(p,xs)       )

(* TASK *)
fun evens (l : int list) : int list = filter(evenP, l)
fun keepUpper (l : char list) : char list = filter(Char.isUpper, l)

val [#"B"] = keepUpper [#"a", #"B"]

(* TASK *)
fun quicksort_l (l : int list) : int list = 
    case l of
    []       => []
  | x::xs => let
               val (l1,l2) = (filter(fn a => a < x,xs),filter(fn a => a >= x,xs))
             in
               quicksort_l(l1) @ x::quicksort_l(l2)
             end

val [1,2,3,4,5,6,7] = quicksort_l([4,2,6,7,3,5,1])
val [2,3,4,5,6,7,8] = quicksort_l([8,7,6,5,4,3,2])
val [3,4,5,6,7,8,9] = quicksort_l([3,4,5,6,7,8,9])

(* ---------------------------------------------------------------------- *)
(* map and filter *)

(* TASK *)
fun ages_over_18 (l : (string * int) list) : (string * int) list = 
  case l of
    []      => []
  | (a,b) :: xs => case (2014 - b) > 18 of
                      true => (a,2014 - b) :: ages_over_18(xs)
                   | false => ages_over_18(xs)

fun ages_over_18 (l : (string * int) list) : (string * int) list = 
  filter(fn (_,x) => x > 18, (map(fn (y,z) => (y,2014 - z),l)))
 
val [("Sri",22),("Dan",32)] = ages_over_18 [("Sri",1992),("Dan",1982),("Cassie",2004)]


(* ---------------------------------------------------------------------- *)
(* all *)

fun allPos (l : int list) : bool =
    case l of
         [] => true
       | x :: xs => (x > 0) andalso allPos xs

fun allOfLength (len : int, l : 'a list list) : bool =
     case l of
          [] => true
         | x :: xs => ( (List.length x = len) andalso allOfLength(len, xs))

(* TASK: define a function named all *)
fun all (p : 'a -> bool, l : 'a list) : bool =
   case l of
     []      => true
   | x :: xs => p(x) andalso all(p,xs) 

(* TASK *)
fun allPos (l : int list) : bool = all(fn a => a > 0, l)
fun allOfLength (len : int, l : 'a list list) : bool = all(fn b => List.length b = len, l)

fun square(l : 'a list list) : bool = all(fn c => allOfLength(List.length l, l),l)

val true = square [[1,2],[3,4]]
val false = square [[1,2],[3]]
val false = square [[1,2],[3,4],[5,6]]
val true = square [[1,2,3],[4,5,6],[7,8,9]]

(* ---------------------------------------------------------------------- *)
(* reduce *)

fun sum (l : int list) : int = 
   case l of 
        [] => 0
      | x :: xs => x + (sum xs)

fun join (l : string list) : string = 
    case l of 
        [] => ""
      | x :: xs => x ^ join xs

(* TASK *)
fun reduce(c : 'a * 'a -> 'a, n : 'a, l : 'a list) : 'a =
    case l of
      []  => n
    | x :: xs => c(x,reduce(c,n,xs))

(* TASK *)
fun sum (l : int list) : int = reduce(fn (a,b) => a + b,0,l)
fun join (l : string list) : string = reduce(fn (c,d) => c ^ d,"",l)


(* ---------------------------------------------------------------------- *)
(* reduce *)

fun lines (s : string) : string list =
    (String.tokens (fn #"\n" => true | _ => false) s)

fun words (s : string) : string list =
    (String.tokens (fn #" " => true | #"\n" => true | _ => false) s)

(* TASK *)
fun wordcount (s : string) : int = sum(map(fn a => 1,words(s)))

fun longestline (s : string) : int = 
  let 
    val l = lines(s)
  in
    case l of
      [] => 0
    | x::[] => wordcount(x)  
    | x::y::xs => (case (wordcount(x) < wordcount(y)) of
                     true => wordcount(y)
                   | false => wordcount(x)   )
  end

fun longestline (s : string) : int =
   reduce(Int.max,0,map(wordcount,lines(s)))


val 7 = longestline "for life’s not a paragraph\nAnd death i think is no parenthesis\n"
val 12 = wordcount "for life’s not a paragraph\nAnd death i think is no parenthesis\n"
