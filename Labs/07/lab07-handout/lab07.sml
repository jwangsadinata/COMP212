
val true = CM.make("sources.cm");

exception Unimplemented
val map = Seq.map

fun inteq(x,y) = case Int.compare (x,y) of EQUAL => true | _ => false
fun stringeq(x,y) = case String.compare (x,y) of EQUAL => true | _ => false

(* USE THESE FOR TESTING ONLY ! *)
fun seqFromList (l : 'a list) : 'a Seq.seq = 
    List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty ()) l
fun seqToList   (s : 'a Seq.seq) : 'a list = Seq.mapreduce (fn x => [x]) [] (op@) s

fun oddP (n : int) = inteq(n mod 2, 1)

fun seqFromList2 (l : 'a list list) : 'a Seq.seq Seq.seq = seqFromList (List.map seqFromList l)
fun seqToList2 (l : 'a Seq.seq Seq.seq) : 'a list list = seqToList (Seq.map seqToList l)

(* ---------------------------------------------------------------------- *)

(* Task *)

fun seqExists (f : 'a -> bool) : 'a Seq.seq -> bool =
	Seq.reduce (fn (b1,b2) => b1 orelse b2) false o (Seq.map f)

(* Tests *) 
val true  = seqExists oddP (seqFromList [1,2,3])
val false = seqExists oddP (seqFromList [2,4,6])


(* ---------------------------------------------------------------------- *)

(* Task *)

fun myAppend (s1 : 'a Seq.seq) (s2 : 'a Seq.seq) : 'a Seq.seq = 
	Seq.tabulate (fn i => (case i < Seq.length(s1) of 
							true => Seq.nth i s1
						  | _    => Seq.nth (i - Seq.length s1) s2))
								              (Seq.length s1 + Seq.length s2)


(* Tests *) 
val [1,2,3,4,5,6] = seqToList (myAppend (seqFromList [1,2,3]) (seqFromList [4,5,6]))
val [1,2,3] = seqToList (myAppend (seqFromList [1,2,3]) (seqFromList []))


(* Task *)
fun reverse (s : 'a Seq.seq) : 'a Seq.seq =
    Seq.tabulate (fn i => Seq.nth ((Seq.length s) - (i + 1)) s) (Seq.length s)

(* Task *)

(* assumes s is valid: 
   rectangular n x m where n,m > 0
   *)

fun transpose (s : 'a Seq.seq Seq.seq) : 'a Seq.seq Seq.seq = 
	Seq.tabulate (fn i => Seq.tabulate (fn j => Seq.nth i (Seq.nth j s)) (Seq.length s)) 
			(Seq.length (Seq.nth 0 s))

(* Tests *)
val [[1,4],[2,5],[3,6]] = seqToList2 (transpose (seqFromList2 [[1,2,3],[4,5,6]]))


(* ---------------------------------------------------------------------- *)

(* Stocks *)

val SOME minint = Int.minInt

(* compute a list whose elements are all the suffixes of the input 

val zip : (’a list * ’b list) -> (’a * ’b) list
val suffixes : ’a list -> (’a list) list

fun withSuffixes (l : int list) : (int * int list) list =
    zip (l, suffixes l)
fun bestGain (l : int list) : int =
    maxAll (map (fn (buy,sells) => (map (fn sell => sell - buy, sells)),
                 withSuffixes l))
fun maxL (l : int list) : int = reduce (Int.max , minint , l)
fun maxAll (l : (int list) list) : int = maxL (map (maxL, l))

Seq.reduce ('a.max, minint , s)
*)
(*
val suffixes : 'a Seq.seq -> 'a Seq.seq Seq.seq = *)

fun suffixes (s : 'a Seq.seq) : 'a Seq.seq Seq.seq =
	Seq.tabulate (fn i => Seq.tabulate (fn j => Seq.nth (j + i + 1) s) (Seq.length s - i - 1)) (Seq.length s)

(* Tests *)
val [[2,3,4],[3,4],[4],[]] = seqToList2 (suffixes (seqFromList [1,2,3,4]))

fun withSuffixes (s : 'a Seq.seq) = 
	Seq.zip(s, suffixes s)
(*
fun maxL (s : 'a Seq.seq) : 'a = raise Unimplemented
fun maxAll (s : 'a Seq.seq Seq.seq) : 'a = maxL (Seq.map (maxL, s))

fun bestGain (s : 'a Seq.seq) : 'a =
	maxAll (map (fn (buy,sells) => (map (fn sell => sell - buy, sells)), withSuffixes s))
*)
