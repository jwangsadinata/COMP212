
signature COUNTER =
sig

    (* invariant: counter always represents a natural number *)
    type counter 

    val zero : counter
    val increment : counter -> counter
    val value : counter -> int

end

structure TMICounter : COUNTER =
struct

    (* representation invariant: C x always satisfies x >= 0 *)
    type counter = int
        
    val zero = 0
    fun increment x = x + 1
    fun value x = x

end
val ~1 = TMICounter.value (TMICounter.increment ~2) 


structure IntCounter : COUNTER =
struct
    datatype counter = I of int

    val zero = (I 0)
    fun increment (I x) = (I (x + 1))
    fun value (I x) = x
end


signature BETTER_COUNTER =
sig

    structure C : COUNTER

    (* assuming n >= 0, increment c that many times *)
    val increment_many_times : C.counter * int -> C.counter

end

functor BetterCounter(C : COUNTER) : BETTER_COUNTER = 
struct
    structure C = C

    fun increment_many_times (count, acc) = 
        (case acc of
           0 => count
         | _ => increment_many_times (count,acc - 1))
end

(* ---------------------------------------------------------------------- *)

signature ORDERED =
sig

    type t
    val compare : t * t -> order

end

structure IntLt : ORDERED =
struct

    type t = int
    val compare = Int.compare

end

structure YMDOrder : ORDERED =
struct

    type t = int * (int * int)

    fun compare ((y,(m,d)),(y',(m',d')))  = 
        case Int.compare (y,y') of 
            LESS => LESS 
          | GREATER => GREATER
          | EQUAL => (case Int.compare (m,m') of
                          LESS => LESS
                        | GREATER => GREATER
                        | EQUAL => Int.compare (d,d'))
                          

end

val LESS = YMDOrder.compare ((1999,(1,1)), (1999,(1,2)))
val LESS = YMDOrder.compare ((1999,(1,3)), (1999,(2,2)))
val GREATER = YMDOrder.compare ((2000,(1,1)), (1999,(2,2)))

signature TWO_ORDERS =
sig
    structure O1 : ORDERED
    structure O2 : ORDERED
end

functor PairOrder(T : TWO_ORDERS) : ORDERED =
struct
    type t = T.O1.t * T.O2.t

    fun compare ((a,b),(a',b')) =
      (case T.O1.compare(a,a') of
          LESS    => LESS
        | GREATER => GREATER
        | EQUAL   => T.O2.compare(b,b'))
end

(* Task 3.2 *)
structure CompareIntPair : ORDERED = 
    PairOrder(struct
        structure O1 = IntLt
        structure O2 = IntLt end)

(* Task 3.3 *)
structure YMDOrderPair : ORDERED = 
    PairOrder
        (struct
        structure O1 = CompareIntPair
        structure O2 = IntLt end)