structure TreeDict =
struct

  datatype ('k, 'v) tree = Leaf | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree
  type ('k, 'v) dict = ('k, 'v) tree

  val empty = Leaf

  (* insert cmp (k1 ~ v1, ..., kn ~ vn) (k,v) 
        == (k1 ~ v1, ..., ki ~ v,...) if cmp(k,ki) ==> EQUAL for some ki
     or == (k1 ~ v1, ..., kn ~ vn, k ~ v) otherwise
     *)
  fun insert (cmp : ('k * 'k -> order)) (t : ('k, 'v) dict) ((m,n) : ('k * 'v)) : ('k, 'v) dict =
    case t of
      Leaf            => t
    | Node(l,(k,v),r) => (case cmp(k,m) of
                            EQUAL   => Node(l,(m,n),r)
                          | LESS    => Node((insert cmp l (m,n)),(k,v),r)
                          | GREATER => Node(l,(k,v),(insert cmp r (m,n)))
                                                                    )

  (* lookup cmp (k1 ~ v1,...,kn ~ vn) k == SOME vi 
                                           if cmp(k,ki) ==> EQUAL for some ki
                                        == NONE otherwise
     *)
  fun lookup (cmp : ('k * 'k -> order)) (t : ('k, 'v) dict) (key : 'k) : 'v option =
    case t of
      Leaf            => NONE
    | Node(l,(k,v),r) => (case cmp(k,key) of
                            EQUAL   => SOME v 
                          | LESS    => lookup cmp l key
                          | GREATER => lookup cmp r key
                                                                    )


end