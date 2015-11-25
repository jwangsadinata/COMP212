datatype 'a tree = 
	Empty
  | Node of 'a tree * (string * 'a) * 'a tree


fun lookup (t : 'a tree) (k : string) : 'a option = 
	case t of
		Empty                 => NONE
	  | Node(l,(key,value),r) => case key = k of
	  								true  => SOME value 
	  							  | false => (case lookup l k of
	  							  				SOME v => SOME v
	  							  			  | NONE => lookup r k)