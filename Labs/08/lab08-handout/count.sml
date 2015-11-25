structure Count =
struct

    (* break a string into a list of words*)
    fun words (s : string) : string list = (String.tokens Char.isSpace s)

    (* derived form: update a key if it is already there *)
    fun increment (d : (string,int) TreeDict.dict) (s : string) : (string,int) TreeDict.dict = 
        case (TreeDict.lookup String.compare d s) of
          SOME v => TreeDict.insert String.compare d (s,v + 1)
        | NONE   => TreeDict.insert String.compare d (s,1)

    fun count (tweets : string list) (d : (string,int) TreeDict.dict) : (string,int) TreeDict.dict = 
        List.foldr (fn (x,tree) => increment tree x) TreeDict.empty tweets


(* List.foldr is like reduce for lists from the right associative (fancy, eh) 
    val it = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    *)
(*
    val test_dict = count ["hi there", "how are you today", "I'm fine how are you"] TreeDict.empty
    val SOME 1 = TreeDict.lookup String.compare test_dict "hi";
    val SOME 1 = TreeDict.lookup String.compare test_dict "there";
    val SOME 2 = TreeDict.lookup String.compare test_dict "how";
    val SOME 2 = TreeDict.lookup String.compare test_dict "are";
    val SOME 2 = TreeDict.lookup String.compare test_dict "you";
    val SOME 1 = TreeDict.lookup String.compare test_dict "today";
    val SOME 1 = TreeDict.lookup String.compare test_dict "I'm";
    val SOME 1 = TreeDict.lookup String.compare test_dict "fine";
*)

end