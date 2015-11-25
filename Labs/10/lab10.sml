
(* copy tasks *)
fun copy inputs outputs =
    case TextIO.inputLine inputs of
      SOME v => let
                  val () = TextIO.output (outputs,v)
                in
                  copy inputs outputs
                end  
    | NONE   => ()

fun copyfiles (file1) (file2) =
    let
        val x  = TextIO.openOut file2
        val () = copy (TextIO.openIn file1) x
    in
        TextIO.flushOut(x)
    end


(* ---------------------------------------------------------------------- *)

(* map reduce tasks *)

signature MAP_REDUCE = 
sig
    type 'a mapreducable 
    val mapreduce : 
           ('a -> 'b)      (* handle single element *)
        -> 'b              (* result for empty *) 
        -> ('b * 'b -> 'b) (* merge results: assumed to be associative and commutative, with unit above *)
        -> 'a mapreducable -> 'b    
end

structure FileMR : MAP_REDUCE =
struct

    type 'a mapreducable = TextIO.instream
                           * (string -> 'a) (* parse a line as an 'a *)
        
    (* note: combines in reverse order, which is OK since n is commutative *)
    fun mapreduce (l : 'a -> 'b) (cur : 'b) (n : 'b * 'b -> 'b) (stream,parse) = 

    (*    do it for one file first

        one file

        (something to do with instream) *)
        case TextIO.inputLine stream of
          NONE   => cur
        | SOME v => let 
                      val cur =  n (l (parse v),cur)  (* How do I update this? *)
                    in
                      mapreduce l cur n (stream,parse)
                    end
end

fun stringconvert (s : string) : int =
    case Int.fromString s of
        NONE        => 0
      | SOME number => number

val numbersFromStdIn : int FileMR.mapreducable = (TextIO.stdIn, stringconvert)

fun add (somemap) =
    FileMR.mapreduce (fn x => x) (0) (fn (x,y) => x + y) (somemap)






