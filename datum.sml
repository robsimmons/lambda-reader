(* Abstract representation of syntax trees, lightly-parsed strings. *)
signature DATUM = 
sig 
   (* Representation of trailing whitespace (to reconstruct concrete syntax) *)
   type whitespace 

   (* Positions *)
   type pos

   (* Datum type *)
   type 'tok datum
   type 'tok t = 'tok datum

   (* Constructing datums *)
   val atom: 'tok * whitespace * pos -> 'tok datum
   val list: ('tok * whitespace * 'tok datum list * pos) list -> 'tok datum
end

signature DATUM_SIMPLE = DATUM where type whitespace = unit and type pos = unit
signature DATUM_POS = DATUM where type whitespace = unit and type pos = Pos.t

structure SimpleDatum =
struct
   type whitespace = unit
   type pos = unit
   datatype 'tok datum = Atom of 'tok | List of ('tok * 'tok datum list) list
   fun atom (tok, _, _) = Atom tok
   fun list pieces = 
      List (map (fn (tok, _, datums, _) => (tok, datums)) pieces)
end
structure Test:> DATUM_SIMPLE = SimpleDatum
structure Test:> DATUM_POS = SimpleDatum

structure PosDatum = 
struct
   type whitespace = unit
   type pos = Pos.t
   datatype 'tok datum = 
      Atom of 'tok * Pos.t
    | List of ('tok * 'tok datum list) list
   fun atom (tok, _, pos) = Atom (tok, pos)
   fun list pieces = 
      List (map (fn (tok, _, datums, pos) => (tok, datums, pos)) pieces)
end
structure Test:> DATUM_POS = PosDatum

   (* EXAMPLES:
    * 
    * 'A
    * -->
    * List [ ("'",[A]) ]
    * 
    * [A B C D]
    * -->
    * List [ ("[",[A,B,C,D]), ("]",[]) ]
    * 
    * (A B . C D)
    * -->
    * List [ ("(",[A,B]), (".",[C,D]), (")",[]) ]
    *
    * begin A middle B C late D end
    * -->
    * List [ ("begin",[A]), ("middle",[B,C]), ("late",[D]), ("end",[]) ]
    *
    * if A B C then D E F else G H I J
    * -->
    * List [ ("if",[A,B,C]), ("then",[D,E,F]), ("else",[G,H,I,J]) ]
    * 
    * EXISTS A B : C . D E 
    * -->
    * List [ ("EXISTS",[A,B]), (":",[C]), (".",[D,E]) ] *)

