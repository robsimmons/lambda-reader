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


(* Simple syntactic datums *)

structure SimpleDatum = 
struct
   datatype 'tok datum = 
      Atom of 'tok 
    | List of ('tok * 'tok datum list) list
   type 'tok t = 'tok datum
end

functor SimpleDatumFn (type whitespace type pos):> 
   DATUM where type 'tok datum = 'tok SimpleDatum.datum
         and type whitespace = whitespace 
         and type pos = pos =
struct
   type whitespace = whitespace
   type pos = pos
   type 'tok datum = 'tok SimpleDatum.datum
   type 'tok t = 'tok datum
   fun atom (tok, _, _) = SimpleDatum.Atom tok
   fun list pieces = 
      SimpleDatum.List (map (fn (tok, _, datums, _) => (tok, datums)) pieces)
end


(* Syntactic datums carrying positional information *)

structure PosDatum = 
struct
   datatype 'tok datum = 
      Atom of 'tok * Pos.t
    | List of ('tok * 'tok datum list * Pos.t) list
   type 'tok t = 'tok datum
end

functor PosDatumFn (type whitespace):> 
   DATUM where type 'tok datum = 'tok PosDatum.datum
         and type whitespace = whitespace 
         and type pos = Pos.t = 
struct
   type whitespace = whitespace
   type pos = Pos.t
   type 'tok datum = 'tok PosDatum.datum
   type 'tok t = 'tok datum
   fun atom (tok, _, pos) = PosDatum.Atom (tok, pos)
   fun list pieces = 
      PosDatum.List
         (map (fn (tok, _, datums, pos) => (tok, datums, pos)) pieces)
end



(*
structure Test:> DATUM_SIMPLE = 
SimpleDatumFn (type whitespace = unit type pos = unit)
structure Test:> DATUM_POS = DatumPos

structure PosDatumFn (type whitespace) = 
struct
   type whitespace = whitespace
   type pos = Pos.t
   datatype 'tok datum = 
      Atom of 'tok * Pos.t
    | List of ('tok * 'tok datum list) list
   type 'tok t = 'tok datum
   fun atom (tok, _, pos) = Atom (tok, pos)
   fun list pieces = 
      List (map (fn (tok, _, datums, pos) => (tok, datums, pos)) pieces)
end
structure PosDatum = 
structure Test:> DATUM_POS = PosDatum

*)
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

