(* Abstract representation of syntax trees, lightly-parsed strings. *)
(* Rob Simmons *)

(* DATUM is the signature that a parser should expect as an argument 
 * SimpleDatum and PosDatum are datatype representations of datums
 * SimpleDatumFn and PosDatumFn adapt their representations to DATUM *)

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
   (* Properly a conslist - the list must be nonempty *)
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

(* Adapts SimpleDatum to DATUM with any whitespace/pos type. *)
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
   fun pos datum = 
      case datum of 
         Atom (_, pos) => pos
       | List [] => raise Domain
       | List ((_, _, pos) :: parts) => 
         let fun loop pos' [] = Pos.union pos pos' 
               | loop _ ((_, _, pos') :: parts) = loop pos' parts
         in loop pos parts end 
   fun poss [] = raise Domain
     | poss (datum :: datums) =
       let val pos0 = pos datum 
          fun loop pos' [] = Pos.union pos0 pos' 
            | loop _ (datum :: datums) = loop (pos datum) datums
       in loop pos0 datums end 
end

(* Adapts PosDatum to DATUM with any whitespace type *)
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
