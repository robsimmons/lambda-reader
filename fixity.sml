(* Fixity as used by the lambda-reader *)
(*
signature FIXITY = 
sig
   type precedence (* Greater precedence binds tighter *)
   val compare = precedence * precedence -> order
   datatype fix = Left | Right | None | Prefix | Postfix
end

(* Concrete implementation of fixity from an ordered precedence type. *)
functor Fixity (Precedence: ORDERED):> 
   FIXITY where type precedence = Precedence.t = 
struct
   type precedence = Precedence.t
   open Precedence
   datatype fix = Left  | Right | None | Prefix | Postfix
   type t = fix
end

(* Various implementations of fixity *)
structure IntInfFixity = Fixity (IntInfOrdered)
structure IntFixity = Fixity (IntOrdered)
*)

structure Resolve = 
struct
   structure Datum = PosDatum

   structure Precedence = IntOrdered
   type tok = string
   datatype 'a fix = 
      Left of tok * Precedence.t * ('a list * 'a list -> 'a) 
    | Right of tok * Precedence.t * ('a list * 'a list -> 'a)
    | None of tok * Precedence.t * ('a list * 'a list -> 'a)
    | Prefix of tok * Precedence.t * ('a list -> 'a)
    | Postfix of tok * Precedence.t * ('a list -> 'a)

   val fake = ""

   datatype listitem = 
      DATUM of string Datum.t
    | OPER of string Datum.t fix option (* NONE is an application *)

   fun binary con prec sym = 
      OPER (SOME (con (sym, prec, fn (xs, ys) => 
                                     Datum.List [(sym, xs, Datum.poss xs),
                                                (sym, ys, Datum.poss ys)])))

   fun binary_swap con prec sym = 
      OPER (SOME (con (sym, prec, fn (xs, ys) => 
                                     Datum.List [(sym, ys, Datum.poss ys), 
                                                 (sym, xs, Datum.poss xs)])))

   fun unary con prec sym = 
      OPER (SOME (con (sym, prec, fn xs => 
                                     Datum.List [(sym, xs, Datum.poss xs)])))

   fun fixity datum = 
      case datum of
         Datum.Atom ("o-", pos) => binary_swap Left  1 "-o"
       | Datum.Atom ("@-", pos) => binary_swap Left  1 "-@"
       | Datum.Atom ("<-", pos) => binary_swap Left  1 "->"
       | Datum.Atom ("-o", pos) => binary      Right 2 "-o" 
       | Datum.Atom ("-@", pos) => binary      Right 2 "-@"
       | Datum.Atom ("->", pos) => binary      Right 2 "->"
       | Datum.Atom ("*", pos)  => binary      Right 3 "*"
       | Datum.Atom ("&", pos)  => binary      Right 3 "&"
       | Datum.Atom ("#1", pos) => unary       Prefix 4 "#1"
       | Datum.Atom ("#2", pos) => unary       Prefix 4 "#2"
       | Datum.Atom ("!", pos) =>  unary       Prefix 4 "!"
       | Datum.Atom ("@", pos) =>  unary       Prefix 4 "@"
       | _ => DATUM datum

   datatype 'a stack
     = Bot
     | $ of 'a stack * 'a

   infix 2 $
 
   (* Ensure that an application always falls between two adjacent tokens *)
   fun juxta y [] = y :: []
     | juxta (DATUM x) (DATUM y :: ys) = 
          DATUM x :: OPER NONE :: juxta (DATUM y) ys
     | juxta x (y :: ys) = x :: juxta y ys

(*
   datatype stackitem = 
      DAT of string Datum.t
    | LST of string Datum.t stack
    | OPR of (tok * string Datum.t fix) option (* NONE is an application *)

   fun resolve S (DATUM (NONE, datum)) inputs = 
          (* Atoms just get thrown on the stack. *)
          next (S $ DAT datum) datums

     | resolve S (DATUM (Prefix (prec, oper))) inputs = 
          (* Prefix operators just get shifted right away. *)
          next (S $ OPR (Prefix (prec, oper))) inputs

     | resolve (Bot $ datums) (OPER oper) inputs = 
          (* Shifting the operator is the only option! *)
          next (Bot $ datums $ OPR oper) inputs

     | resolve (S $ OPR (Postfix (_, f))) (OPER oper') inputs = 
          (* This deals with postfix; hopefully oper is a postfix! *)
          reduce (S $ OPR oper) (OPER oper' :: inputs)

     | resolve (S $ OPR _) (OPER oper') inputs = 
          
  
     | resolve (B

   fun reduce _ = raise Match

   and next (BOT $ _) [] = raise Fail "success"
     | next S [] = reduce S []
     | next S (input :: inputs) = resolve S input inputs
*)

(*
   (* *)  
   fun resolve S ((NONE, datum) :: datums) = 
          next (S $ DATUM (NONE, datum)
     | resolve S (()) =

 case (S, fixity datum) of 
         (S $ DATUM ) => 
       | 
*)
end

(* 
structure UserPrecedence = 
Fixity 
(struct
    datatype t =
       High of IntInf.int
     | UserDefined of IntInf.int
     | Low of IntInf.int 
 
    fun compare (High x, High y) = IntInf.compare (x, y)
     | compare (High x, _) = GREATER
     | compare (UserDefined x, High y) = LESS
     | compare (UserDefined x, UserDefined y) = IntInf.compare (x, y)
     | compare (UserDefined x, _) = GREATER
     | compare (Low x, High y) = LESS
     | compare (Low x, UserDefined y) = LESS
     | compare (Low x, Low y) = IntInf.compare (x, y)

   fun eq (x, y) = compare (x, y) = EQUAL
end

structure UserFixity = Fixity (UserPrecedence)

(* Verify that the implementations meet the specification *)
structure Test:> FIXITY where type precedence = IntInf.int = IntInfFixity 
structure Test:> FIXITY where type precedence = Int.int = IntFixity 
structure Test:> FIXITY where type precedence = UserPrecedence.t = UserFixity
structure Test = struct end
*)
