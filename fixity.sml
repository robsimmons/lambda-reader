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

structure Assert = 
struct
   fun assert f = if f () then () else raise Fail "failed assertion"
(* fun assert f = () *)
end

structure Resolve = 
struct

   structure Datum = PosDatum

   structure Precedence = IntOrdered
   type tok = string

   exception EmptyFixity
   exception TrailingInfix of tok
   exception TrailingPrefix of tok
   exception SuccessiveInfix of tok * tok
   exception PrefixInfix of tok * tok

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

   datatype stackitem = 
      DAT of unit
    | PREFIX of Precedence.t * tok * unit
    | INFIX of Precedence.t * tok * unit

   datatype opt_prec = MIN | PREC of Precedence.t | MAX 

   fun lt (MIN, MIN) = false
     | lt (MIN, _) = true
     | lt (PREC _, MIN) = false
     | lt (PREC prec1, PREC prec2) = 
          LESS = Precedence.compare (prec1, prec2)
     | lt (PREC _, MAX) = true
     | lt (MAX, _) = false

   fun leq (MIN, _) = true
     | leq (PREC _, MIN) = false
     | leq (PREC prec1, PREC prec2) = 
          GREATER <> Precedence.compare (prec1, prec2) 
     | leq (PREC _, MAX) = true
     | leq (MAX, MIN) = false
     | leq (MAX, PREC _) = false
     | leq (MAX, MAX) = true

   (* valid_stack checks for the well-formedness of a shift-reduce
    * parse stack.
    * 
    * A shift-reduce stack is well-formed if it represents a complete
    * parse in which the operators are *strictly* ordered from lowest
    * to highest. This invariant makes it impossible to parse some
    * technically unambiguous strings, like "- sin - 5", where "-" is
    * unary minus and "sin" is the sin function. If unary minus has
    * lower precedence (binds less tightly) than sin, we must write "-
    * sin (- 5)". Alternatively, if unary minus has higher precedence
    * (binds more tightly) than sin, we must write "- (sin - 5)".
    *
    * Certainly, it would be possible to allow "- sin - 5" to parse 
    * meaningfully, but resulting invariant would be more complicated,
    * and just because something is possible that doesn't mean it's
    * a good idea. *)

   fun valid_stack S =
   let
      fun is_data x = 
         case x of 
            DAT _ => true
          | _ => false
   
      (* Validates a stack that has the low-prec data at the end removed *)
      fun validate_at S low = 
         case S of 
            Bot (* $ d1 *) => true
          | S $ PREFIX (prec, _, _) (* $ d1 *) =>
               leq (PREC prec, low) 
               andalso validate_at S (PREC prec) 
          | S $ d1 $ INFIX (prec, _, _) (* $ d2 *) =>
               is_data d1 
               andalso leq (PREC prec, low) 
               andalso validate_at S (PREC prec)
          | _ => false
   in 
      case S of 
         Bot => false (* Empty stack is ill-formed *)
       | S $ d => is_data d andalso validate_at S MAX
   end

   (* If we want to add an operator to a valid stack, we have to make sure
    * everything further down in the stack has lower precedence, effectively
    * reducing the overall precedence of the stack.
    * 
    * requires: valid_stack S
    * returns: (S', max_prec) where S' is another valid stack whose 
    *   maximum precedence is max_prec <= required. *)

   fun reduce_precedence S required = 
    ( Assert.assert (fn () => valid_stack S)
    ; case S of 
         Bot $ d => (S, MIN)
       | S' $ PREFIX (prec, _, f) $ d => 
           (if leq (PREC prec, required) then (S, PREC prec)
            else reduce_precedence (S' $ DAT ()) required)
       | S' $ d1 $ INFIX (prec, _, f) $ d2 => 
           (if leq (PREC prec, required) then (S, PREC prec)
            else reduce_precedence (S' $ DAT ()) required)
       | _ => raise Fail "Impossible? (Should be caught by assertion.)")


   (* must_shift is called when either
    *   1 - the input is *known* to be nonempty, and S is valid
    *   2 - the input is a valid input + 1 infix + n prefix operators *)
   fun must_shift S max_stack_prec [] = 
         (case S of 
             Bot => raise EmptyFixity
           | S $ INFIX (_, tok, _) => raise TrailingInfix tok
           | S $ PREFIX (_, tok, _) => raise TrailingPrefix tok
           | _ => raise Fail "Impossible by invariant?")
     | must_shift S max_stack_prec (x :: xs) = raise Match
         (*case x of 
             DAT () => shift (S $ DAT ()) max_stack_prec
           | INFIX (prec, _) => 
               (case S of 
                   _ $ INFIX (prec, g) => 
         (if is_first 
          the *)

   fun shift S max_stack_prec xs = 
   ( Assert.assert (fn () => valid_stack S)
   ; case xs of
        [] => 
        (case #1 (reduce_precedence S MIN) of 
            Bot $ DAT () => ()
          | _ => raise Fail "Impossible by reduce_precedence's postcondition")
      | _ => must_shift S max_stack_prec xs)
            
    

(*

   fun resolve S (DATUM datum) inputs = 
          (* Atoms just get thorwn on the stack *)
          next (S $ DATUM  
  

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

     | resolve (Bot $ datums) (

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
