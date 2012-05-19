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

signature FIXITY = 
sig
   type precedence
   type tok
   type result
   
   (* Internal state of the parser (allows for resumption) *)
   type partial_state (* Needs more input to resolve fixity *)
   type total_state (* Has enough input to resolve fixity *)

   (* Fixity resolution can't be given an empty list/stream *)
   exception EmptyFixity 
   exception TrailingInfix of tok * partial_state  (* "2 + 6 + 9 +" *)
   exception TrailingPrefix of tok * partial_state (* "2 + ~ 8 + 9 ~" *)
   exception SuccessiveInfix of tok * tok          (* "2 + + 8" *)
   exception PrefixInfix of tok * tok              (* "2 ~ + 6" *)
   exception ConsecutiveNonInfix of tok * tok      (* "2 = 3 = 5" *)
   exception MixedAssociation of tok * lrn * tok * lrn (* "a <- b -> c" *)
   exception PrefixPrecInfix of tok * tok          (* "~ 2 + 4", same prec. *) 
   exception Finished of total_state               (* "2 + 9 * 12" *)
end

functor SimplestFixityFn 
  (structure Precedence: ORDERED
   type tok = string
   type result = string):>
FIXITY where type precedence = Precedence.t 
         and type tok = tok
         and type result = result = 
struct
   type precedence = Precedence.t
   type tok = string
   type result = string


   (* Precedence with an extra top and bottom, and comparison *)

   datatype opt_prec = MIN | PREC of Precedence.t | MAX 

   fun compare x =
      case x of 
         (MIN, MIN) => EQUAL
       | (MIN, _) => LESS
       | (PREC _, MIN) => GREATER
       | (PREC prec1, PREC prec2) => Precedence.compare (prec1, prec2)
       | (MAX, MAX) => EQUAL
       | (_, MAX) => LESS  
       | (MAX, _) => GREATER

   fun eq x = EQUAL = compare x
   fun lt x = LESS = compare x
   fun leq x = GREATER <> compare x


   (* Generic stacks (snoc lists) and the stack items we'll put on them *)

   datatype 'a stack
     = Bot
     | $ of 'a stack * 'a

   infix 2 $

   datatype lrn = LEFT | RIGHT | NON
 
   datatype item = 
      DAT of result
    | PREFIX of Precedence.t * tok * (result -> result)
    | INFIX of Precedence.t * lrn * tok * (result -> result -> result)

   
   (* Exception interface *)

   type state = item stack * opt_prec
   type total_state = state
   type partial_state = state

   exception EmptyFixity
   exception TrailingInfix of tok * state
   exception TrailingPrefix of tok * state
   exception SuccessiveInfix of tok * tok
   exception PrefixInfix of tok * tok
   exception ConsecutiveNonInfix of tok * tok
   exception MixedAssociation of tok * lrn * tok * lrn
   exception PrefixPrecInfix of tok * tok
   exception Finished of state

   (* valid_stack checks for the well-formedness of a shift-reduce
    * parse stack.
    * 
    * A shift-reduce stack is well-formed if it represents a complete
    * parse in which the operators are (non-strictly) ordered from
    * lowest to highest. This invariant makes it impossible to parse
    * some technically unambiguous strings, like "- sin - 5", where
    * "-" is unary minus and "sin" is the unary sin function and the
    * two have different precedence. If unary minus has lower
    * precedence (binds less tightly) than sin, we must write "- sin
    * (- 5)". Alternatively, if unary minus has higher precedence
    * (binds more tightly) than sin, we must write "- (sin - 5)". (If
    * the two have the same precedence, then "- sin - 5" can parse
    * correctly.)
    *
    * Certainly, it would be possible to allow "- sin - 5" to parse
    * meaningfully, even if the two had different precedences, but the
    * resulting invariant would be more complicated, and just because
    * something is possible that doesn't mean it's a good idea. *)

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
          | S $ d1 $ INFIX (prec, _, _, _) (* $ d2 *) =>
               is_data d1 
               andalso leq (PREC prec, low) 
               andalso validate_at S (PREC prec)
          | _ => false
   in 
      case S of 
         Bot => false (* Empty stack is ill-formed *)
       | S $ d => is_data d andalso validate_at S MAX
   end


   (* Reduce a series of left-associative operations *)
   fun reduce_left (x: 'a) (ys: (tok * ('a -> 'a -> 'a) * 'a) list): 'a = 
      case ys of 
         [] => x
       | ((_, f, y) :: ys) => reduce_left (f x y) ys


   (* Reduce a series of right-associatve operations *)
   fun reduce_right (x: 'a) (ys: (tok * ('a -> 'a -> 'a) * 'a) list): 'a =
      case ys of 
         [] => x
       | ((_, f, y) :: ys) => f x (reduce_right y ys)


   (* reduce_infix_at_precedence
    * 
    * If we want to reduce an infix operator, we collect all the
    * operators at the top of the stack that have the same precedence
    * on the stack and reduce them at the same time. The resulting
    * stack will have a strictly lower maximum precedence.
    * 
    * Rough example (/ and * are both left associative with same fixity, 6):
    *
    * If we call
    * reduce_infix_at_precedence 
    *    (Bot $ 4 $ + $ 6 $ * $ 12 $ / $ 16) 
    *    LEFT * 6 [ ("/", ..., 9), ("*", ..., 2) ]
    *     
    * the result will be 
    * Bot $ 4 $ + $ ((((6*12)/16)/9)*2) 
    * *)

   fun reduce_infix_at_precedence S (running_prec, lrn, last_tok) xs = 
   let
      fun dispatch LEFT x xs = reduce_left x xs
        | dispatch RIGHT x xs = reduce_right x xs
        | dispatch NON x [ (_, f, y) ] = f x y
        | dispatch NON x [] = raise Fail "Invariant: xs must be nonempty"
        | dispatch NON x ((tok1, _, _) :: (tok2, _, _) :: _) = 
             raise ConsecutiveNonInfix (tok1, tok2)
   in 
    ( Assert.assert (fn () => valid_stack S)
    ; case S of 
         Bot $ DAT d => 
           ((* Finished: dispatch everything *)
            Bot $ DAT (dispatch lrn d xs))
       | S $ PREFIX (prec, tok, f) $ DAT d => 
           ((* Prefix: better be lower prec *)
            if lt (PREC prec, running_prec) 
            then S $ PREFIX (prec, tok, f) $ DAT (dispatch lrn d xs)
            else raise PrefixAtTheSamePrecedenceAsInfix (tok, last_tok))
       | S $ INFIX (prec, lrn', tok, f) $ DAT d2 =>
           ((* Infix: better be lower prec or the same associtivity *)
            if lt (PREC prec, running_prec) 
            then S $ INFIX (prec, lrn', tok, f) $ DAT (dispatch lrn d2 xs)
            else if ( Assert.assert (fn () => eq (PREC prec, running_prec))
                    ; lrn = lrn')
            then reduce_infix_at_precedence S (running_prec, lrn, tok) 
                    ((tok, f, d2) :: xs)
            else raise DifferentAssociativity (tok, lrn', last_tok, lrn))
       | _ => raise Fail "Impossible? (Should be precluded by assertion)")
   end


   (* If we want to add an operator to a valid stack, we have to make sure
    * everything further down in the stack has lower or equal precedence, 
    * reducing the overall precedence of the stack.
    * 
    * requires: valid_stack S
    * returns: (S', max_prec) where S' is another valid stack whose 
    *   maximum precedence is max_prec <= required. *)         

   fun reduce_precedence S required = 
    ( Assert.assert (fn () => valid_stack S)
    ; case S of 
         Bot $ d => (S, MIN)
       | S' $ PREFIX (prec, _, f) $ DAT d => 
           (if leq (PREC prec, required) 
            then (S, PREC prec)
            else reduce_precedence (S' $ DAT (f d)) required)
       | S' $ INFIX (prec, lrn, tok, f) $ DAT d2 => 
           (if leq (PREC prec, required) 
            then (S, PREC prec)
            else reduce_precedence
                    (reduce_infix_at_precedence S' (PREC prec, lrn, tok) 
                        [(tok, f, d2)])
                    required)
       | _ => raise Fail "Impossible? (Should be caught by assertion.)")


   (* shift is called on an arbitrary valid stack and list of tokens *)

   fun shift (state as (S, _)) xs = 
   ( Assert.assert (fn () => valid_stack S)
   ; if null xs 
     then raise Finished state
     else must_shift state xs)

   (* must_shift is called when either
    *   1 - we are starting with new input
    *   2 - the input is *known* to be nonempty, and S is valid
    *   3 - the input is a valid input + 1 infix + n prefix operators *)
   and must_shift (state as (S, max_stack_prec)) [] = 
         (case S of 
             Bot => raise EmptyFixity
           | _ $ INFIX (_, _, tok, _) => raise TrailingInfix (tok, state)
           | _ $ PREFIX (_, tok, _) => raise TrailingPrefix (tok, state)
           | _ => raise Fail "Invariant failed")
     | must_shift (S, max_stack_prec) (x :: xs) = raise Match
         
         
(*         case x of 
             DAT () => shift (S $ DAT ()) max_stack_prec
           | INFIX (prec, tok, _) => 
               (case S of 
                   _ $ INFIX (prec, tok, g) => 
         (if is_first 
          the *)

            
    

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
