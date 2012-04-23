(* Fixity as used by the lambda-reader *)
signature FIXITY = 
sig
   type precedence

   type fix
   val Left: fix
   val Right: fix
   val None: fix
   val Prefix: fix
   val Postfix: fix

   type 'tok tokencharacter

   (* Token is a fixity operator w/ precedence: `_ + _` or `!_` or `_++`. *)
   val Fix: fix * precedence -> 'tok tokencharacter

   (* Token is the beginning of a binder - like `(` or `[` or `<` or `|` - that
      ends at the specified token - like `)` or `]` or `>` or `|`.
      @params Binder tok *)
   val Binder: 'tok -> 'tok tokencharacter

   (* Token is the beginning of a binder - like `Pi` or `Exists` or 
      `\` or `{` - that expects to read a piece of syntax, then the specified
      token - like `.` or `}` - and then read a second piece of syntax. *)
   val Bracket: 'tok -> 'tok tokencharacter
end

(* Concrete implementation of fixity from an ordered precedence type. *)
functor Fixity (Precedence: ORDERED) = 
struct
   type precedence = Precedence.t

   datatype fix = Left | Right | None | Prefix | Postfix

   datatype 'tok tokencharacter = 
      Fix of fix * precedence
    | Binder of 'tok 
    | Bracket of 'tok
end

(* Various implementations of fixity *)
structure IntInfFixity = Fixity (IntInfOrdered)
structure IntFixity = Fixity (IntOrdered)

structure UserPrecedence = 
struct
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
