
structure Datum = 
struct
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

   datatype 'tok datum = 
      Atom of 'tok * Pos.t
    | List of ('tok * 'tok datum list * Pos.t) list 
   type 'tok t = 'tok datum

   (* The coordinate is the entire width of the datum, including the token. *)
   type 'tok piece = 'tok * 'tok datum list * Pos.t
end

functor ParseDatum (structure Datum: DATUM where type whitespace = unit 
                                             and type pos = Pos.t) = 
struct

   val unshiftable = ["."]
   val global_schema = Schema.scheme
   val eq = fn (x, y: string) => x = y
   val toString = fn x: string => x

   fun member [] y = false
     | member (x :: xs) y = eq (x, y) orelse member xs y

   fun lookup [] y = NONE
     | lookup ((x, data) :: xs) y = if eq (x, y) then SOME data else lookup xs y

   fun >>(f,g) = fn x => g(f(x))
   fun |>(x,f) = f x

   infixr 2 >>
   infix 1 |>
   
   datatype 'a stack
     = Bot
     | $ of 'a stack * 'a

   infix 2 $

   fun toList Bot ys = ys
     | toList (xs $ x) ys = toList xs (x :: ys)

   val toList = fn stack => toList stack []

   (* End of input reached without finishing multipart syntax begun at coord;
    * provides series of tokens that were expected next. *)
   exception EndOfInput of 
      Coord.coord * (string * Pos.t) Stream.front * string Schema.t

   fun parse_one ((tok, pos), str) local_unshifts
   : string Datum.t * Pos.t * (string * Pos.t) Stream.front  = 
     (case lookup global_schema tok of 
         SOME token_cont => 
            (* Time to begin with the parsings! *)
            parse_cont token_cont (Pos.left pos)
               Bot 
               (tok, pos)
               local_unshifts
               str 
       | NONE =>
            (* Oh, this is simple! *) 
            (Datum.atom (tok, (), pos), pos, Stream.front str))

   (* Dispatch:
    * cont - The Schema.tok_cont we're dispatching upon
    * coord0 - The initial coordinate for the very beginning of this multipart
    *    datum.
    * pieces - The complete pieces of this multipart datum that have already
    *    been parsed.
    * (tok, pos) - The local parts of the multipart datum.
    * local_unshifts - The unshiftable tokens in the surrounding context
    *    of the in-progress multipart datum.
    * str - the input stream *)
   and parse_cont cont coord0 pieces (tok, pos) local_unshifts str
   : string Datum.t * Pos.t * (string * Pos.t) Stream.front  = 
     (case cont of 
         Schema.DONE => 
            (Datum.list (toList pieces), 
             Pos.pos coord0 (Pos.right pos),
             Stream.front str)
       | Schema.MUST_SEE schema =>
            parse_must coord0 pieces (tok, Bot, pos) schema local_unshifts 
               (Stream.front str)
       | Schema.MAY_SEE schema => 
            parse_may coord0 pieces (tok, Bot, pos) schema local_unshifts
               (Stream.front str)
       | Schema.EXACTLY_ONE => 
            parse_exactly_one coord0 pieces (tok, pos) local_unshifts  
               (Stream.front str))

   (* Handles MUST_SEE continuations. 
    * The local_unshifts are temporarily irrelevant in the handling of 
    * MUST_SEE continuations; they will only become relevant if a later token
    * in the same multipart datum has an EXACTLY_ONE or MAY_SEE continuation. *)
   and parse_must coord0 pieces (tok, datums, pos) schema local_unshifts str
   : string Datum.t * Pos.t * (string * Pos.t) Stream.front  = 
     (case str of 
         Stream.Nil => raise EndOfInput (coord0, str, schema)
       | Stream.Cons (front as ((newtok, newpos), newstr)) => 
           (case lookup schema newtok of 
               SOME token_cont => (* Next piece of multipart will now begin! *)
                  parse_cont token_cont coord0
                     (pieces $ (tok, (), toList datums, pos)) 
                     (newtok, newpos)
                     local_unshifts
                     newstr
             | NONE => (* Not starting the next piece of the multipart. *)
                 (if member unshiftable newtok  
                  then raise EndOfInput (coord0, str, schema)
                  else (* Add another datum to this piece. *)
                  let val (datum, pos', str') = 
                        parse_one front (map #1 schema) (* NEW LOCAL UNSHIFTS *)
                  in parse_must coord0 pieces 
                        (tok, datums $ datum, Pos.union pos pos')
                        schema 
                        local_unshifts
                        str'
                  end)))

   (* Handles MAY_SEE continuations. *)
   and parse_may coord0 pieces (tok, datums, pos) schema local_unshifts str
   : string Datum.t * Pos.t * (string * Pos.t) Stream.front  = 
     (case str of 
         Stream.Nil => (* Done! (by default) *)
            (Datum.list (toList (pieces $ (tok, (), toList datums, pos))),
             Pos.pos coord0 (Pos.right pos),
             str)
       | Stream.Cons (front as ((newtok, newpos), newstr)) => 
           (case lookup schema newtok of 
               SOME token_cont => (* Next piece of multipart will now begin! *)
                  parse_cont token_cont coord0
                     (pieces $ (tok, (), toList datums, pos)) 
                     (newtok, newpos)
                     local_unshifts
                     newstr
             | NONE => (* Not starting the next piece of the multipart. *)
                 (if member unshiftable newtok 
                     orelse member local_unshifts newtok
                  then (* Also done! (by default, token that we won't shift) *)
                     (Datum.list 
                         (toList (pieces $ (tok, (), toList datums, pos))),
                      Pos.pos coord0 (Pos.right pos),
                      str)
                  else (* Add another datum to this piece *)
                  let (* AUGMENT LOCAL UNSHIFTS *)
                     val local_unshifts = local_unshifts @ (map #1 schema)
                     val (datum, pos', str') = parse_one front local_unshifts
                  in parse_may coord0 pieces
                        (tok, datums $ datum, Pos.union pos pos')
                        schema 
                        local_unshifts
                        str'
                  end)))         

   (* Handles EXACTLY_ONE continuations *)
   and parse_exactly_one coord0 pieces (tok, pos) local_unshifts str
   : string Datum.t * Pos.t * (string * Pos.t) Stream.front  = 
     (case str of 
         Stream.Nil => raise EndOfInput (coord0, str, [])
       | Stream.Cons (front as ((newtok, newpos), newstr)) => 
            if member unshiftable newtok orelse member local_unshifts newtok
            then raise EndOfInput (coord0, str, [])
            else (* Find the datum; return the datum *)
            let val (datum, pos', str') = 
                   parse_one front local_unshifts (* PRESERVE LOCAL UPSHIFTS *)
               val pieces' = pieces $ (tok, (), [datum], Pos.union pos pos')
            in 
               (Datum.list (toList pieces),
                Pos.pos coord0 (Pos.right pos'),
                str')
            end)

(*
   fun lookup_schemas schemas 

   fun shift S local_schema str = 
      case Stream.force str of 
         Stream.Nil => reduce_forever S
       | Stream.Cons ((c, pos), cs) =>
           (case local_schema c of 
               SOME con => continue con (reduce S) (c, pos) cs
             | NONE => 
                 (case lookup grammar c of
                     SOME con => begin con S (c, pos) cs
                   | NONE =>  
                       (case member unshiftable c of 
                           SOME con => reduce_forever S
                         | NONE
*)
end
