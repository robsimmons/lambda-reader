(* Simple lex examples: LexParens and LexBrackets *)
(* Rob Simmons *)

(* Dumbest possible reasonable lexer:
 * `#...\n` is a single-line comment
 * Reserved characters (`reserved c` returns `true`) are parsed as single 
 * tokens.
 * Tokens are separated by whitespace and reserved . *)

structure LexSimple = 
struct
   type to_resume = unit
   val initial = ()
   exception Resumable of to_resume * Coord.coord
   
   fun tokenize reserved () coord cs = 
   let
      fun single (coord, cs) = 
         case Stream.front cs of
            Stream.Nil => (coord, cs)
          | Stream.Cons ((#"\n", coord'), cs) => (coord', cs)
          | Stream.Cons ((_, coord'), cs) => single (coord', cs)

      (* Find the beginning of the next token *)
      fun next (coord, cs) () = 
         case Stream.front cs of
            Stream.Nil => Stream.Nil
          | Stream.Cons ((#"#", coord'), cs) => next (single (coord', cs)) ()
          | Stream.Cons ((c, coord'), cs) => 
               if reserved c 
               then Stream.Cons ((str c, Pos.pos coord coord'), 
                                 Stream.lazy (next (coord', cs)))
               else if Char.isSpace c
               then next (coord', cs) ()
               else token coord [c] (coord', cs)

      and token start chars (coord, cs) = 
         case Stream.front cs of
            Stream.Nil => 
               Stream.Cons ((implode (rev chars), Pos.pos start coord),
                 Stream.eager Stream.Nil)
          | Stream.Cons ((#"#", coord'), cs) => 
               Stream.Cons ((implode (rev chars), Pos.pos start coord),
                 Stream.lazy (next (single (coord', cs))))
          | Stream.Cons ((c, coord'), cs) => 
               if reserved c 
               then Stream.Cons ((implode (rev chars), Pos.pos start coord), 
                      Stream.eager
                        (Stream.Cons ((str c, Pos.pos coord coord'),
                           Stream.lazy (next (coord', cs)))))
               else if Char.isSpace c
               then Stream.Cons ((implode (rev chars), Pos.pos start coord),
                      Stream.lazy (next (coord', cs)))
               else token coord (c :: chars) (coord', cs)
   in
      Stream.lazy (next (coord, cs))
   end
end

(* A minimal lispy lexer *)
structure LexParens = 
MakeLexer
  (struct
      open LexSimple
      val tokenize = 
         tokenize (fn #"(" => true | #")" => true | #"'" => true | _ => false)
   end)

(* An slightly more involved lexer - ()[]{}.'=, are separators *)
structure LexBrackets = 
MakeLexer
  (struct
      open LexSimple
      val tokenize = 
         tokenize (fn #"(" => true | #")" => true | #"'" => true 
                    | #"[" => true | #"]" => true | #"." => true
                    | #"{" => true | #"}" => true | #"=" => true
                    | #"," => true | _ => false)
   end)
