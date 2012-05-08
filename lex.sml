(* Some very simple lexers; a lexer generated by something like CMLex
 * could potentially work just as well, of course. *)

signature LEX_CORE = 
sig
   (* If a lexer receives only partial information (for instance, the first
      part of a multi-line comment), we may want to resume it. The type
      `to resume` contains the information that would be needed to resume
      lexing at the point where the input stream ended with Nil. *)
   type to_resume
   exception Resumable of to_resume * Coord.coord

   (* @params to_resume coord cs
    * @param to_resume Information needed to resume parsing at the point where
    *                  the end of the input stream was reached.
    * @param coord 
    * @requires depth > 0 *)
   val tokenize: to_resume 
                 -> Coord.coord
                 -> (char * Coord.coord) Stream.stream
                 -> (string * Pos.pos) Stream.stream

   (* Initial state for the tokenizer *)
   val initial: to_resume
end

signature LEXER = 
sig
   type to_resume
   exception Resumable of to_resume * Coord.coord
   val tokenize: to_resume 
                 -> Coord.coord
                 -> (char * Coord.coord) Stream.stream
                 -> (string * Pos.pos) Stream.stream

(*
   val tokenizeNamedString: string -> string -> (string * Pos.pos) Stream.stream
   val tokenizeString: string -> (string * Pos.pos) Stream.stream
*)
   val tokenizeFile: string -> (string * Pos.pos) Stream.stream
(*
   val tokenizeStdIn: unit -> (string * Pos.pos) Stream.stream
   val tokenizeStreamLine: string -> TextIO.instream
                           -> (string * Pos.pos) Stream.stream
   val tokenizeStdInLine: string -> (string * Pos.pos) Stream.stream
*)
end

functor MakeLexer (Lex: LEX_CORE):> 
   LEXER where type to_resume = Lex.to_resume = 
struct
   type to_resume = Lex.to_resume
   exception Resumable = Lex.Resumable 

   (* Hmm, factor this out? I think this belongs in a "fsutil" smackage
      package. *)
   fun iostream (finalize: unit -> unit) (stream: TextIO.instream) = 
      Stream.lazy 
      (fn () => 
          case TextIO.input1 stream of
             NONE => (finalize (); Stream.Nil)
           | SOME c => Stream.Cons (c, iostream finalize stream))

   (* Factor this out too? *)
   fun eol stream = 
      case Stream.front stream of
         Stream.Cons (#"\n", _) => true
       | Stream.Cons (#"\v", _) => true
       | Stream.Cons (#"\f", _) => true
       | Stream.Cons (#"\r", stream) => 
           (case Stream.front stream of
               Stream.Cons (#"\n", _) => false
             | _ => true)
       | _ => false

   val tokenize = Lex.tokenize

   fun tokenizeFile (name: string) =
   let
      val coord = Coord.init name
      val instream = TextIO.openIn name
      val charstream = iostream (fn () => TextIO.closeIn instream) instream
      val coordstream = CoordinatedStream.coordinate eol coord charstream
   in
      Lex.tokenize Lex.initial coord coordstream
   end 
end

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
            Stream.Nil => Stream.Nil
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

(* An slightly more involved lexer - ()[].'=, are separators *)
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

(* Twelf-like lexing:
 * 
 * `%' is special:
 * `%{...}%` is a nested multiline comments.
 * `%.` ends lexing
 * `%%...\n` is a single-line comment
 * `%` followed by `%` or whitespace is a single-line comment
 * 
 * Reserved characters (`reserved c` returns `true`) are lexed individually.
 * All other characters are tokenized until whitespace, a reserved character,
 * or whitespaces arises. *)
structure LexPercent = 
struct
   type to_resume = int
   val initial = 0
   exception Resumable of to_resume * Coord.coord

   fun tokenize reserved n coord cs = 
   let
      (* Invariant: n > 0. `coord` is the last seen coordinate *)
      (* Returns the stream after reading n closing braces *)
      fun multi (coord, cs) n = 
         case Stream.front cs of
            Stream.Nil => raise Resumable (n, coord)
          | Stream.Cons ((#"}", coord'), cs) =>
              (case Stream.front cs of 
                  Stream.Nil => raise Resumable (n, coord')
                | Stream.Cons ((#"%", coord''), cs) => 
                     if n = 1 then (coord'', cs) else multi (coord', cs) (n-1) 
                | _ => multi (coord', cs) n)
          | Stream.Cons ((_, coord'), cs) => multi (coord', cs) n

      (* Returns the stream after reading `\n` (or an end-of-file). *)
      fun single (coord, cs) = 
         case Stream.front cs of
            Stream.Nil => (coord, cs)
          | Stream.Cons ((#"\n", coord'), cs) => (coord', cs)
          | Stream.Cons ((_, coord'), cs) => single (coord', cs)

      (* Looks for the beginning of a token *)
      fun next (coord, cs) () = 
         case Stream.front cs of
            Stream.Nil => Stream.Nil
          | Stream.Cons ((#"%", coord'), cs) => percent coord (coord', cs)
          | Stream.Cons ((c, coord'), cs) =>
               if reserved c 
               then Stream.Cons ((str c, Pos.pos coord coord'), 
                                 Stream.lazy (next (coord', cs)))
               else if Char.isSpace c
               then next (coord', cs) ()
               else token coord [c] (coord', cs)

      (* Handles a stream following a percent sign *)
      and percent percent_coord (coord, cs) =
         case Stream.front cs of
            Stream.Nil => Stream.Nil
          | Stream.Cons ((#"{", coord'), cs) => next (multi (coord', cs) 1) ()
          | Stream.Cons ((#"%", coord'), cs) => next (single (coord', cs)) ()
          | Stream.Cons ((#".", _), cs) => Stream.Nil
          | Stream.Cons ((#"\n", coord'), cs) => (* Trivial linecomment *)
               next (coord', cs) ()
          | Stream.Cons ((c, coord'), cs) => 
               if Char.isSpace c 
               then next (single (coord', cs)) ()
               else token percent_coord [c, #"%"] (coord', cs) 
      
      (* Looks for the completion of a non-reserved token *)
      and token start chars (coord, cs) = 
         case Stream.front cs of
            Stream.Nil =>
               Stream.Cons ((implode (rev chars), Pos.pos start coord), 
                            Stream.eager Stream.Nil)
          | Stream.Cons ((#"%", coord'), cs) =>
               Stream.Cons ((implode (rev chars), Pos.pos start coord), 
                            Stream.lazy (fn () => percent coord (coord', cs)))
          | Stream.Cons ((c, coord'), cs) => 
               if Char.isSpace c 
               then Stream.Cons ((implode (rev chars), Pos.pos start coord), 
                      Stream.lazy (next (coord', cs)))
               else if reserved c 
               then Stream.Cons ((implode (rev chars), Pos.pos start coord),
                      Stream.eager
                        (Stream.Cons ((str c, Pos.pos coord coord'),
                           Stream.lazy (next (coord', cs)))))
               else token start (c :: chars) (coord', cs)
   in 
      if n > 0 
      then Stream.lazy (next (coord, cs))
      else Stream.lazy (next (multi (coord, cs) n))
   end
end

(* A faithful Twelf lexer *)
structure LexCoreTwelf =
MakeLexer
  (struct
      open LexPercent
      val tokenize = 
         tokenize (fn #"(" => true | #")" => true | #":" => true
                    | #"[" => true | #"]" => true | #"." => true
                    | #"{" => true | #"}" => true | #"\"" => true
                    | _ => false)
   end)
