(* Lexing character streams into token streams. *)
(* Rob Simmons *)

(* To write a lexer, implement the LEX_CORE signature and pass the resulting
 * structor to the MakeLexer functor; it the result implements LEXER. 
 * Example implementations are given in lex-simple.sml and lex-twelf.sml *)

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

   val tokenizeNamedString: string -> string -> (string * Pos.pos) Stream.stream
   val tokenizeString: string -> (string * Pos.pos) Stream.stream
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

   fun tokenizeCoordStream (coord: Coord.t) (str: char Stream.stream) = 
      Lex.tokenize Lex.initial coord 
         (CoordinatedStream.coordinate eol coord str)

   fun tokenizeCoordString (coord: Coord.t) (str: string) = 
      tokenizeCoordStream coord (Stream.fromString str)

   fun tokenizeFile (name: string) =
   let
      val coord = Coord.init name
      val instream = TextIO.openIn name
      val charstream = iostream (fn () => TextIO.closeIn instream) instream
   in
      tokenizeCoordStream coord charstream
   end 

   fun tokenizeNamedString (name: string) (str: string) = 
      tokenizeCoordString (Coord.init name) str

   fun tokenizeString (str: string) = tokenizeNamedString "<string>" str
end

