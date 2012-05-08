(* Abstract representation of syntax trees, lightly-parsed strings. *)
signature DATUM = 
sig
   type 'tok syn
   type 'tok t = 'tok syn

   (* Atomic token: x, ->, +, "Foo", etc.
      @params (tok, coord) 
      @param coord The file coordinates of the atomic token. 
      @returns foo *)
   val Atom: 'tok * Coord.coord -> 'tok syn

   (* Sequence of syntax trees S1 S2 S3 ... Sn.
      @params (syns, coord)
      @param syns  List of syntax tress S1 S2 S3 ... Sn
      @param coord The file coordinates of the list of syntax trees *)
   val List: 'tok * 'tok syn list * Coord.coord -> 'tok syn     
end

(* Concrete representation of syntax. *)
structure SyntaxTree = 
struct
   datatype 'tok syn = 
      Atom of 'tok * Coord.coord
    | List of 'tok * 'tok syn list * Coord.coord
   type 'tok t = 'tok syn
end

(* Verify that SyntaxTree meets the specification *)
structure Test:> SYNTAX_TREE = SyntaxTree
structure Test = struct end
