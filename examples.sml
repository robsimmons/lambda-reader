

structure Examples = 
struct
   open SimpleDatum

   val A = Atom "A"
   val B = Atom "B"
   val C = Atom "C"
   val D = Atom "D"
   val E = Atom "E"
   val F = Atom "F"
   val G = Atom "G"
   val H = Atom "H"
   val I = Atom "I"
   val J = Atom "J"

   structure Parse = 
   ParseDatumFn
     (structure Datum = SimpleDatumFn (type whitespace = unit type pos = Pos.t)
      structure Tok = StringHashable)

   fun run schema (string, datum: string datum) =
   let
      val stream = LexBrackets.tokenizeString string
      val (datum', _, stream') = Parse.parse (schema ,[]) (Stream.front stream)
   in 
      (datum', stream')
   end

   fun check schema (string, datum: string datum) = 
   let
      val (datum', stream') = run schema (string, datum)
      
   in 
      if datum <> datum' 
      then raise Fail "Datums that were supposed to match failed to match"
      else case stream' of 
              Stream.Nil => () 
            | Stream.Cons _ => raise Fail "Incomplete lex" 
   end

   val a = check Schema.scheme
           ("'A", 
            List [ ("'", [A]) ])

   val b = check Schema.scheme
           ("[A B C D]",
            List [ ("[",[A,B,C,D]), ("]",[]) ])

   val c = check Schema.scheme
           ("(A B . C D)",
            List [ ("(",[A,B]), (".",[C,D]), (")",[]) ])

   val d = check Schema.mllang
           ("if A B then C D else E F G H I J",
            List [ ("if",[A,B]),
                   ("then",[C,D]),
                   ("else",[E,F,G,H,I,J]) ])

   val e = check Schema.mllang
           ("if A B C then D E F else if G H then I J", 
            List [ ("if",[A,B,C]), 
                   ("then",[D,E,F]), 
                   ("else",[List [ ("if",[G,H]), ("then",[I,J]) ]]) ])

   val f = check Schema.celf
           ("EXISTS A B : C . D E",
            List [ ("EXISTS",[A,B]), (":",[C]), (".",[D,E]) ])
end

