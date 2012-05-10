

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

   structure SchemeParse = 
   ParseDatum 
     (structure Datum = SimpleDatumFn (type whitespace = unit type pos = Pos.t)
      val unshiftable = []
      val global_schema = Schema.scheme)

   structure MLParse = 
   ParseDatum 
     (structure Datum = SimpleDatumFn (type whitespace = unit type pos = Pos.t)
      val unshiftable = []
      val global_schema = Schema.mllang)

   structure CelfParse = 
   ParseDatum 
     (structure Datum = SimpleDatumFn (type whitespace = unit type pos = Pos.t)
      val unshiftable = []
      val global_schema = Schema.celf)

   fun run lex parse_one (string, datum: string datum) =
   let
      val stream = lex string
      val front = case Stream.front stream of 
                     Stream.Nil => raise Fail "Empty string"
                   | Stream.Cons front => front
      val (datum', _, stream') = parse_one front []
   in 
      (datum', stream')
   end

   fun check lex parse_one (string, datum: string datum) = 
   let
      val (datum', stream') = run lex parse_one (string, datum)
      
   in 
      if datum <> datum' 
      then raise Fail "Datums that were supposed to match failed to match"
      else case stream' of 
              Stream.Nil => () 
            | Stream.Cons _ => raise Fail "Incomplete lex" 
   end

   val a = check LexBrackets.tokenizeString SchemeParse.parse_one
           ("'A", 
            List [ ("'", [A]) ])

   val b = check LexBrackets.tokenizeString SchemeParse.parse_one
           ("[A B C D]",
            List [ ("[",[A,B,C,D]), ("]",[]) ])

   val c = check LexBrackets.tokenizeString SchemeParse.parse_one
           ("(A B . C D)",
            List [ ("(",[A,B]), (".",[C,D]), (")",[]) ])

   val d = check LexBrackets.tokenizeString MLParse.parse_one
           ("if A B then C D else E F G H I J",
            List [ ("if",[A,B]),
                   ("then",[C,D]),
                   ("else",[E,F,G,H,I,J]) ])

   val e = check LexBrackets.tokenizeString MLParse.parse_one 
           ("if A B C then D E F else if G H then I J", 
            List [ ("if",[A,B,C]), 
                   ("then",[D,E,F]), 
                   ("else",[List [ ("if",[G,H]), ("then",[I,J]) ]]) ])

   val f = check LexBrackets.tokenizeString CelfParse.parse_one
           ("EXISTS A B : C . D E",
            List [ ("EXISTS",[A,B]), (":",[C]), (".",[D,E]) ])
end

