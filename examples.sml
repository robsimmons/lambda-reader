
structure SimpleExamples = 
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
      structure Tok = StringHashable
      type name = unit)

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
      Testing.expect ()
         (fn () => datum = datum' 
                   andalso (case stream' of Stream.Nil => true | _ => false))
         string
   end

   val () = Testing.reset ()

   val () = check Schema.scheme
           ("'A", 
            List [ ("'", [A]) ])

   val () = check Schema.scheme
           ("[A B C D]",
            List [ ("[",[A,B,C,D]), ("]",[]) ])

   val () = check Schema.scheme
           ("(A B . C D)",
            List [ ("(",[A,B]), (".",[C,D]), (")",[]) ])

   val () = check Schema.mllang
           ("if A B then C D else E F G H I J",
            List [ ("if",[A,B]),
                   ("then",[C,D]),
                   ("else",[E,F,G,H,I,J]) ])

   val () = check Schema.mllang
           ("if A B C then D E F else if G H then I J", 
            List [ ("if",[A,B,C]), 
                   ("then",[D,E,F]), 
                   ("else",[List [ ("if",[G,H]), ("then",[I,J]) ]]) ])

   val () = check Schema.celf
           ("EXISTS A B : C . D E",
            List [ ("EXISTS",[A,B]), (":",[C]), (".",[D,E]) ])

   val () = print "\nSimpleExamples:\n"
   val () = Testing.report ()
end



