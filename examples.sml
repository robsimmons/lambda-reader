

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

   val _ = ("'A", 
            List [ ("'", [A]) ])
   val _ = ("[A B C D]",
            List [ ("[",[A,B,C,D]), ("]",[]) ])
   val _ = ("(A B . C D)",
            List [ ("(",[A,B]), (".",[C,D]), (")",[]) ])
   val _ = ("if A B C then D E F else if G H then I J", 
            List [ ("if",[A,B,C]), 
                   ("then",[D,E,F]), 
                   ("else",[List [ ("if",[G,H]), ("then",[I,J]) ]]) ])
   val _ = ("EXISTS A B : C . D E",
            List [ ("EXISTS",[A,B]), (":",[C]), (".",[D,E]) ])
end
