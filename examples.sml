

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

   structure CelfParse = 
   ParseDatum 
     (structure Datum = SimpleDatumFn (type whitespace = unit type pos = Pos.t)
      val unshiftable = []
      val global_schema = Schema.celf)

end
