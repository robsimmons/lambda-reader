
structure LDatum = 
struct
   datatype ('label, 'atom) datum = 
      Atom of 'atom * Pos.t
    | List of 'label * ('label, 'atom) datum list * Pos.t

   fun map f d = 
      case d of 
         Atom (x, pos) => Atom (f x, pos)
       | List (l, ds, pos) => List (l, map f ds, pos)

   fun fold f g d = 
      case d of
         Atom (x, pos) => f x
       | List (l, ds, pos) => g (l, map (fold f g) ds)
end

functor FixDatumFn (structure Label: HASHABLE
                    structure Atom: HASHABLE) = 
struct

   fun member [] y = false
     | member (x :: xs) y = eq (x, y) orelse member xs y

   fun lookup [] y = NONE
     | lookup ((x, data) :: xs) y = if eq (x, y) then SOME data else lookup xs y
   
   exception BadSchema

   fun find [] y = raise BadSchema
     | member (x :: xs) y = eq (x, y) orelse member xs y

   fun lookup [] y = NONE
     | lookup ((x, data) :: xs) y = if eq (x, y) then SOME data else lookup xs y

end

structure FixDatum =
FixDatumFn (structure Label = StringHashable
            structure Atom = StringHashable)

structure Test = 
struct

   fun series [] name = raise Fail "Invariant (series)"
     | series [x] name = (x, DONE name)
     | series (x :: xs) name = (x, MUST_SEE [ series xs ])

   fun series_longest [] name = raise Fail "Invariant (series_longest)"
     | series_longest [x] name = (x, MAY_SEE ([], name))
     | series_longest (x :: xs) name = (x, MUST_SEE [ series_longest xs name ]) 

   fun optional_annotation_binding name = 
      MUST_SEE 
       [ (".", MAY_SEE ([], (SOME name))),
         (":", MUST_SEE [ (".", MAY_SEE ([], (SOME name))) ]) ]


   val foo =
      [
        ("!", Right 10, "Bang"),
        ("@", Right 10, "Aff"),
        ("$", Right 10, "Gnab"),

        ("*", Right 7, "Tensor"),
        ("+", Right 6, "Tensor"),

        (">->", Right 5, "Lefti"),
        ("->>", Right 5, "Righti"),

        ("<-<", Left 4, "Left'"),
        ("<<-", Left 4, "Righti'"),

        ("&", Right 3, "With")
      ]

end
        


