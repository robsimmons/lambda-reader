
structure Struct = 
struct
   datatype 'a token_cont = 
      DONE
    | SHORTEST
    | LONGEST of ('a * 'a token_cont) list 
    | ONLY of ('a * 'a token_cont) list

    ("if",
     [("then", 
       [("else", LONGEST),
        LONGEST])]

  val iflang = 
     [ ("if", 
        ONLY)
       ("(", 
        ONLY [(")", DONE)] 
       ("[",
        ONLY 
         [ (":", 
            ONLY 
             [ ("]", LONGEST) ]),
           ("]", LONGEST) ]
     CONT 
      { tok = "if",
        success =
        CONT
         { tok = "then",
           success =
           CONT 
            { tok = "else",
              success = LONGEST
              failure = 
           CONT
      failure=?
end

(*
functor Read (structure Prec: ORDERED
              structure Tokens: ORDERED (* actually only needs to be eq *)
              structure ) =
struct
   datatype nfix = Left | Right | None
   datatype Character = 
      Infix of Precedence.t    (* Infix character *)
    | Prefix of Precedence.t   (* Prefix character *)
    | Postfix of Precedence.t  (* Postfix character *)
    | Binder of tok            (* Binder, waiting for bindpoint tok *)
    | Bracket of tok           (* Bracket, waiting for endpoint tok *)

   type data = {
      fixity: 
      }
end
*)

