
structure Struct = 
struct
   (* After you see a token, what do you expect? *)
   datatype 'a token_cont = 
      (* You expect to parse some datums, and then reach one of these tokens,
         at which point you continue working on this datum. *)
      TOK of ('a * 'a token_cont) list
      
      (* The datum continues until either one of these characters is reached,
         or the hard-stop is reached. *)
    | LONGEST of ('a * 'a token_cont) list

      (* The datum is fully parsed. *)
    | DONE

      (* Parse exactly one more character and add it to the datum. *)
    | SHORTEST

   fun series_longest [] = raise Fail "Invariant (series_longest)"
     | series_longest [x] = (x, LONGEST [])
     | series_longest (x :: xs) = (x, TOK [ series_longest xs ]) 

   fun series [] = raise Fail "Invariant (series)"
     | series [x] = (x, DONE)
     | series (x :: xs) = (x, TOK [ series xs ])


   (* Simple if language *)
   val iflang = 
      [ series_longest [ "if", "then", "else" ],
        series [ "(", ")" ] ]


   (* Little Ocaml *)
   val mllang =
      [ series ["(", ")"],
        series ["{", "}"],
        series_longest [ "match", "with" ],
        series_longest [ "fn" ],
        ("if", TOK [ ("then", LONGEST [ ("else", LONGEST []) ]) ]) ]


   (* Celf *)
   val optional_annotation_binding = 
      TOK [ (".", LONGEST []),
            (":", TOK [ (".", LONGEST []) ]) ]

   val celf = 
      [ series ["(", ")"],
        series ["{", "}"],
        series ["<", ">"],
        series ["[", "]"],

        ("Pi", optional_annotation_binding),
        series_longest [ "PI", ":", "." ],
        ("Exists", optional_annotation_binding),
        series_longest [ "EXISTS", ":", "." ],
 
        series_longest ["let", "{", "}", "="],
        ("\\", optional_annotation_binding) ]


   (* Twelf *)
   val optional_intermediate_colon = 
      fn close => TOK [ (close, DONE), (":", TOK [ (close, LONGEST []) ]) ]

   val twelf = 
      [ series [ "(", ")" ],
        ("[", optional_intermediate_colon "]"),
        ("{", optional_intermediate_colon "}") ]


   (* Scheme r6rs *)
   val optional_intermediate_dot = 
      fn close => TOK [ (close, DONE), (".", TOK [ (close, DONE) ]) ]

   val scheme = 
      [ (* List *)
        ("(", optional_intermediate_dot ")"),
        ("[", optional_intermediate_dot "]"),

        (* Abbrev prefix *)
        ("'", SHORTEST),
        ("`", SHORTEST),
        (",", SHORTEST),
        (",@", SHORTEST),
        ("#'", SHORTEST),
        ("#`", SHORTEST),
        ("#,", SHORTEST),
        ("#,@", SHORTEST),

        (* Vector *)
        series [ "#(", ")" ],

        (* Bytevector *)
        series [ "#vu8(", ")" ] ]
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

