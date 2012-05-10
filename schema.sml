(* A schema explans how to parse a language into a dumb syntax tree. *)

signature SCHEMA = 
sig
   (* After you see a token, what do you expect? *)
   datatype 'a token_cont = 
      (* The datum is fully parsed. *)
      DONE

      (* You expect to parse some datums, and then reach one of these tokens,
       * at which point you continue working on this datum. If an unshiftable
       * character/end of input is reached, this will be a parse error. If
       * the list is empty, this will always fail. *)
    | MUST_SEE of ('a * 'a token_cont) list
      
      (* The datum continues until either one of these characters is reached,
       * or an unshiftable character/end of input is reached. If this list 
       * is empty, this is the usual "longest parse" semantics of binding
       * syntax. *)
    | MAY_SEE of ('a * 'a token_cont) list

      (* Parse exactly one more character and add it to the datum. This is 
       * a very demanding prefix operator. If there is nothing to parse next
       * (due to an unshiftable character/end of input), this will be a parse
       * error. *)
    | EXACTLY_ONE

   type 'a t = ('a * 'a token_cont) list
   type 'a schema = ('a * 'a token_cont) list

   (* Examples *)
   val iflang: string schema
   val mllang: string schema
   val celf: string schema
   val twelf: string schema
   val scheme: string schema
end

structure Schema:> SCHEMA = 
struct
   (* After you see a token, what do you expect? *)
   datatype 'a token_cont = 
      DONE
    | MUST_SEE of ('a * 'a token_cont) list
    | MAY_SEE of ('a * 'a token_cont) list
    | EXACTLY_ONE

   type 'a t = ('a * 'a token_cont) list
   type 'a schema = ('a * 'a token_cont) list

   fun series_longest [] = raise Fail "Invariant (series_longest)"
     | series_longest [x] = (x, MAY_SEE [])
     | series_longest (x :: xs) = (x, MUST_SEE [ series_longest xs ]) 

   fun series [] = raise Fail "Invariant (series)"
     | series [x] = (x, DONE)
     | series (x :: xs) = (x, MUST_SEE [ series xs ])


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
        ("if", MUST_SEE [ ("then", MAY_SEE [ ("else", MAY_SEE []) ]) ]) ]


   (* Celf *)
   val optional_annotation_binding = 
      MUST_SEE [ (".", MAY_SEE []), (":", MUST_SEE [ (".", MAY_SEE []) ]) ]

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
      fn close => MUST_SEE 
                   [ (close, DONE),
                     (":", MUST_SEE [ (close, MAY_SEE []) ]) ]

   val twelf = 
      [ series [ "(", ")" ],
        ("[", optional_intermediate_colon "]"),
        ("{", optional_intermediate_colon "}") ]


   (* Scheme r6rs *)
   val optional_intermediate_dot = 
      fn close => MUST_SEE 
                   [ (close, DONE), 
                     (".", MUST_SEE [ (close, MAY_SEE []) ]) ]

   val scheme = 
      [ (* Actually a comment. *)
        ("#;", EXACTLY_ONE),
        
        (* List *)
        ("(", optional_intermediate_dot ")"),
        ("[", optional_intermediate_dot "]"),

        (* Abbrev prefix *)
        ("'", EXACTLY_ONE),
        ("`", EXACTLY_ONE),
        (",", EXACTLY_ONE),
        (",@", EXACTLY_ONE),
        ("#'", EXACTLY_ONE),
        ("#`", EXACTLY_ONE),
        ("#,", EXACTLY_ONE),
        ("#,@", EXACTLY_ONE),

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

