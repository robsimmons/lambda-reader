(* A schema explans how to parse a language into a dumb syntax tree. *)
(* Rob Simmons *)

signature SCHEMA = 
sig
   (* After you see a token, what do you expect? *)
   datatype ('tok, 'name) token_cont = 
      (* The datum is fully parsed. *)
      DONE of 'name

      (* You expect to parse some datums, and then reach one of these tokens,
       * at which point you continue working on this datum. If an unshiftable
       * character/end of input is reached, this will be a parse error. If
       * the list is empty, this will always fail. *)
    | MUST_SEE of ('tok * ('tok, 'name) token_cont) list
      
      (* The datum continues until either one of these characters is reached,
       * or an unshiftable character/end of input is reached. If this list 
       * is empty, this is the usual "longest parse" semantics of binding
       * syntax. The second list represents any augmentations that should
       * be made to the list of unshiftable tokens for the purpose of
       * this MAY binding. *)
    | MAY_SEE of ('tok * ('tok, 'name) token_cont) list * 'tok list * 'name

      (* Parse exactly one more character and add it to the datum. This is 
       * a very demanding prefix operator. If there is nothing to parse next
       * (due to an unshiftable character/end of input), this will be a parse
       * error. *)
    | EXACTLY_ONE of 'name

   type ('tok, 'name) t = ('tok * ('tok, 'name) token_cont) list
   type ('tok, 'name) schema = ('tok * ('tok, 'name) token_cont) list

   (* Examples *)
   val iflang: (string, unit) schema
   val mllang: (string, unit) schema
   val celf: (string, unit) schema
   val twelf: (string, unit) schema
   val scheme: (string, unit) schema
end

structure Schema:> SCHEMA = 
struct
   (* After you see a token, what do you expect? *)
   datatype ('tok, 'name) token_cont = 
      DONE of 'name
    | MUST_SEE of ('tok * ('tok, 'name) token_cont) list
    | MAY_SEE of ('tok * ('tok, 'name) token_cont) list * 'tok list * 'name
    | EXACTLY_ONE of 'name

   type ('tok, 'name) t = ('tok * ('tok, 'name) token_cont) list
   type ('tok, 'name) schema = ('tok * ('tok, 'name) token_cont) list

   fun series_longest [] = raise Fail "Invariant (series_longest)"
     | series_longest [x] = (x, MAY_SEE ([], [], ()))
     | series_longest (x :: xs) = (x, MUST_SEE [ series_longest xs ]) 

   fun series [] = raise Fail "Invariant (series)"
     | series [x] = (x, DONE ())
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
        ("if", MUST_SEE 
                [ ("then", MAY_SEE 
                            ([ ("else", MAY_SEE ([], [], ())) ], [], ())) ]) ]


   (* Celf *)
   val optional_annotation_binding = 
      MUST_SEE 
       [ (".", MAY_SEE ([], [], ())),
         (":", MUST_SEE [ (".", MAY_SEE ([], [], ())) ]) ]

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
                   [ (close, DONE ()),
                     (":", MUST_SEE [ (close, MAY_SEE ([], [], ())) ]) ]

   val twelf = 
      [ series [ "->" ],
        series [ "<-" ],
        series [ "_" ],
        series [ "=" ],
        series [ ":" ],
        series [ "type" ],
         
        series [ "(", ")" ],
        ("[", optional_intermediate_colon "]"),
        ("{", optional_intermediate_colon "}") ]


   (* Scheme r6rs *)
   val optional_intermediate_dot = 
      fn close => MUST_SEE 
                   [ (close, DONE ()), 
                     (".", MUST_SEE [ (close, MAY_SEE ([], [], ())) ]) ]

   val scheme = 
      [ (* Actually a comment. *)
        ("#;", EXACTLY_ONE ()),
        
        (* List *)
        ("(", optional_intermediate_dot ")"),
        ("[", optional_intermediate_dot "]"),

        (* Abbrev prefix *)
        ("'", EXACTLY_ONE ()),
        ("`", EXACTLY_ONE ()),
        (",", EXACTLY_ONE ()),
        (",@", EXACTLY_ONE ()),
        ("#'", EXACTLY_ONE ()),
        ("#`", EXACTLY_ONE ()),
        ("#,", EXACTLY_ONE ()),
        ("#,@", EXACTLY_ONE ()),

        (* Vector *)
        series [ "#(", ")" ],

        (* Bytevector *)
        series [ "#vu8(", ")" ] ]
end

