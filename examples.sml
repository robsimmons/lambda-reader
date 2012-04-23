structure Examples = 
struct
   structure Dict = StringRedBlackDict

   local
      open IntFixity (* Left/Right/None/Prefix/Postfix/Fix/Bindr/Bracket *)

      fun makeMap appFixity tokFixity = 
         (appFixity, 
          List.foldr 
             (fn ((toks, tchar), dict) =>
                 List.foldr (fn (tok, dict) => Dict.insert dict tok tchar) 
                    dict toks)
             Dict.empty tokFixity)

      fun makeFun (appFixity, tokFixity) =
         fn NONE => SOME (Fix (Right, appFixity))
          | SOME tok => Dict.find tokFixity tok

      fun encode appFixity tokFixity = makeFun (makeMap appFixity tokFixity)
   in
   (* Lisp is really simple *)
   val lisp = 
      encode 1
        [ (["'"], Fix (Prefix, 1)),
          (["("], Bracket ")") ]

   (* Twelf without fixity declarations *)
   val twelf = 
      encode 6
        [ (["["], Binder "]"),
          (["{"], Binder "}"),
          (["="], Fix (None, 1)),
          ([":"], Fix (None, 2)),
          (["<-"], Fix (Left, 3)),
          (["->"], Fix (Right, 4)),
          (["("], Bracket ")") ]

   (* Celf *)
   val celf = 
      encode 6
        [ (["EXISTS", "PI", "Exists", "Pi", "\\"], Binder "."),
          (["="], Fix (None, 1)),
          ([":"], Fix (None, 2)),
          (["o-", "@-", "<-"], Fix (Left, 3)),
          (["-o", "-@", "->"], Fix (Right, 4)),
          (["*", "&"], Fix (Right, 5)),
          (["!", "@"], Fix (Prefix, 6)),
          (["("], Bracket ")"),
          (["["], Bracket "]"),
          (["<"], Bracket ">"),
          (["{"], Bracket "}") ]
   end
end
