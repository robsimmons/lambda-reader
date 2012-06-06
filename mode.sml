signature MODE =
sig
   val handler: Handle.handler
end

structure Mode =
struct

   fun handleMode (dats, pos) =
    ( print ("#mode")
    ; List.app (fn dat => (print " "; PosDatum.print dat)) dats)
 
   fun modeCheck _ = ()

   val handler = 
      {syntax: fn (PosDatum.List [("mode", ds, pos)]) =>
                | _ => (),
       condec: fn _ => (),
       rule: modeCheck}
end
