include Stdlib.Option

let on_none =
  fun func option ->
  (match option with
   | None -> func ()
   | Some _ -> ());
  option
;;
