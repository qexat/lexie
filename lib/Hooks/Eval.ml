module type CONFIG = Check.CONFIG

let exec =
  fun ~nurse:_ ~painter (module Config : CONFIG) program ->
  let code = Runtime.Core.evaluate ~painter program in
  match code = 0 with
  | false -> None
  | true -> Some ()
;;
