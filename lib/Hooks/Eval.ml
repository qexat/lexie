module type CONFIG = sig
  val strict : bool
  val print_program : bool
end

let exec =
  fun ~nurse:_ ~painter (module Config : CONFIG) program ->
  let code = Runtime.Core.evaluate ~painter program in
  match code = 0 with
  | false -> None
  | true -> Some ()
;;
