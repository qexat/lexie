module type CONFIG = Check.CONFIG

let compiler_intrinsics_warning =
  {|compiler intrinsics are not fully supported by the runtime and may cause unexpected crashes.|}
;;

let exec =
  fun ~nurse:_ ~painter (module Config : CONFIG) program ->
  let env =
    if Config.use_compiler_intrinsics
    then (
      Printf.eprintf "WARNING: %s\n" compiler_intrinsics_warning;
      Some Runtime.Core.intrinsics)
    else None
  in
  let code = Runtime.Core.evaluate ~painter ?env program in
  match code = 0 with
  | false -> None
  | true -> Some ()
;;
