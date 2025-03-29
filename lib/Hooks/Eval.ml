open Clinic

let warn_about_compiler_intrinsics =
  fun doctor painter ->
  let review =
    Doctor.emit_single_warning
      painter
      (doctor : Doctor.t).config
      Diagnosis.Unsupported_intrinsics_at_runtime
  in
  Option.iter (Printf.eprintf "%s\n") review.details
;;

let exec =
  fun ~doctor ~painter (config : Config.t) program ->
  let env =
    if config.use_compiler_intrinsics
    then (
      warn_about_compiler_intrinsics doctor painter;
      Some Runtime.Core.intrinsics)
    else None
  in
  let code = Runtime.Core.evaluate ~painter ?env program in
  match code = 0 with
  | false -> None
  | true -> Some ()
;;
