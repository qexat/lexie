open Clinic

let execute =
  fun ~doctor ~painter (config : Config.t) program ->
  if config.print_program
  then Printf.printf "%s\n---\n" (Ail.Program.show painter program);
  let context =
    if config.use_compiler_intrinsics then Some Analysis.Core.intrinsics else None
  in
  let _ = Analysis.Core.check_program ~doctor ?context program in
  let review = Doctor.review painter doctor in
  Option.iter (Printf.eprintf "%s\n") review.details;
  match review.decision with
  | Abort -> None
  | Pass -> Some program
;;
