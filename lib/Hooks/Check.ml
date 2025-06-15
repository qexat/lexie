open Batteries.Operators
open Clinic

let execute =
  fun ~doctor ~painter (config : Config.t) program ->
  if config.print_program
  then Printf.printf "%s\n---\n" (AIL.Program.show painter program);
  let context =
    if config.use_compiler_intrinsics then Some Analysis.Core.intrinsics else None
  in
  let maybe_context = Analysis.Core.check_program ~doctor ?context program in
  Option.iter (Analysis.Core.Context.show painter *> Printf.printf "%s\n") maybe_context;
  let review = Doctor.review painter doctor in
  Option.iter (Printf.eprintf "%s\n") review.details;
  match review.decision with
  | Abort -> None
  | Pass -> Some program
;;
