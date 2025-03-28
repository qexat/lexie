module type CONFIG = sig
  val strict : bool
  val print_program : bool
end

let setup = fun nurse -> Nurse.set_diagnosis_renderer Analysis.Diagnosis.render nurse

let exec =
  fun ~nurse ~painter (module Config : CONFIG) program ->
  if Config.print_program
  then Printf.printf "%s\n---\n" (Analysis.Program.show painter program);
  let result = Analysis.Core.check ~nurse program in
  if Config.strict then Nurse.turn_warnings_into_errors nurse;
  Nurse.report painter nurse;
  Option.map (fun _ -> program) result
;;
