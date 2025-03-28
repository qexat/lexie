open Custom

let usage = "<usage>"
let strict = ref false
let arg_spec = [ "--strict", Arg.Set strict, "strict mode" ]

module Painter = Painter.Make (struct
    let show_styling = `Auto
  end)

let main () =
  Arg.parse arg_spec (fun _ -> ()) usage;
  let nurse = Nurse.create () in
  Printf.printf "Program:\n%s\n" (Analysis.Program.show (module Painter) Sample.program);
  let code =
    match Analysis.Core.check ~nurse Sample.program with
    | Some _ -> 0
    | None -> 1
  in
  if !strict then Nurse.turn_warnings_into_errors nurse;
  Nurse.report (module Painter) nurse;
  code
;;
