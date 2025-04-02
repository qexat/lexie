open Custom
open Clinic

let execute =
  fun (config : Config.t) ->
  let hook_config : Hooks.Config.t =
    { print_program = config.print_program
    ; use_compiler_intrinsics = config.use_compiler_intrinsics
    }
  in
  let doctor = Doctor.create { strict = config.strict } in
  let painter =
    (module Painter.Make (struct
         let show_styling = config.show_styling
       end) : Painter.TYPE)
  in
  match Hooks.Check.execute ~doctor ~painter hook_config Sample.program with
  | None -> 1
  | Some _ -> 0
;;
