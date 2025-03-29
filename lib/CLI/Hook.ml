open Custom
open Clinic

module Painter = Painter.Make (struct
    let show_styling = `Auto
  end)

let config : Config.t =
  { strict = true; print_program = true; use_compiler_intrinsics = true }
;;

let doctor = Doctor.create config

let check () =
  match Hooks.Check.exec ~doctor ~painter:(module Painter) config Sample.program with
  | None -> 1
  | Some _ -> 0
;;

let run () =
  match Hooks.Run.exec ~doctor ~painter:(module Painter) config Sample.program with
  | None -> 1
  | Some () -> 0
;;
