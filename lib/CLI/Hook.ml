open Custom
open Clinic

let check =
  fun strict print_program use_compiler_intrinsics () ->
  let config = Config.create ~strict ~print_program ~use_compiler_intrinsics () in
  let doctor = Doctor.create config in
  let painter =
    (module Painter.Make (struct
         let show_styling = `Auto
       end) : Painter.TYPE)
  in
  match Hooks.Check.exec ~doctor ~painter config Sample.program with
  | None -> 1
  | Some _ -> 0
;;

let run =
  fun strict print_program use_compiler_intrinsics () ->
  let config = Config.create ~strict ~print_program ~use_compiler_intrinsics () in
  let doctor = Doctor.create config in
  let painter =
    (module Painter.Make (struct
         let show_styling = `Auto
       end) : Painter.TYPE)
  in
  match Hooks.Run.exec ~doctor ~painter config Sample.program with
  | None -> 1
  | Some () -> 0
;;
