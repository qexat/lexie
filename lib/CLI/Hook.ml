open Custom

let nurse = Nurse.create ()

module Painter = Painter.Make (struct
    let show_styling = `Auto
  end)

module Config = struct
  let strict = true
  let print_program = true
end

let check () =
  match
    Hooks.Check.exec ~nurse ~painter:(module Painter) (module Config) Sample.program
  with
  | None -> 1
  | Some _ -> 0
;;

let run () =
  match
    Hooks.Run.exec ~nurse ~painter:(module Painter) (module Config) Sample.program
  with
  | None -> 1
  | Some () -> 0
;;
