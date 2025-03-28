open Custom

type t =
  { strict : bool
  ; print_program : bool
  ; use_compiler_intrinsics : bool
  }

let create =
  fun ?(strict = false) ?(print_program = false) ?(use_compiler_intrinsics = false) () ->
  { strict; print_program; use_compiler_intrinsics }
;;

let show =
  let show_pair (module Painter : Painter.TYPE) key value =
    Printf.sprintf "%s = %s" (Painter.paint_name key) (Painter.paint_constant value)
  in
  fun painter { strict; print_program; use_compiler_intrinsics } ->
    Printf.sprintf
      "{ %s ; %s ; %s }"
      (show_pair painter "strict" (Bool.to_string strict))
      (show_pair painter "print_program" (Bool.to_string print_program))
      (show_pair
         painter
         "use_compiler_intrinsics"
         (Bool.to_string use_compiler_intrinsics))
;;
