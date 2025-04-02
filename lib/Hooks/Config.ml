type t =
  { print_program : bool
  ; use_compiler_intrinsics : bool
  }

let create =
  fun ?(print_program = false) ?(use_compiler_intrinsics = false) () ->
  { print_program; use_compiler_intrinsics }
;;
