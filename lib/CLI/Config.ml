type t =
  { strict : bool
  ; print_program : bool
  ; use_compiler_intrinsics : bool
  ; show_styling : [ `Never | `Always | `Auto ]
  }

let create =
  fun ?(strict = false)
    ?(print_program = false)
    ?(use_compiler_intrinsics = false)
    ?(show_styling = `Auto)
    () ->
  { strict; print_program; use_compiler_intrinsics; show_styling }
;;
