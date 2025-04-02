(** Config used by CLI hooks. *)
type t =
  { strict : bool
  ; print_program : bool
  ; use_compiler_intrinsics : bool
  ; show_styling : [ `Always | `Auto | `Never ]
  }
