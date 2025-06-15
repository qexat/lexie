type t =
  { print_program : bool
  ; use_compiler_intrinsics : bool
  }

(** [create ?print_program ?use_compiler_intrinsics ()]
      creates a hook config without having to specify all
      fields, providing good defaults. *)
val create
  :  ?print_program:bool
  -> ?use_compiler_intrinsics:bool
  -> unit
  -> t
