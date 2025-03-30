open Custom

(** A configuration for the entire toolchain! *)
type t =
  { strict : bool
  ; print_program : bool
  ; use_compiler_intrinsics : bool
  }

(** [create ?strict ?print_program ?use_compiler_intrinsics ()]
    builds a configuration but allows not to specify certain or
    all fields, using default values for them. *)
val create
  :  ?strict:bool
  -> ?print_program:bool
  -> ?use_compiler_intrinsics:bool
  -> unit
  -> t

(** [show painter configuration] produces a pretty-printable
    representation of the [configuration] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
