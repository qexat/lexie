(** [check strict print_program use_compiler_intrinsics ()]
    checks the sample program. *)
val check : bool -> bool -> bool -> unit -> int

(** [run strict print_program use_compiler_intrinsics ()] checks
    and evaluates the sample program. *)
val run : bool -> bool -> bool -> unit -> int
