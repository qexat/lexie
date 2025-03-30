open Custom
module Environment : Quickmap.TYPE with type key = Name.t with type value = Object.t

(** [evaluate_term ~env term] evaluates the [term] within the
    [env]ironment and returns the resulting object. *)
val evaluate_term : env:Environment.t -> Tail.Term.t -> Outcome.t

(** [apply_term ~env term1 term2] applies [term1] to [term2]
    within the [env]ironment and returns the resulting object. *)
val apply_term : env:Environment.t -> Tail.Term.t -> Tail.Term.t -> Outcome.t

(** [evaluate_statement ~env ~painter statement] evaluates the
    [statement] within the [env]ironment and returns the new
    environment. *)
val evaluate_statement
  :  env:Environment.t
  -> painter:(module Painter.TYPE)
  -> Tail.Statement.t
  -> (Environment.t, (Exception.t, Unreachable.t) Either.t) result

(** [evaluate_program ~env ~painter program] evaluates the
    program within the [env]ironment and returns the new
    environment. *)
val evaluate_program
  :  env:Environment.t
  -> painter:(module Painter.TYPE)
  -> Tail.Program.t
  -> (Environment.t, (Exception.t, Unreachable.t) Either.t) result

(** [evaluate ?env ~painter program] evaluates the program
    returns an exit code. *)
val evaluate
  :  ?env:Environment.t
  -> painter:(module Painter.TYPE)
  -> Tail.Program.t
  -> int

(** (Incomplete) implementation of the compiler intrinsics at
    runtime. *)
val intrinsics : Environment.t
