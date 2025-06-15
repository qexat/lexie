open Custom
open Clinic
open Common
open AIL

module Context :
  Quickmap.TYPE with type key = Name.t with type value = Kind.t

(** [infer_sort_of_sort sort] infers the sort of [sort].
    Returns [None] if it fails to do so. *)
val infer_sort_of_sort : Sort.t -> Sort.t option

(** [infer_kind_of_kind ~doctor ~context kind] infers the kind
    of [kind] given a [context].
    Returns [None] if it fails to do so. *)
val infer_kind_of_kind
  :  doctor:Doctor.t
  -> context:Context.t
  -> Kind.t
  -> Kind.t option

(** [infer_kind_of_term ~doctor ~context term] infers the kind
    of [term] given a [context].
    Returns [None] if it fails to do so. *)
val infer_kind_of_term
  :  doctor:Doctor.t
  -> context:Context.t
  -> Term.t
  -> Kind.t option

(** [infer_kind_of_primitive primitive] infers the kind of
    [primitive] given a [context].
    Returns [None] if it fails to do so. *)
val infer_kind_of_primitive : Primitive.t -> Kind.t option

(** [infer_kind_of_parameterparameter] infers the kind of
    [parameter] given a [context].
    Returns [None] if it fails to do so. *)
val infer_kind_of_parameter : Parameter.t -> Kind.t option

(** [check_kind ~doctor ~expected found] determines whether the
    kind [found] is compatible with the [expected] one. *)
val check_kind
  :  doctor:Doctor.t
  -> expected:Kind.t
  -> Kind.t
  -> bool

(** [check_sort ~expected found] determines whether the sort
    [found] is compatible with the [expected] one. *)
val check_sort : expected:Sort.t -> Sort.t -> bool

(** [check_term ~doctor ~expected found] determines whether the
    term [found] is compatible with the [expected] one. *)
val check_term
  :  doctor:Doctor.t
  -> expected:Term.t
  -> Term.t
  -> bool

(** [check_parameter ~doctor ~expected found] determines whether
    the parameter [found] is compatible with the [expected] one. *)
val check_parameter
  :  doctor:Doctor.t
  -> expected:Parameter.t
  -> Parameter.t
  -> bool

(** [check_statement ~doctor ~context statement] infers given
    a [context] the type of underlying terms and checks whether
    the [statement] is type-safe.
    Returns the new context, or [None] upon failure. *)
val check_statement
  :  doctor:Doctor.t
  -> context:Context.t
  -> Statement.t
  -> Context.t option

(** [check_program ~doctor ?context program] infers given a
    [context] the type of underlying terms and checks whether
    the [program] is type-safe.
    If [context] is not provided, an empty one is created.
    Returns the new context, or [None] upon failure. *)
val check_program
  :  doctor:Doctor.t
  -> ?context:Context.t
  -> Program.t
  -> Context.t option

(** Compiler intrinsics. *)
val intrinsics : Context.t
