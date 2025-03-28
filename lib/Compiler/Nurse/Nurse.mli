open Custom

type 'a renderer = (module Painter.TYPE) -> 'a -> string
type t

(** [create ()] returns a new nurse. *)
val create : unit -> t

(** [set_diagnosis_renderer renderer nurse] sets the diagnosis
    renderer for the [nurse] to [renderer]. *)
val set_diagnosis_renderer : Diagnostic.Diagnosis.t renderer -> t -> unit

(** [add_diagnostic diagnostic nurse] gives the [diagnostic] to
    the nurse. *)
val add_diagnostic : Diagnostic.t -> t -> unit

(** [add_error diagnosis nurse] gives the error [diagnosis] to
    the nurse. *)
val add_error : Diagnostic.Diagnosis.t -> t -> unit

(** [add_warning diagnosis nurse] gives the warning [diagnosis]
    to the nurse. *)
val add_warning : Diagnostic.Diagnosis.t -> t -> unit

(** [add_info diagnosis nurse] gives the info [diagnosis] to the
    nurse. *)
val add_info : Diagnostic.Diagnosis.t -> t -> unit

(** [turn_warnings_into_errors nurse] transforms all registered
    warnings into errors. *)
val turn_warnings_into_errors : t -> unit

(** [report ?out painter nurse] lets the [nurse] report the
    registered diagnostics to the [out] channel.
    By default, [out] is the standard error. *)
val report : ?out:out_channel -> (module Painter.TYPE) -> t -> unit
