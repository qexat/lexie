open Custom

(** A doctor manages diagnostics. *)
type t

module Config : sig
  type t = { strict : bool }

  (** [create ?strict ()] creates a clinic configuration without
        having to specify all fields, providing good defaults. *)
  val create : ?strict:bool -> unit -> t
end

(** [create config] creates a new doctor given a [config]. *)
val create : Config.t -> t

(** [get_config doctor] returns the config that was used to
    create the [doctor]. *)
val get_config : t -> Config.t

(** [add_diagnostic diagnostic doctor] adds the [diagnostic] to
    the [doctor] documents. *)
val add_diagnostic : Diagnostic.t -> t -> unit

(** [add_error diagnosis doctor] is a shorthand to create an
    [Error] diagnostic given a [diagnosis] and add the latter to
    the [doctor] documents. *)
val add_error : Diagnosis.t -> t -> unit

(** [add_warning diagnosis doctor] is a shorthand to create an
    [Warning] diagnostic given a [diagnosis] and add the latter
    to the [doctor] documents. *)
val add_warning : Diagnosis.t -> t -> unit

(** [add_info diagnosis doctor] is a shorthand to create an
    [Info] diagnostic given a [diagnosis] and add the latter
    to the [doctor] documents. *)
val add_info : Diagnosis.t -> t -> unit

(** Represents a doctor decision after analysing the diagnostics
    it has registered. *)
type decision =
  | Pass
  | Abort

(** The result of a doctor review of the registered diagnostics. *)
type review =
  { decision : decision
  ; details : string option
  }

(** [review painter doctor] reviews the currently registered
    diagnostics and render the result using the [painter]. *)
val review : (module Painter.TYPE) -> t -> review

(** [emit_single_diagnostic painter config diagnostic] is a
    shorthand to create a temporary doctor with the given
    [config], add the [diagnostic], and immediately review it
    using the [painter]. *)
val emit_single_diagnostic : (module Painter.TYPE) -> Config.t -> Diagnostic.t -> review

(** [emit_single_error painter config diagnosis] is the same as
    [emit_single_diagnostic] but the diagnostic is an error
    created using the [diagnosis]. *)
val emit_single_error : (module Painter.TYPE) -> Config.t -> Diagnosis.t -> review

(** [emit_single_error painter config diagnosis] is the same as
    [emit_single_diagnostic] but the diagnostic is a warning
    created using the [diagnosis]. *)
val emit_single_warning : (module Painter.TYPE) -> Config.t -> Diagnosis.t -> review

(** [emit_single_error painter config diagnosis] is the same as
    [emit_single_diagnostic] but the diagnostic is an info
    created using the [diagnosis]. *)
val emit_single_info : (module Painter.TYPE) -> Config.t -> Diagnosis.t -> review
