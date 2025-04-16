type t = Lang.task

(** [return term] creates a new [Done] task with the [term]. *)
val return : Term.t -> t

(** [future syntax kind] creates a new [Future] task on the
    non-terminating [syntax] of the provided [kind]. *)
val future : string -> Kind.t -> t

(** [show painter task] produces a pretty-printable
    representation of the [task] using the [painter]. *)
val show : (module Custom.Painter.TYPE) -> t -> string
