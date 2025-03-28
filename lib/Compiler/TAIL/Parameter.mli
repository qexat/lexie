open Custom

type t = Lang.parameter

(** [name param] returns the name of the [param]eter. *)
val name : t -> Name.t

(** [kind param] returns the kind annotation of the [param]eter. *)
val kind : t -> Kind.t

(** [show painter param] produces a pretty-printable
    representation of the [param]eter. *)
val show : (module Painter.TYPE) -> t -> string
