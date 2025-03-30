include Batteries.String.REFINED
module Type : Batteries.String.REFINED

(** [is_type name] determines whether the [name] is a valid type
    name. *)
val is_type : t -> bool

(** [show painter name] produces a pretty-printable
    representation of the [name] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
