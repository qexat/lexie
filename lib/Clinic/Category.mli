open Custom

(** Category of a diagnostic. *)
type t =
  | Error
  | Warning
  | Info

(** [name category] returns the name of the [category]. *)
val name : t -> string

(** [get_painter_function painter category] returns the function
    of the [painter] that should be used to represent the
    [category]. *)
val get_painter_function : (module Painter.TYPE) -> t -> string -> string

(** [show painter category] produces a pretty-printable
    representation of the [category] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
