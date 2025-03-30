open Custom

(** A runtime error that is symptomatic of a bug in the
    compiler. *)
type t =
  | Illegal_application
  | Undefined_name of Name.t

(** [show painter unreachable] produces a pretty-printable
    representation of the [unreachable] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
