open Custom

type t = Statement.t list

(** [show program] produces a pretty-printable representation of
    the [program]. *)
val show : (module Painter.TYPE) -> t -> string
