open Custom

type t = Statement.t list

(** [show painter program] produces a pretty-printable
    representation of the [program] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
