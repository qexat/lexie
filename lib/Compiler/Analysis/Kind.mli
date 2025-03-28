open Custom

type t = Lang.kind

(** [arrow param ret] produces an arrow kind where [param] and
    [ret] are on the left and right handside of the arrow
    respectively. *)
val arrow : Lang.parameter -> t -> t

(** [sort s] produces a sort kind where [s] is the underlying
    sort. *)
val sort : Sort.t -> t

(** [term t] produces a term kind where [t] is the underlying
    term. *)
val term : Term.t -> t

(** [show painter kind] produces a pretty-printable
    representation of [kind]. *)
val show : (module Painter.TYPE) -> t -> string
