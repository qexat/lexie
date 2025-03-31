open Custom

type t =
  | Let of Name.t * Kind.t option * Term.t
  | Print of Term.t

(** [let' name ?annotation body] creates a [Let] statement given
    a binding's [name], an optional [annotation] and its [body]
    term. *)
val let' : Name.t -> ?annotation:Kind.t -> Term.t -> t

(** [print term] creates a [Print] statement given a [term]. *)
val print : Term.t -> t

(** [show painter statement] produces a pretty-printable
    representation of the [statement] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
