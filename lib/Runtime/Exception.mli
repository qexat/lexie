open Custom

(** The kind of runtime errors. *)
type kind = Incomplete_program

(** A runtime error. *)
type t = { kind : kind }

(** [show painter exn] produces a pretty-printable
    representation of [exn] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
