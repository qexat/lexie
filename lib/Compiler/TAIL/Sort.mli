type t =
  | Prop
  | Type

(** [show painter sort] produces a pretty-printable
    representation of the [sort] using the [painter]. *)
val show : (module Custom.Painter.TYPE) -> t -> string
