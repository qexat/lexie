open Batteries

(** A primitive value is a value built into the language. *)
type t =
  | Nat of Intp.t
  | Unit

(** [nat value] constructs a [Nat] from a positive integer. *)
val nat : Intp.t -> t

(** A [unit] primitive value. *)
val unit : t

(** [equal prim1 prim2] determines whether [prim1] and
    [prim2] are the same. *)
val equal : t -> t -> bool

(** [show painter prim] produces a pretty-printable string that
    represents [prim] using [painter]. *)
val show : (module Custom.Painter.TYPE) -> t -> string
