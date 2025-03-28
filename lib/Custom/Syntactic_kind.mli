(** A syntactic kind is a group of term elements that have
    the same precedence. *)
type t =
  | Atom
  | Grouping
  | App
  | Binary
  | Fun

(** [equal sk1 sk2] determines whether [sk1] and [sk2] are
    the same. *)
val equal : t -> t -> bool

(** [compare sk1 sk2] returns whether [sk1] is greater (>0),
    equal (=0) or less (<0) than [sk2]. *)
val compare : t -> t -> int

(** [binds_tighter sk1 ~than:sk2] determines whether [sk1]
    binds more tightly (higher precedence) than [sk2]. *)
val binds_tighter : t -> than:t -> bool
