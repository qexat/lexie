(** Represents a non-negative integer (Z+). *)
type t = private int

(** Zero. *)
val zero : t

(** One. *)
val one : t

(** [equal p1 p2] determines whether [p1] and [p2] are equal. *)
val equal : t -> t -> bool

(** [compare p1 p2] determines whether [p1] is greater, equal or
    less than [p2]. *)
val compare : t -> t -> int

(** [succ p] is [p + 1]. *)
val succ : t -> t

(** [pred p] is [p - 1], except for [0] where it returns itself. *)
val pred : t -> t

(** [add p1 p2] is [p1 + p2]. *)
val add : t -> t -> t

(** [sub p1 p2] is [p1 - p2] for [p1 >= p2] ; otherwise, it
    returns [0]. *)
val sub : t -> t -> t

(** [mul p1 p2] is [p1 * p2]. *)
val mul : t -> t -> t

(** [div p1 p2] is [p1 / p2]. *)
val div : t -> t -> t

(** [modulo p1 p2] is [p1 mod p2]. *)
val modulo : t -> t -> t

(** [rem p1 p2] returns the remainder of the division of [p1] by
    [p2]. *)
val rem : t -> t -> t

(** [min p1 p2] returns the smallest value between [p1] and
    [p2]. *)
val min : t -> t -> t

(** [max p1 p2] returns the largest value between [p1] and
    [p2]. *)
val max : t -> t -> t

(** [abs p] returns [p], since it is always positive.
    Exists for consistency with [Int]. *)
val abs : t -> t

(** [to_int p] converts [p] to a built-in [int]. *)
val to_int : t -> int

(** [to_float p] converts [p] to a built-in [float]. *)
val to_float : t -> float

(** [to_string p] converts [p] to a built-in [string]. *)
val to_string : t -> string

(** [of_int i] converts the integer [i] into a positive one.
    Returns [None] if [i] is negative. *)
val of_int : int -> t option

(** [of_int_exn i] converts the integer [i] into a positive one.
    Raises [Invalid_arg] if [i] is negative. *)
val of_int_exn : int -> t

module Notation : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val ( % ) : t -> t -> t
  val ( ~++ ) : t -> t
  val ( ~-- ) : t -> t
end
