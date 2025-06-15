include module type of Stdlib.String

(** [indent ~width ~count ~marker text] indents each line of
    [text] by [count] tabulations of [width]. [marker] can
    be specified if each indent should start with a non-
    whitespace marker. *)
val indent : ?width:int -> ?count:int -> ?marker:char -> t -> t

(** [indent_line ~width ~count ~marker line] indents a
    single [line] by [count] tabulations of [width].
    [marker] can specified if the indent should start with a
    non-whitespace marker. *)
val indent_line
  :  ?width:int
  -> ?count:int
  -> ?marker:char
  -> t
  -> t

module Notation : sig
  val ( <> ) : t -> t -> t
end

module type REFINED = sig
  (** String that satisfies a predicate. *)
  type t = private string

  (** Predicate that is always satisfied by the values of this
      type. *)
  val predicate : string -> bool

  (** [equal s1 s2] determines whether [s1] and [s2] are the
      same string. *)
  val equal : t -> t -> bool

  (** [compare s1 s2] determines whether [s1] is
      lexicographically before, at the same place or after [s2]. *)
  val compare : t -> t -> int

  (** [length s] returns the number of characters in [s]. *)
  val length : t -> int

  (** [lift func s] applies [func] to [s] as if it was a normal
      string, but preserves the refinement of the output.
      Returns [None] if the produced string does not satisfy the
      predicate. *)
  val lift : 'r. (string -> string) -> t -> t option

  (** [lift_exn func s] is the same as [lift func s], but a
      [Failure] exception is raised in the erroneous case. *)
  val lift_exn : 'r. (string -> string) -> t -> t

  (** [to_string s] "forgets" the refinement and converts [s]
      back to a normal string. *)
  val to_string : t -> string

  (** [to_seq s] converts [s] into a sequence of characters. *)
  val to_seq : t -> char Seq.t

  (** [of_string s] refines [s] by trying to guarantee that the
      output will always satisfy [predicate]. Returns [None] if
      it fails to do so. *)
  val of_string : string -> t option

  (** [of_string_exn s] is the same as [of_string s], but an
      [Invalid_arg] exception is raised in the erroneous case. *)
  val of_string_exn : string -> t
end

module type PREDICATE = sig
  val predicate : string -> bool
end

module Constrain : functor (_ : PREDICATE) -> REFINED
