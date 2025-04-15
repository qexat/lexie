open Batteries
open Custom
open Common

type t = Lang.term

(** {2 Constructors} *)

(** [app t1 t2] produces an application term where [t1] is the
    functional term and [t2] is the argument. *)
val app : t -> t -> t

(** [lambda param term] produces the function term where
    [param] is the parameter and [term] the body. Note that
    functions are curried. *)
val lambda : Lang.parameter -> t -> t

(** [hole] is the hole term. *)
val hole : t

(** [primitive prim] produces a primitive term from a
    primitive value. *)
val primitive : Primitive.t -> t

(** [var name] produces a variable term from a [name]. *)
val var : Name.t -> t

(** {2 Shorthands} *)

(** [app2 t1 t2 t3] produces an application term where [t1]
    is the functional term and [t2] and [t3] are the first
    and second arguments respectively. *)
val app2 : t -> t -> t -> t

(** [nat n] produces a primitive [Nat] term given a positive
    integer [n].*)
val nat : Intp.t -> t

(** [unit] is the primitive [Unit] term. *)
val unit : t

(** {2 Other} *)

(** [to_syntactic_kind term] returns the syntactic kind of the
    given [term]. *)
val to_syntactic_kind : t -> Syntactic_kind.t

(** [show painter term] produces a pretty-printable
    representation of [term]. *)
val show : (module Painter.TYPE) -> t -> string
