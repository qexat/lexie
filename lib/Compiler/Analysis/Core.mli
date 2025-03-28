open Custom
open Clinic
open Common
open Tail
module Context : Quickmap.TYPE with type key = Name.t with type value = Kind.t

(* private API *)
val fetch_term : doctor:Doctor.t -> context:Context.t -> Name.t -> Kind.t option
val try_apply : doctor:Doctor.t -> context:Context.t -> Term.t -> Term.t -> Kind.t option
val propagate_parameter : Parameter.t -> Kind.t -> Kind.t

(* public API *)
val infer_sort_of_sort : doctor:Doctor.t -> context:Context.t -> Sort.t -> Sort.t option
val infer_kind_of_kind : doctor:Doctor.t -> context:Context.t -> Kind.t -> Kind.t option

val infer_kind_of_arrow
  :  doctor:Doctor.t
  -> context:Context.t
  -> Parameter.t
  -> Kind.t
  -> Kind.t option

val infer_kind_of_term : doctor:Doctor.t -> context:Context.t -> Term.t -> Kind.t option

val infer_kind_of_primitive
  :  doctor:Doctor.t
  -> context:Context.t
  -> Primitive.t
  -> Kind.t option

val infer_kind_of_parameter
  :  doctor:Doctor.t
  -> context:Context.t
  -> Parameter.t
  -> Kind.t option

val check_kind : doctor:Doctor.t -> expected:Kind.t -> Kind.t -> bool
val check_sort : doctor:Doctor.t -> expected:Sort.t -> Sort.t -> bool
val check_term : doctor:Doctor.t -> expected:Term.t -> Term.t -> bool
val check_parameter : doctor:Doctor.t -> expected:Parameter.t -> Parameter.t -> bool

val check_statement
  :  doctor:Doctor.t
  -> context:Context.t
  -> Statement.t
  -> Context.t option

val check_program : doctor:Doctor.t -> context:Context.t -> Program.t -> Context.t option
val check : doctor:Doctor.t -> ?context:Context.t -> Program.t -> Context.t option
val intrinsics : Context.t
