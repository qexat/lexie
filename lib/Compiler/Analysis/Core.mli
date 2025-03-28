open Custom
open Common
module Context : Quickmap.TYPE with type key = Name.t with type value = Kind.t

(* private API *)
val fetch_term : nurse:Nurse.t -> context:Context.t -> Name.t -> Kind.t option
val try_apply : nurse:Nurse.t -> context:Context.t -> Term.t -> Term.t -> Kind.t option
val propagate_parameter : Parameter.t -> Kind.t -> Kind.t

(* public API *)
val infer_sort_of_sort : nurse:Nurse.t -> context:Context.t -> Sort.t -> Sort.t option
val infer_kind_of_kind : nurse:Nurse.t -> context:Context.t -> Kind.t -> Kind.t option

val infer_kind_of_arrow
  :  nurse:Nurse.t
  -> context:Context.t
  -> Parameter.t
  -> Kind.t
  -> Kind.t option

val infer_kind_of_term : nurse:Nurse.t -> context:Context.t -> Term.t -> Kind.t option

val infer_kind_of_primitive
  :  nurse:Nurse.t
  -> context:Context.t
  -> Primitive.t
  -> Kind.t option

val infer_kind_of_parameter
  :  nurse:Nurse.t
  -> context:Context.t
  -> Parameter.t
  -> Kind.t option

val check_kind : nurse:Nurse.t -> expected:Kind.t -> Kind.t -> bool
val check_sort : nurse:Nurse.t -> expected:Sort.t -> Sort.t -> bool
val check_term : nurse:Nurse.t -> expected:Term.t -> Term.t -> bool
val check_parameter : nurse:Nurse.t -> expected:Parameter.t -> Parameter.t -> bool

val check_statement
  :  nurse:Nurse.t
  -> context:Context.t
  -> Statement.t
  -> Context.t option

val check_program : nurse:Nurse.t -> context:Context.t -> Program.t -> Context.t option
val check : ?nurse:Nurse.t -> ?context:Context.t -> Program.t -> Context.t option
val intrinsics : Context.t
