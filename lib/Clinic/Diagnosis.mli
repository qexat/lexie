open Custom

(** A record representing a type mismatch. *)
type type_mismatch =
  { expected : AIL.Type.t
  ; found : AIL.Type.t
  }

(** A record representing information about a term. *)
type term_info =
  { term : AIL.Term.t
  ; ty : AIL.Type.t
  }

(** The kind of diagnosis. *)
type t =
  | Annotation_type_mismatch of type_mismatch
  | Argument_type_mismatch of type_mismatch
  | Expected_type of AIL.Type.t
  | Hole_found
  | Name_not_found of Custom.Name.t
  | Non_functional_application of term_info
  | Unsupported_intrinsics_at_runtime

(** [show painter diagnosis] produces a pretty-printable
    representation of the [diagnosis] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
