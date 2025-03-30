open Custom

(** A diagnostic is a record of a diagnosis and which category
    it falls in ([Error], [Warning], etc). *)
type t =
  { category : Category.t
  ; diagnosis : Diagnosis.t
  }

(** [is_error diagnostic] determines whether the [diagnostic]
    falls in the [Error] category. *)
val is_error : t -> bool

(** [is_warning diagnostic] determines whether the [diagnostic]
    falls in the [Warning] category. *)
val is_warning : t -> bool

(** [show painter diagnostic] produces a pretty-printable
    representation of the [diagnostic] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
