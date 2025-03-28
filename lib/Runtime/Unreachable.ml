(** For runtime errors that should not be reached or canaries
    symptomatic of a bug in the compiler. *)
open Custom

type t =
  | Illegal_application
  | Undefined_name of Name.t
