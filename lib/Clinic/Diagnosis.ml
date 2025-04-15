open Custom

type type_mismatch =
  { expected : Ail.Kind.t
  ; found : Ail.Kind.t
  }

type term_info =
  { term : Ail.Term.t
  ; kind : Ail.Kind.t
  }

type t =
  | Annotation_type_mismatch of type_mismatch
  | Argument_type_mismatch of type_mismatch
  | Expected_type of Ail.Kind.t
  | Hole_found
  | Name_not_found of Name.t
  | Non_functional_application of term_info
  | Unsupported_intrinsics_at_runtime

let show =
  fun painter diagnosis ->
  let module Painter = (val painter : Painter.TYPE) in
  match diagnosis with
  | Annotation_type_mismatch { expected; found } ->
    Printf.sprintf
      "I expected a value of type %s (as given by the annotation), but found a %s instead"
      (Ail.Kind.show painter expected)
      (Ail.Kind.show painter found)
  | Argument_type_mismatch { expected; found } ->
    Printf.sprintf
      "I expected an argument of type %s, but found a %s"
      (Ail.Kind.show painter expected)
      (Ail.Kind.show painter found)
  | Expected_type kind -> Printf.sprintf "I expect a %s here" (Ail.Kind.show painter kind)
  | Hole_found -> Printf.sprintf "there is a hole here"
  | Name_not_found name ->
    Printf.sprintf "I could not find the name %s" (Name.show painter name)
  | Non_functional_application { term; kind } ->
    Printf.sprintf
      "the term %s (of type %s) is not a function, it cannot be applied"
      (Ail.Term.show painter term)
      (Ail.Kind.show painter kind)
  | Unsupported_intrinsics_at_runtime ->
    "compiler intrinsics are not fully supported by the runtime and may cause unexpected \
     crashes"
;;
