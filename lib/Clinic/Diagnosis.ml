open Custom

type type_mismatch =
  { expected : Tail.Kind.t
  ; found : Tail.Kind.t
  }

type term_info =
  { term : Tail.Term.t
  ; kind : Tail.Kind.t
  }

type t =
  | Annotation_type_mismatch of type_mismatch
  | Argument_type_mismatch of type_mismatch
  | Expected_type of Tail.Kind.t
  | Hole_found
  | Name_not_found of Name.t
  | Non_functional_application of term_info

let show =
  fun painter diagnosis ->
  let module Painter = (val painter : Painter.TYPE) in
  match diagnosis with
  | Annotation_type_mismatch { expected; found } ->
    Printf.sprintf
      "I expected a value of type %s (as given by the annotation), but found a %s instead"
      (Tail.Kind.show painter expected)
      (Tail.Kind.show painter found)
  | Argument_type_mismatch { expected; found } ->
    Printf.sprintf
      "I expected an argument of type %s, but found a %s"
      (Tail.Kind.show painter expected)
      (Tail.Kind.show painter found)
  | Expected_type kind ->
    Printf.sprintf "I expect a %s here" (Tail.Kind.show painter kind)
  | Hole_found -> Printf.sprintf "there is a hole here"
  | Name_not_found name ->
    Printf.sprintf "I could not find the name %s" (Name.show painter name)
  | Non_functional_application { term; kind } ->
    Printf.sprintf
      "the term %s (of type %s) is not a function, it cannot be applied"
      (Tail.Term.show painter term)
      (Tail.Kind.show painter kind)
;;
