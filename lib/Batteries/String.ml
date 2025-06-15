include Stdlib.String

let indent_line =
  fun ?(width = 4) ?(count = 1) ?(marker = ' ') string ->
  let indent = Printf.sprintf "%c%*s" marker (width - 1) " " in
  Printf.sprintf "%*s%s" count indent string
;;

let indent =
  fun ?(width = 4) ?(count = 1) ?(marker = ' ') string ->
  split_on_char '\n' string
  |> List.map (indent_line ~width ~count ~marker)
  |> concat "\n"
;;

module Notation = struct
  let ( <> ) = cat
end

module type REFINED = sig
  type t = private string

  val predicate : string -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val length : t -> int
  val lift : 'r. (string -> string) -> t -> t option
  val lift_exn : 'r. (string -> string) -> t -> t
  val to_string : t -> string
  val to_seq : t -> char Seq.t
  val of_string : string -> t option
  val of_string_exn : string -> t
end

module type PREDICATE = sig
  val predicate : string -> bool
end

module Constrain (P : PREDICATE) : REFINED = struct
  type t = string

  let predicate = P.predicate
  let equal = equal
  let compare = compare
  let length = length

  let lift =
    fun func refined ->
    let result = func refined in
    match predicate result with
    | true -> Some result
    | false -> None
  ;;

  let lift_exn =
    fun func refined ->
    match lift func refined with
    | None ->
      failwith
        "func did not return a string that satisfies the \
         predicate"
    | Some result -> result
  ;;

  let to_string = fun refined -> refined
  let to_seq = to_seq

  let of_string =
    fun string ->
    match predicate string with
    | false -> None
    | true -> Some string
  ;;

  let of_string_exn =
    fun string ->
    match of_string string with
    | None ->
      invalid_arg "string does not satisfy the predicate"
    | Some refined -> refined
  ;;
end
