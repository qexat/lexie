module type SHOWABLE = sig
  (** The showable type. *)
  type t

  (** [show painter value] produces a pretty-printable
      represention of the [value] using the [painter]. *)
  val show : (module Painter.TYPE) -> t -> string
end

module type TYPE = sig
  (** The [key] of the mapping. *)
  type key

  (** The [value] of the mapping. *)
  type value

  (** The mapping. *)
  type t

  (** A empty mapping. *)
  val empty : t

  (** [keys mapping] returns a list of the mapping keys. *)
  val keys : t -> key list

  (** [values mapping] returns a list of the mapping values. *)
  val values : t -> value list

  (** [get key mapping] returns the value associated with the
      [key] in the [mapping] if any. Returns [None] otherwise.*)
  val get : key -> t -> value option

  (** [add key value mapping] adds a [key] associated with
      [value] in the [mapping]. *)
  val add : key -> value -> t -> t

  (** [update key value mapping] updates the associated [value]
      of [key] if it exists in the [mapping]. *)
  val update : key -> value -> t -> t

  (** [concat mapping1 mapping2] returns a new mapping
      containing the entries of the two. *)
  val concat : t -> t -> t

  (** [show painter mapping] returns a pretty-printable
      representation of the [mapping] using the [painter]. *)
  val show : (module Painter.TYPE) -> t -> string
end

module Make (Key : SHOWABLE) (Value : SHOWABLE) :
  TYPE with type key = Key.t with type value = Value.t = struct
  type key = Key.t
  type value = Value.t
  type t = (key * value) list

  let empty = []
  let keys = List.map fst
  let values = List.map snd
  let get = List.assoc_opt

  let[@tail_mod_cons] rec update =
    fun key value mapping ->
    match mapping with
    | [] -> []
    | (key', _) :: rest when key = key' -> (key, value) :: rest
    | (key', value') :: rest -> (key', value') :: update key value rest
  ;;

  let add = fun key value mapping -> (key, value) :: mapping
  let concat = List.append

  let show =
    fun painter mapping ->
    mapping
    |> List.rev_map (fun (key, value) ->
      Printf.sprintf "%s : %s" (Key.show painter key) (Value.show painter value))
    |> String.concat " ; "
    |> Printf.sprintf "[ %s ]"
  ;;
end
