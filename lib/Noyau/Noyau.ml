[@@@warning "-65"]

module rec Pervasives : sig
  (** [unit] is a type with a single inhabitant, the empty
      tuple. *)
  type unit = ()

  type nonrec bool = bool

  type compare =
    | Equal
    | Greater
    | Less

  type 'item maybe =
    | Nothing
    | Some of 'item

  type ('left, 'right) either =
    | Left of 'left
    | Right of 'right

  type ('value, 'error) result =
    | Ok of 'value
    | Error of 'error

  type nat =
    | O
    | S of nat

  type nonrec int = int
  type nonrec float = float
  type nonrec char = char
  type nonrec string = string
  type code_point = Uchar.t

  type unicode =
    | Empty
    | Ucons of (code_point * unicode)

  type 'item list =
    | []
    | ( :: ) of ('item * 'item list)

  type ('first, 'second) pair = 'first * 'second

  type ('first, 'second, 'third) triple =
    'first * 'second * 'third

  type lambda =
    | App of lambda * lambda
    | Fun of string * lambda
    | Var of string

  (* functions *)
  val not : bool -> bool
  val succ : nat -> nat

  (* operators *)
  val ( && ) : bool -> bool -> bool
  val ( || ) : bool -> bool -> bool
  val ( |> ) : 'arg 'ret. 'arg -> ('arg -> 'ret) -> 'ret
end = struct
  type nonrec unit = ()
  type nonrec bool = bool

  type compare =
    | Equal
    | Greater
    | Less

  type 'item maybe =
    | Nothing
    | Some of 'item

  type ('left, 'right) either =
    | Left of 'left
    | Right of 'right

  type ('value, 'error) result =
    | Ok of 'value
    | Error of 'error

  type nat =
    | O
    | S of nat

  type nonrec int = int
  type nonrec float = float
  type code_point = Uchar.t (* TODO *)
  type nonrec char = char
  type nonrec string = string

  type unicode =
    | Empty
    | Ucons of (code_point * unicode)

  type 'item list =
    | []
    | ( :: ) of ('item * 'item list)

  type ('first, 'second) pair = 'first * 'second

  type ('first, 'second, 'third) triple =
    'first * 'second * 'third

  type lambda =
    | App of lambda * lambda
    | Fun of string * lambda
    | Var of string

  let not = Bool.not
  let ( && ) = Bool.( && )
  let ( || ) = Bool.( || )
  let succ = Nat.succ
  let ( |> ) = Fun.( |> )
end

and Unit : sig
  type ty = Pervasives.unit

  val compare : ty -> ty -> Compare.ty
  val equal : ty -> ty -> Bool.ty
end = struct
  open Pervasives

  type ty = unit

  let compare : ty -> ty -> compare = fun () () -> Equal
  let equal : ty -> ty -> bool = fun () () -> true
end

and Bool : sig
  type ty = Pervasives.bool

  val compare : ty -> ty -> Compare.ty
  val equal : ty -> ty -> ty
  val not : ty -> ty
  val ( && ) : ty -> ty -> ty
  val ( || ) : ty -> ty -> ty
end = struct
  open Pervasives

  type ty = bool

  let compare : ty -> ty -> compare =
    fun left right ->
    match left, right with
    | false, false | true, true -> Equal
    | true, false -> Greater
    | false, true -> Less
  ;;

  let equal : ty -> ty -> bool =
    fun left right -> Compare.(compare left right = Equal)
  ;;

  let not : ty -> ty = function
    | false -> true
    | true -> false
  ;;

  let ( && ) left right =
    match left, right with
    | true, true -> true
    | _, _ -> false
  ;;

  let ( || ) left right =
    match left, right with
    | false, false -> false
    | _, _ -> true
  ;;
end

and Compare : sig
  type ty = Pervasives.compare

  val ( = ) : ty -> ty -> Bool.ty
  val is_either : left:ty -> right:ty -> ty -> Bool.ty
  val to_int : ty -> int
  val of_int : int -> ty
end = struct
  (* NOTE: Compare does not define [compare] for itself because
     1) I don't see how it is meaningful.
     2) It makes comparison code more annoying.
        Consider:

          Compare.(compare foo bar = Equal)

        If [Compare.compare] was a function, users (me) would
        have to refactor this simple piece into a less readable
        two-line expression. *)

  open Pervasives

  type ty = compare

  let ( = ) left right =
    match left, right with
    | Equal, Equal | Greater, Greater | Less, Less -> true
    | _, _ -> false
  ;;

  (* [is_either] is mostly useful for [>=] and [<=]. *)
  let is_either ~left ~right compare =
    compare = left || compare = right
  ;;

  (* [to_int] and [of_int] are defined for "compatibility" with
     the standard library, but their use is really discouraged. *)

  let to_int compare =
    match compare with
    | Equal -> 0
    | Greater -> 1
    | Less -> -1
  ;;

  let of_int value =
    if value > 0
    then Greater
    else if value < 0
    then Less
    else Equal
  ;;
end

and Maybe : sig
  type 'item ty = 'item Pervasives.maybe

  val nothing : 'item. 'item ty
  val some : 'item. 'item -> 'item ty

  val compare
    : 'item.
    ('item -> 'item -> Compare.ty)
    -> 'item ty
    -> 'item ty
    -> Compare.ty

  val equal
    : 'item.
    ('item -> 'item -> Bool.ty)
    -> 'item ty
    -> 'item ty
    -> Bool.ty

  val are_same_variants : 'item. 'item ty -> 'item ty -> Bool.ty

  val map
    : 'item1 'item2.
    ('item1 -> 'item2) -> 'item1 ty -> 'item2 ty

  val map2
    : 'item1 'item2 'item3.
    ('item1 -> 'item2 -> 'item3)
    -> 'item1 ty
    -> 'item2 ty
    -> 'item3 ty

  val lift : 'item. 'item -> 'item ty

  val apply
    : 'item1 'item2.
    ('item1 -> 'item2) ty -> 'item1 ty -> 'item2 ty

  val join : 'item. 'item ty ty -> 'item ty

  val bind
    : 'item1 'item2.
    'item1 ty -> ('item1 -> 'item2 ty) -> 'item2 ty

  val perform : 'item. ('item -> Unit.ty) -> 'item ty -> Unit.ty
  val get : 'item. 'item ty -> fallback:'item -> 'item

  val get_lazy
    : 'item.
    'item ty -> fallback:(Unit.ty -> 'item) -> 'item

  val to_result
    : 'item 'error.
    nothing:'error -> 'item ty -> ('item, 'error) Result.ty

  val to_result_lazy
    : 'item 'error.
    nothing:(Unit.ty -> 'error)
    -> 'item ty
    -> ('item, 'error) Result.ty

  val to_list : 'item. 'item ty -> 'item List.ty
  val ( or ) : 'item. 'item ty -> 'item ty -> 'item ty

  val ( ||> )
    : 'item.
    'item ty -> (Unit.ty -> 'item ty) -> 'item ty

  val ( let+ )
    : 'item1 'item2.
    'item1 ty -> ('item1 -> 'item2 ty) -> 'item2 ty

  val ( let- )
    : 'item.
    'item ty -> (Unit.ty -> 'item ty) -> 'item ty
end = struct
  open Pervasives

  type 'item ty = 'item maybe

  let nothing : type item. item ty = Nothing
  let some : type item. item -> item ty = fun item -> Some item

  let compare
    : type item.
      (item -> item -> compare) -> item ty -> item ty -> compare
    =
    fun item_compare left right ->
    match left, right with
    | Some item_left, Some item_right ->
      item_compare item_left item_right
    | Some _, Nothing -> Greater
    | Nothing, Some _ -> Less
    | Nothing, Nothing -> Equal
  ;;

  let equal
    : type item.
      (item -> item -> bool) -> item ty -> item ty -> bool
    =
    fun item_equal left right ->
    match left, right with
    | Some item_left, Some item_right ->
      item_equal item_left item_right
    | Nothing, Nothing -> true
    | _, _ -> false
  ;;

  let are_same_variants
    : type item1 item2. item1 ty -> item2 ty -> bool
    =
    fun left right ->
    match left, right with
    | Nothing, Nothing | Some _, Some _ -> true
    | _, _ -> false
  ;;

  let map
    : type item1 item2. (item1 -> item2) -> item1 ty -> item2 ty
    =
    fun f -> function
    | Nothing -> Nothing
    | Some item -> Some (f item)
  ;;

  let bind
    : type item1 item2.
      item1 ty -> (item1 -> item2 ty) -> item2 ty
    =
    fun maybe f ->
    match map f maybe with
    | Nothing -> Nothing
    | Some maybe_item -> maybe_item
  ;;

  let map2
    : type item1 item2 item3.
      (item1 -> item2 -> item3)
      -> item1 ty
      -> item2 ty
      -> item3 ty
    =
    fun f left right -> bind (map f left) (Fun.flip map right)
  ;;

  let apply
    : type item1 item2.
      (item1 -> item2) ty -> item1 ty -> item2 ty
    =
    fun maybe_f maybe_arg -> map2 Fun.( @@ ) maybe_f maybe_arg
  ;;

  let lift = some

  let join : type item. item ty ty -> item ty =
    fun maybe -> bind maybe Fun.identity
  ;;

  let perform : type item. (item -> unit) -> item ty -> unit =
    fun proc maybe ->
    match maybe with
    | Nothing -> ()
    | Some item -> proc item
  ;;

  let get_lazy
    : type item. item ty -> fallback:(unit -> item) -> item
    =
    fun maybe ~fallback ->
    match maybe with
    | Nothing -> fallback ()
    | Some item -> item
  ;;

  let get : type item. item ty -> fallback:item -> item =
    fun maybe ~fallback ->
    match maybe with
    | Nothing -> fallback
    | Some item -> item
  ;;

  let to_result_lazy
    : type item error.
      nothing:(unit -> error) -> item ty -> (item, error) result
    =
    fun ~nothing maybe ->
    match maybe with
    | Some item -> Ok item
    | Nothing -> Error (nothing ())
  ;;

  let to_result
    : type item error.
      nothing:error -> item ty -> (item, error) result
    =
    fun ~nothing maybe ->
    to_result_lazy ~nothing:(fun () -> nothing) maybe
  ;;

  let to_list : type item. item ty -> item list = function
    | Nothing -> []
    | Some item -> [ item ]
  ;;

  let ( && ) left right =
    match left, right with
    | Some _, Some _ -> right
    | _, _ -> left
  ;;

  let ( ||> )
    : type item. item ty -> (unit -> item ty) -> item ty
    =
    fun left f ->
    match left with
    | Some _ -> left
    | Nothing -> f ()
  ;;

  let ( or ) : type item. item ty -> item ty -> item ty =
    fun left right -> left ||> fun () -> right
  ;;

  let first_some maybes = List.fold ( or ) Nothing maybes
  let last_some maybes = join (List.fold_on_first ( && ) maybes)
  let ( let+ ) = bind
  let ( let- ) = ( ||> )
end

and Either : sig
  type ('left, 'right) ty = ('left, 'right) Pervasives.either
end = struct
  open Pervasives

  type ('left, 'right) ty = ('left, 'right) either
end

and Result : sig
  type ('value, 'error) ty = ('value, 'error) Pervasives.result
end = struct
  open Pervasives

  type ('value, 'error) ty = ('value, 'error) result
end

and Nat : sig
  type ty = Pervasives.nat

  val zero : ty
  val succ : ty -> ty
  val pred : ty -> ty Maybe.ty
  val compare : ty -> ty -> Compare.ty
  val equal : ty -> ty -> Bool.ty
end = struct
  open Pervasives

  type ty = Pervasives.nat

  let zero = O
  let[@inline] succ n = S n

  let pred = function
    | O -> Nothing
    | S n' -> Some n'
  ;;

  let rec compare n m =
    match n, m with
    | O, O -> Equal
    | O, S _ -> Less
    | S _, O -> Greater
    | S n', S m' -> compare n' m'
  ;;

  let equal n m = Compare.(compare n m = Equal)
end

and Int : sig
  type ty = Pervasives.int
end = struct
  open Pervasives

  type ty = int
end

and Float : sig
  type ty = Pervasives.float
end = struct
  open Pervasives

  type ty = float
end

and Char : sig
  type ty = Pervasives.char
end = struct
  open Pervasives

  type ty = char
end

and String : sig
  type ty = Pervasives.string
end = struct
  open Pervasives

  type ty = string
end

and Code_point : sig
  type ty = Pervasives.code_point
end = struct
  open Pervasives

  type ty = code_point
end

and Unicode : sig
  type ty = Pervasives.unicode
end = struct
  open Pervasives

  type ty = unicode
end

and List : sig
  type 't ty = 't Pervasives.list

  val reverse_concat : 'item. 'item ty -> 'item ty -> 'item ty
  val reverse : 'item. 'item ty -> 'item ty
  val concat : 'item. 'item ty -> 'item ty -> 'item ty

  val fold
    : 'item 'acc.
    ('acc -> 'item -> 'acc) -> 'acc -> 'item ty -> 'acc

  val fold_on_first
    : 'item.
    ('item -> 'item -> 'item) -> 'item ty -> 'item Maybe.ty

  module Associative : sig
    type nonrec ('key, 'value) ty = ('key * 'value) ty
  end
end = struct
  open Pervasives

  type 'item ty = 'item list

  let rec reverse_concat (left : 'a ty) (right : 'a ty) : 'a ty =
    match left with
    | [] -> right
    | first :: rest -> reverse_concat rest (first :: right)
  ;;

  let reverse (list : 'a ty) : 'a ty = reverse_concat list []

  let concat (left : 'a ty) (right : 'a ty) : 'a ty =
    reverse_concat (reverse left) right
  ;;

  let rec fold (f : 'acc -> 'item -> 'acc) (initial : 'acc)
    = function
    | [] -> initial
    | first :: rest -> fold f (f initial first) rest
  ;;

  let fold_on_first f = function
    | [] -> Nothing
    | first :: rest -> Some (fold f first rest)
  ;;

  module Associative = struct
    type nonrec ('key, 'value) ty = ('key * 'value) ty
  end
end

and Pair : sig
  type ('first, 'second) ty = ('first, 'second) Pervasives.pair
end = struct
  open Pervasives

  type ('first, 'second) ty = ('first, 'second) pair
end

and Triple : sig
  type ('first, 'second, 'third) ty =
    ('first, 'second, 'third) Pervasives.triple
end = struct
  open Pervasives

  type ('first, 'second, 'third) ty =
    ('first, 'second, 'third) triple
end

and Lambda : sig
  type ty = Pervasives.lambda
end = struct
  open Pervasives

  type ty = lambda
end

and Fun : sig
  type ('param, 'ret) ty = 'param -> 'ret

  val identity : 't. ('t, 't) ty
  val const : 'a 'b. 'a -> 'b -> 'a
  val false' : 'a 'b. 'a -> 'b -> 'b

  val flip
    : 'left 'right 'ret.
    ('left -> 'right -> 'ret) -> 'right -> 'left -> 'ret

  val ( -*> )
    : 'param 'seal 'ret.
    ('param -> 'seal) -> ('seal -> 'ret) -> 'param -> 'ret

  val repeat : 't. n:Nat.ty -> ('t, 't) ty -> ('t, 't) ty

  val fix
    : 'param 'ret.
    (('param -> 'ret) -> 'param -> 'ret) -> 'param -> 'ret

  val contract
    : 'param 'ret.
    ('param -> 'param -> 'ret) -> 'param -> 'ret

  val tautology : 't. 't -> Bool.ty
  val antilogy : 't. 't -> Bool.ty
  val negate : 't. ('t -> Bool.ty) -> 't -> Bool.ty

  val negate_binary
    : 'left 'right.
    ('left -> 'right -> Bool.ty) -> 'left -> 'right -> Bool.ty

  val map
    : 'param1 'param2 'ret1 'ret2.
    ('param2 -> 'param1)
    -> ('ret1 -> 'ret2)
    -> ('param1, 'ret1) ty
    -> ('param2, 'ret2) ty

  val map_left
    : 'param1 'param2 'ret.
    ('param2 -> 'param1)
    -> ('param1, 'ret) ty
    -> ('param2, 'ret) ty

  val map_right
    : 'param 'ret1 'ret2.
    ('ret1 -> 'ret2) -> ('param, 'ret1) ty -> ('param, 'ret2) ty

  val ( |> ) : 'param 'ret. 'param -> ('param, 'ret) ty -> 'ret
  val ( @@ ) : 'param 'ret. ('param, 'ret) ty -> 'param -> 'ret
  val ( ^ ) : 't. ('t, 't) ty -> Nat.ty -> ('t, 't) ty
end = struct
  open Pervasives

  type ('a, 'b) ty = 'a -> 'b

  let identity x = x
  let const x _ = x
  let false' _ y = y
  let flip f x y = f y x
  let ( -*> ) f g x = g (f x)

  let repeat ~n f =
    let rec inner ~n f acc =
      match n with
      | O -> acc
      | S n' -> inner ~n:n' f (acc -*> f)
    in
    inner ~n f identity
  ;;

  let rec fix f x = f (fix f) x
  let contract f x = f x x
  let tautology _ = true
  let antilogy _ = false
  let negate f x = not (f x)
  let negate_binary f left right = not (f left right)
  let map contra co f = contra -*> f -*> co
  let map_left contra f = map contra identity f
  let map_right co f = map identity co f
  let ( |> ) x f = f x
  let ( @@ ) f x = f x
  let ( ^ ) f n = repeat ~n f
end

module Interface = struct
  open Pervasives

  module type COMPARABLE = sig
    type ty

    val compare : ty -> ty -> compare
  end

  module type COMPARABLE_POLY1 = sig
    type 'first ty

    val compare
      : 'first.
      ('first -> 'first -> compare)
      -> 'first ty
      -> 'first ty
      -> compare
  end

  module type COMPARABLE_POLY2 = sig
    type ('first, 'second) ty

    val compare
      : 'first 'second.
      ('first -> 'first -> compare)
      -> ('second -> 'second -> compare)
      -> ('first, 'second) ty
      -> ('first, 'second) ty
      -> compare
  end

  module type COMPARABLE_POLY3 = sig
    type ('first, 'second, 'third) ty

    val compare
      : 'first 'second 'third.
      ('first -> 'first -> compare)
      -> ('second -> 'second -> compare)
      -> ('third -> 'third -> compare)
      -> ('first, 'second, 'third) ty
      -> ('first, 'second, 'third) ty
      -> compare
  end

  module type SERIALIZE = sig
    type ty

    val serialize : ty -> string
  end

  module type SERIALIZE_POLY1 = sig
    type 'first ty

    val serialize
      : 'first.
      ('first -> string) -> 'first ty -> string
  end

  module type SERIALIZE_POLY2 = sig
    type ('first, 'second) ty

    val serialize
      : 'first 'second.
      ('first -> string)
      -> ('second -> string)
      -> ('first, 'second) ty
      -> string
  end

  module type SERIALIZE_POLY3 = sig
    type ('first, 'second, 'third) ty

    val serialize
      : 'first 'second 'third.
      ('first -> string)
      -> ('second -> string)
      -> ('third -> string)
      -> ('first, 'second, 'third) ty
      -> string
  end

  module type DESERIALIZE = sig
    type ty
    type deserialization_error

    val deserialize
      :  string
      -> (ty, deserialization_error) result
  end

  module type DESERIALIZE_POLY1 = sig
    type 'first ty
    type deserialization_error

    val deserialize
      : 'first.
      (string -> ('first, deserialization_error) result)
      -> string
      -> ('first ty, deserialization_error) result
  end

  module type DESERIALIZE_POLY2 = sig
    type ('first, 'second) ty
    type deserialization_error

    val deserialize
      : 'first 'second.
      (string -> ('first, deserialization_error) result)
      -> (string -> ('second, deserialization_error) result)
      -> string
      -> (('first, 'second) ty, deserialization_error) result
  end

  module type DESERIALIZE_POLY3 = sig
    type ('first, 'second, 'third) ty
    type deserialization_error

    val deserialize
      : 'first 'second 'third.
      (string -> ('first, deserialization_error) result)
      -> (string -> ('second, deserialization_error) result)
      -> (string -> ('third, deserialization_error) result)
      -> string
      -> ( ('first, 'second, 'third) ty
           , deserialization_error )
           result
  end
end

include Pervasives
