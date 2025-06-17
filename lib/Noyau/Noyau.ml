[@@@warning "-65"]

module rec Pervasives : sig
  (* types *)
  type unit = ()

  type bool =
    | False
    | True

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
  type unit = ()

  type bool =
    | False
    | True

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

and Bool : sig
  type ty = Pervasives.bool

  val not : ty -> ty
  val ( && ) : ty -> ty -> ty
  val ( || ) : ty -> ty -> ty
end = struct
  open Pervasives

  type ty = bool

  let not : ty -> ty = function
    | False -> True
    | True -> False
  ;;

  let ( && ) left right =
    match left, right with
    | True, True -> True
    | _, _ -> False
  ;;

  let ( || ) left right =
    match left, right with
    | False, False -> False
    | _, _ -> True
  ;;
end

and Compare : sig
  type ty = Pervasives.compare

  val ( = ) : ty -> ty -> Bool.ty
  val is_either : left:ty -> right:ty -> ty -> Bool.ty
  val to_int : ty -> int
  val of_int : int -> ty
end = struct
  open Pervasives

  type ty = compare

  let ( = ) left right =
    match left, right with
    | Equal, Equal | Greater, Greater | Less, Less -> True
    | _, _ -> False
  ;;

  let is_either ~left ~right compare =
    compare = left || compare = right
  ;;

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
  type 't ty = 't Pervasives.maybe

  val ( or ) : 't. 't ty -> 't -> 't
end = struct
  open Pervasives

  type 'item ty = 'item maybe

  let ( or ) maybe fallback =
    match maybe with
    | Nothing -> fallback
    | Some value -> value
  ;;
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
  let tautology _ = True
  let antilogy _ = False
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
