open Batteries.Operators
open Custom
open Common

module Sort : sig
  type t =
    | Prop
    | Type

  (** [show painter sort] produces a pretty-printable
    representation of the [sort] using the [painter]. *)
  val show : (module Custom.Painter.TYPE) -> t -> string
end = struct
  (* TODO: universes *)
  type t =
    | Prop
    | Type

  let show =
    fun (module Painter : Painter.TYPE) -> function
    | Prop -> Painter.paint_type "Prop"
    | Type -> Painter.paint_type "Type"
  ;;
end

module rec Kind : sig
  type t =
    | Arrow of Parameter.t * t
    | Sort of Sort.t
    | Term of Term.t

  (** [arrow param ret] produces an arrow kind where [param] and
    [ret] are on the left and right handside of the arrow
    respectively. *)
  val arrow : Parameter.t -> t -> t

  (** [sort s] produces a sort kind where [s] is the underlying
    sort. *)
  val sort : Sort.t -> t

  (** [term t] produces a term kind where [t] is the underlying
    term. *)
  val term : Term.t -> t

  (** [show painter kind] produces a pretty-printable
    representation of [kind]. *)
  val show : (module Painter.TYPE) -> t -> string
end = struct
  type t =
    | Arrow of Parameter.t * t
    | Sort of Sort.t
    | Term of Term.t

  let arrow = fun param ret -> Arrow (param, ret)
  let sort = fun sort -> Sort sort
  let term = fun term -> Term term

  let rec show =
    fun painter kind ->
    match kind with
    | Arrow (param, ret) ->
      Printf.sprintf "%s -> %s" (Parameter.show painter param) (show painter ret)
    | Sort sort -> Sort.show painter sort
    | Term term -> Term.show painter term
  ;;
end

and Term : sig
  open Batteries
  open Custom

  type t =
    | App of t * t
    | Fun of Parameter.t * t
    | Hole
    | Primitive of Primitive.t
    | Var of Name.t

  (** {2 Constructors} *)

  (** [app t1 t2] produces an application term where [t1] is the
    functional term and [t2] is the argument. *)
  val app : t -> t -> t

  (** [lambda param term] produces the function term where
    [param] is the parameter and [term] the body. Note that
    functions are curried. *)
  val lambda : Parameter.t -> t -> t

  (** [hole] is the hole term. *)
  val hole : t

  (** [primitive prim] produces a primitive term from a
    primitive value. *)
  val primitive : Primitive.t -> t

  (** [var name] produces a variable term from a [name]. *)
  val var : Name.t -> t

  (** {2 Shorthands} *)

  (** [app2 t1 t2 t3] produces an application term where [t1]
    is the functional term and [t2] and [t3] are the first
    and second arguments respectively. *)
  val app2 : t -> t -> t -> t

  (** [nat n] produces a primitive [Nat] term given a positive
    integer [n].*)
  val nat : Intp.t -> t

  (** [unit] is the primitive [Unit] term. *)
  val unit : t

  (** {2 Other} *)

  (** [to_syntactic_kind term] returns the syntactic kind of the
    given [term]. *)
  val to_syntactic_kind : t -> Syntactic_kind.t

  (** [show painter term] produces a pretty-printable
    representation of [term]. *)
  val show : (module Painter.TYPE) -> t -> string
end = struct
  open Common

  type t =
    | App of t * t
    | Fun of Parameter.t * t
    | Hole
    | Primitive of Primitive.t
    | Var of Name.t

  (* Constructors *)
  let app = fun func arg -> App (func, arg)
  let lambda = fun param ret -> Fun (param, ret)
  let hole = Hole
  let primitive = fun prim -> Primitive prim
  let var = fun name -> Var name

  (* Shorthands *)
  let app2 = fun func arg1 arg2 -> app (app func arg1) arg2
  let nat = fun n -> primitive (Primitive.nat n)
  let unit = primitive Primitive.unit

  (* Functions *)
  let to_syntactic_kind : t -> Syntactic_kind.t = function
    | App _ -> App
    | Fun _ -> Fun
    | Hole | Primitive _ | Var _ -> Atom
  ;;

  let rec uncurry_function =
    fun term ->
    match term with
    | Fun (param, ret) ->
      let rest, inner_ret = uncurry_function ret in
      param :: rest, inner_ret
    | _ -> [], term
  ;;

  let rec uncurry_application =
    fun func ->
    match func with
    | App (func', last_arg) ->
      let inner_func, args = uncurry_application func' in
      inner_func, args ++ [ last_arg ]
    | _ -> func, []
  ;;

  let rec show =
    fun painter term ->
    let module Painter = (val painter : Painter.TYPE) in
    match term with
    | App (func, arg) ->
      let func, args = uncurry_application func in
      Printf.sprintf
        "%s %s"
        (show_considering_precedence painter func ~parent:term)
        (args ++ [ arg ]
         |> List.map (show_considering_precedence painter ~parent:term)
         |> String.concat " ")
    | Fun (param, ret) ->
      let params, ret = uncurry_function ret in
      Printf.sprintf
        "%s %s -> %s"
        (Painter.paint_keyword "fun")
        (param :: params |> List.map (Parameter.show painter) |> String.concat " ")
        (show_considering_precedence painter ret ~parent:term)
    | Hole -> Painter.paint_hole "_"
    | Primitive prim -> Primitive.show (module Painter) prim
    | Var name -> Name.show (module Painter) name

  and show_considering_precedence =
    fun painter term ~parent ->
    let module Painter = (val painter : Painter.TYPE) in
    let repr = show (module Painter) term in
    if
      Syntactic_kind.binds_tighter
        (to_syntactic_kind parent)
        ~than:(to_syntactic_kind term)
    then "(" <> repr <> ")"
    else repr
  ;;
end

and Parameter : sig
  type nonrec t = Named of Name.t * Kind.t

  (** [name param] returns the name of the [param]eter. *)
  val name : t -> Name.t

  (** [kind param] returns the kind annotation of the [param]eter. *)
  val kind : t -> Kind.t

  (** [show painter param] produces a pretty-printable
    representation of the [param]eter. *)
  val show : (module Painter.TYPE) -> t -> string
end = struct
  type nonrec t = Named of Name.t * Kind.t

  let name = function
    | Named (name, _) -> name
  ;;

  let kind = function
    | Named (_, kind) -> kind
  ;;

  let show =
    fun painter parameter ->
    match parameter with
    | Named (name, kind) ->
      Printf.sprintf "(%s : %s)" (Name.show painter name) (Kind.show painter kind)
  ;;
end

module Statement : sig
  type t =
    | Let of Name.t * Kind.t option * Term.t
    | Print of Term.t

  (** [let' name ?annotation body] creates a [Let] statement given
    a binding's [name], an optional [annotation] and its [body]
    term. *)
  val let' : Name.t -> ?annotation:Kind.t -> Term.t -> t

  (** [print term] creates a [Print] statement given a [term]. *)
  val print : Term.t -> t

  (** [show painter statement] produces a pretty-printable
    representation of the [statement] using the [painter]. *)
  val show : (module Painter.TYPE) -> t -> string
end = struct
  type nonrec t =
    | Let of Name.t * Kind.t option * Term.t
    | Print of Term.t

  let let' = fun name ?annotation term -> Let (name, annotation, term)
  let print = fun term -> Print term

  let show =
    fun painter stmt ->
    let module Painter = (val painter : Painter.TYPE) in
    match stmt with
    | Let (name, annotation, body) ->
      let buffer = Buffer.create 64 in
      Buffer.add_string
        buffer
        (Printf.sprintf "%s %s" (Painter.paint_keyword "let") (Name.show painter name));
      (match annotation with
       | None -> ()
       | Some kind ->
         Buffer.add_string
           buffer
           (Printf.sprintf " %s %s" (Painter.paint_keyword ":") (Kind.show painter kind)));
      Buffer.add_string
        buffer
        (Printf.sprintf " %s %s" (Painter.paint_bold "=") (Term.show painter body));
      Buffer.contents buffer
    | Print term ->
      Printf.sprintf "%s %s" (Painter.paint_keyword "print") (Term.show painter term)
  ;;
end

module Program : sig
  type t = Statement.t list

  (** [show painter program] produces a pretty-printable
    representation of the [program] using the [painter]. *)
  val show : (module Painter.TYPE) -> t -> string
end = struct
  type t = Statement.t list

  let show =
    fun painter program ->
    program |> List.map (Statement.show painter) |> String.concat "\n"
  ;;
end
