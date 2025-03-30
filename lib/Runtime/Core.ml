open Batteries
open Custom
module Environment = Quickmap.Make (Name) (Object)

let rec evaluate_term =
  fun ~env term ->
  let open Exception in
  match (term : Tail.Term.t) with
  | App (func, arg) -> apply_term ~env func arg
  | Fun (param, ret) -> Ok (Object.Fun (Tail.Parameter.name param, Object.Late ret))
  | Hole -> Error (Either.Left { kind = Exception.Incomplete_program })
  | Primitive prim -> Ok (Object.Constant prim)
  | Var name ->
    (match Environment.get name env with
     | Some obj -> Ok obj
     | None -> Error (Either.Right (Unreachable.Undefined_name name)))

and apply_term =
  fun ~env func arg ->
  let* func_object = evaluate_term ~env func in
  let* arg_object = evaluate_term ~env arg in
  match func_object with
  | Fun (param, Late ret) ->
    let* ret' = evaluate_term ~env:(Environment.add param arg_object env) ret in
    Ok ret'
  | Fun (_, ret) -> Ok ret
  | _ -> Error (Either.Right Unreachable.Illegal_application)
;;

let evaluate_statement =
  fun ~env ~painter stmt ->
  match (stmt : Tail.Statement.t) with
  | Let (name, _, body) ->
    let* body_object = evaluate_term ~env body in
    Ok (Environment.add name body_object env)
  | Print term ->
    let* term_object = evaluate_term ~env term in
    Printf.printf "%s\n" (Object.show painter term_object);
    Ok env
;;

let rec evaluate_program =
  fun ~env ~painter program ->
  match program with
  | [] -> Ok env
  | first :: rest ->
    let* env = evaluate_statement ~env ~painter first in
    evaluate_program ~env ~painter rest
;;

let print_exception =
  fun ~painter:(module Painter : Painter.TYPE) exn ->
  let tag = Painter.paint_error "runtime error:" in
  match (exn : Exception.t).kind with
  | Incomplete_program ->
    Printf.eprintf "%s %s\n" tag "this program is incomplete (hole found)"
;;

let print_unreachable =
  fun ~painter:(module Painter : Painter.TYPE) unreachable ->
  let tag = Painter.paint_error "internal error:" in
  let tag_ext = Painter.paint_info "unreachable:" in
  (match (unreachable : Unreachable.t) with
   | Illegal_application -> "illegal application"
   | Undefined_name name ->
     Printf.sprintf "undefined name %s" (Name.show (module Painter) name))
  |> Printf.eprintf "%s %s %s\n" tag tag_ext
;;

let evaluate =
  fun ~painter ?env program ->
  let env = env or Environment.empty in
  match evaluate_program ~env ~painter program with
  | Ok _ -> 0
  | Error (Either.Left exn) ->
    print_exception ~painter exn;
    1
  | Error (Either.Right unreachable) ->
    print_unreachable ~painter unreachable;
    126
;;

let intrinsics =
  let _Nat = Name.of_string_exn "Nat" in
  let _O = Name.of_string_exn "O" in
  let _S = Name.of_string_exn "S" in
  let n = Name.of_string_exn "n" in
  Environment.empty
  |> Environment.add _Nat (Object.Late (Var _Nat))
  |> Environment.add _O (Object.Constant (Nat Intp.zero))
  |> Environment.add _S (Object.Fun (n, Late Hole))
;;
