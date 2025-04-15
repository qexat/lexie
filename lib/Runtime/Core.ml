open Batteries
open Custom
module Environment = Quickmap.Make (Name) (Object)

let rec evaluate_term =
  fun ~env term ->
  let open Exception in
  match (term : Ail.Term.t) with
  | App (func, arg) -> apply_term ~env func arg
  | Fun (param, ret) -> Ok (Object.Fun (Ail.Parameter.name param, Object.Late ret))
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
  match (stmt : Ail.Statement.t) with
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
  fun ~painter exn ->
  let module Painter = (val painter : Painter.TYPE) in
  Printf.eprintf
    "%s %s\n"
    (Painter.paint_error "runtime error:")
    (Exception.show painter exn)
;;

let print_unreachable =
  fun ~painter unreachable ->
  let module Painter = (val painter : Painter.TYPE) in
  let tag = Painter.paint_error "internal error:" in
  let tag_ext = Painter.paint_info "unreachable:" in
  Printf.eprintf "%s %s %s\n" tag tag_ext (Unreachable.show painter unreachable)
;;

let evaluate =
  fun ?env ~painter program ->
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
