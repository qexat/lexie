open Batteries
open Custom
open Clinic
open Common
open Tail
module Context = Quickmap.Make (Name) (Kind)

let nat = Name.of_string_exn "Nat"
let unit = Name.of_string_exn "Unit"

let fetch_term =
  fun ~doctor ~context name ->
  Context.get name context
  |> Option.on_none (fun () -> Doctor.add_error (Diagnosis.Name_not_found name) doctor)
;;

let infer_sort_of_sort = fun ~doctor:_ ~context:_ _ -> Some Sort.Type

let infer_kind_of_primitive =
  fun ~doctor:_ ~context:_ prim ->
  (match (prim : Primitive.t) with
   | Nat _ -> nat
   | Unit -> unit)
  |> Term.var
  |> Kind.term
  |> Option.some
;;

let infer_kind_of_parameter =
  fun ~doctor:_ ~context:_ parameter ->
  match parameter with
  | _, kind -> Some kind
;;

let rec infer_kind_of_kind =
  fun ~doctor ~context kind ->
  let open Lang in
  match kind with
  | Arrow ((name, kind), ret) -> infer_kind_of_arrow ~doctor ~context (name, kind) ret
  | Sort sort ->
    let+ sort_sort = infer_sort_of_sort ~doctor ~context sort in
    Some (Sort sort_sort)
  | Term term -> infer_kind_of_term ~doctor ~context term

and infer_kind_of_arrow =
  fun ~doctor ~context (name, kind) ret ->
  let+ param_kind = infer_kind_of_kind ~doctor ~context kind in
  let+ ret_kind = infer_kind_of_kind ~doctor ~context ret in
  Some (Lang.Arrow ((name, param_kind), ret_kind))

and infer_kind_of_term =
  fun ~doctor ~context term ->
  let open Lang in
  match term with
  | App (func, arg) -> try_apply ~doctor ~context func arg
  | Fun (((name, kind) as param), ret) ->
    let+ param_kind = infer_kind_of_parameter ~doctor ~context param in
    let+ ret_kind =
      infer_kind_of_term ~doctor ~context:(Context.add name kind context) ret
    in
    Some (Arrow ((name, param_kind), ret_kind))
  | Hole ->
    Doctor.add_warning Diagnosis.Hole_found doctor;
    Some (Term Hole)
  | Primitive prim -> infer_kind_of_primitive ~doctor ~context prim
  | Var name -> fetch_term ~doctor ~context name

and check_kind =
  fun ~doctor ~expected found ->
  match ((expected, found) : Kind.t * Kind.t) with
  | Arrow ((_, e_kind), e_ret), Arrow ((_, f_kind), f_ret) ->
    check_kind ~doctor ~expected:e_kind f_kind && check_kind ~doctor ~expected:e_ret f_ret
  | Sort e_sort, Sort f_sort -> check_sort ~doctor ~expected:e_sort f_sort
  | Term Hole, _ -> true
  | _, Term Hole ->
    Doctor.add_info (Diagnosis.Expected_type expected) doctor;
    true
  | Term e_term, Term f_term -> check_term ~doctor ~expected:e_term f_term
  | _, _ -> false

and check_sort =
  fun ~doctor:_ ~expected found ->
  match ((expected, found) : Sort.t * Sort.t) with
  | Prop, Prop -> true
  | Type, Type -> true
  | _, _ -> false

and check_term =
  fun ~doctor ~expected found ->
  match ((expected, found) : Term.t * Term.t) with
  | App (e_func, e_arg), App (f_func, f_arg) ->
    check_term ~doctor ~expected:e_func f_func && check_term ~doctor ~expected:e_arg f_arg
  | Fun (e_param, e_ret), Fun (f_param, f_ret) ->
    check_parameter ~doctor ~expected:e_param f_param
    && check_term ~doctor ~expected:e_ret f_ret
  | Hole, _ -> true
  | _, Hole ->
    Doctor.add_info (Diagnosis.Expected_type (Term expected)) doctor;
    true
  | Primitive e_prim, Primitive f_prim -> Primitive.equal e_prim f_prim
  | Var e_name, Var f_name -> Name.equal e_name f_name
  | _, _ -> false

and check_parameter =
  fun ~doctor ~expected found ->
  match expected, found with
  | (_, e_kind), (_, f_kind) -> check_kind ~doctor ~expected:e_kind f_kind

and try_apply =
  fun ~doctor ~context func arg ->
  let+ func_kind = infer_kind_of_term ~doctor ~context func in
  let+ arg_kind = infer_kind_of_term ~doctor ~context arg in
  match (func_kind : Kind.t) with
  | Arrow ((name, kind), ret) ->
    (match check_kind ~doctor ~expected:kind arg_kind with
     | false ->
       Doctor.add_error
         (Diagnosis.Argument_type_mismatch { expected = kind; found = arg_kind })
         doctor;
       None
     | true ->
       let right = propagate_parameter (name, Kind.term arg) ret in
       Some right)
  | _ ->
    Doctor.add_error
      (Diagnosis.Non_functional_application { term = func; kind = func_kind })
      doctor;
    None

and propagate_parameter =
  fun (param_name, param_kind) rest ->
  let propagate = propagate_parameter (param_name, param_kind) in
  match rest with
  | Arrow ((name, kind), ret) -> Kind.arrow (name, propagate kind) (propagate ret)
  | Sort _ -> rest
  | Term term ->
    (match term with
     | Fun ((param_name', param_kind'), ret') ->
       Kind.term (Term.lambda (param_name', propagate param_kind') ret')
     | Var name when Name.equal name param_name -> param_kind
     | _ -> Kind.term term)
;;

let compare_with_annotation =
  fun ~doctor ~annotation found_kind ->
  match annotation with
  | None -> Some found_kind
  | Some kind ->
    (match check_kind ~doctor ~expected:kind found_kind with
     | false ->
       Doctor.add_warning
         (Diagnosis.Annotation_type_mismatch { expected = kind; found = found_kind })
         doctor;
       Some found_kind
     | true -> Some kind)
;;

let check_statement =
  fun ~doctor ~context statement ->
  match (statement : Statement.t) with
  | Let (name, annotation, body) ->
    let+ body_kind = infer_kind_of_term ~doctor ~context body in
    let+ assigned_kind = compare_with_annotation ~doctor ~annotation body_kind in
    Some (Context.add name assigned_kind context)
  | Print term ->
    let+ _ = infer_kind_of_term ~doctor ~context term in
    Some context
;;

let rec check_program =
  fun ~doctor ~context program ->
  match program with
  | [] -> Some context
  | first :: rest ->
    let+ context = check_statement ~doctor ~context first in
    check_program ~doctor ~context rest
;;

let check =
  fun ~doctor ?context program ->
  let context = context or Context.empty in
  check_program ~doctor ~context program
;;

let intrinsics =
  let _Nat = Name.of_string_exn "Nat" in
  let _Unit = Name.of_string_exn "Unit" in
  let _O = Name.of_string_exn "O" in
  let _S = Name.of_string_exn "S" in
  let n = Name.of_string_exn "n" in
  Context.empty
  |> Context.add _Nat (Kind.sort Type)
  |> Context.add _Unit (Kind.sort Type)
  |> Context.add _O (Kind.term (Var _Nat))
  |> Context.add _S (Kind.arrow (n, Kind.term (Var _Nat)) (Term (Var _Nat)))
;;
