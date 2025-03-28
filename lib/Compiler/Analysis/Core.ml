open Batteries
open Custom
open Common
module Context = Quickmap.Make (Name) (Kind)

let nat = Name.of_string_exn "Nat"
let unit = Name.of_string_exn "Unit"

let fetch_term =
  fun ~nurse ~context name ->
  Context.get name context
  |> Option.on_none (fun () -> Nurse.add_error (Diagnosis.Name_not_found name) nurse)
;;

let infer_sort_of_sort = fun ~nurse:_ ~context:_ _ -> Some Sort.Type

let infer_kind_of_primitive =
  fun ~nurse:_ ~context:_ prim ->
  (match (prim : Primitive.t) with
   | Nat _ -> nat
   | Unit -> unit)
  |> Term.var
  |> Kind.term
  |> Option.some
;;

let infer_kind_of_parameter =
  fun ~nurse:_ ~context:_ parameter ->
  match parameter with
  | _, kind -> Some kind
;;

let rec infer_kind_of_kind =
  fun ~nurse ~context kind ->
  let open Lang in
  match kind with
  | Arrow ((name, kind), ret) -> infer_kind_of_arrow ~nurse ~context (name, kind) ret
  | Sort sort ->
    let+ sort_sort = infer_sort_of_sort ~nurse ~context sort in
    Some (Sort sort_sort)
  | Term term -> infer_kind_of_term ~nurse ~context term

and infer_kind_of_arrow =
  fun ~nurse ~context (name, kind) ret ->
  let+ param_kind = infer_kind_of_kind ~nurse ~context kind in
  let+ ret_kind = infer_kind_of_kind ~nurse ~context ret in
  Some (Lang.Arrow ((name, param_kind), ret_kind))

and infer_kind_of_term =
  fun ~nurse ~context term ->
  let open Lang in
  match term with
  | App (func, arg) -> try_apply ~nurse ~context func arg
  | Fun (((name, kind) as param), ret) ->
    let+ param_kind = infer_kind_of_parameter ~nurse ~context param in
    let+ ret_kind =
      infer_kind_of_term ~nurse ~context:(Context.add name kind context) ret
    in
    Some (Arrow ((name, param_kind), ret_kind))
  | Hole ->
    Nurse.add_warning Diagnosis.Hole_found nurse;
    Some (Term Hole)
  | Primitive prim -> infer_kind_of_primitive ~nurse ~context prim
  | Var name -> fetch_term ~nurse ~context name

and check_kind =
  fun ~nurse ~expected found ->
  match ((expected, found) : Kind.t * Kind.t) with
  | Arrow ((_, e_kind), e_ret), Arrow ((_, f_kind), f_ret) ->
    check_kind ~nurse ~expected:e_kind f_kind && check_kind ~nurse ~expected:e_ret f_ret
  | Sort e_sort, Sort f_sort -> check_sort ~nurse ~expected:e_sort f_sort
  | Term Hole, _ -> true
  | _, Term Hole ->
    Nurse.add_info (Diagnosis.Expected_type expected) nurse;
    true
  | _, _ -> false

and check_sort =
  fun ~nurse:_ ~expected found ->
  match ((expected, found) : Sort.t * Sort.t) with
  | Prop, Prop -> true
  | Type, Type -> true
  | _, _ -> false

and check_term =
  fun ~nurse ~expected found ->
  match ((expected, found) : Term.t * Term.t) with
  | App (e_func, e_arg), App (f_func, f_arg) ->
    check_term ~nurse ~expected:e_func f_func && check_term ~nurse ~expected:e_arg f_arg
  | Fun (e_param, e_ret), Fun (f_param, f_ret) ->
    check_parameter ~nurse ~expected:e_param f_param
    && check_term ~nurse ~expected:e_ret f_ret
  | Hole, _ -> true
  | _, Hole ->
    Nurse.add_info (Diagnosis.Expected_type (Term expected)) nurse;
    true
  | Primitive e_prim, Primitive f_prim -> Primitive.equal e_prim f_prim
  | Var e_name, Var f_name -> Name.equal e_name f_name
  | _, _ -> false

and check_parameter =
  fun ~nurse ~expected found ->
  match expected, found with
  | (_, e_kind), (_, f_kind) -> check_kind ~nurse ~expected:e_kind f_kind

and try_apply =
  fun ~nurse ~context func arg ->
  let+ func_kind = infer_kind_of_term ~nurse ~context func in
  let+ arg_kind = infer_kind_of_term ~nurse ~context arg in
  match (func_kind : Kind.t) with
  | Arrow ((name, kind), ret) ->
    (match check_kind ~nurse ~expected:kind arg_kind with
     | false ->
       Nurse.add_error
         (Diagnosis.Argument_type_mismatch { expected = kind; found = arg_kind })
         nurse;
       None
     | true ->
       let right = propagate_parameter (name, Kind.term arg) ret in
       Some right)
  | _ ->
    Nurse.add_error
      (Diagnosis.Non_functional_application { term = func; kind = func_kind })
      nurse;
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
  fun ~nurse ~annotation found_kind ->
  match annotation with
  | None -> Some found_kind
  | Some kind ->
    (match check_kind ~nurse ~expected:kind found_kind with
     | false ->
       Nurse.add_warning
         (Diagnosis.Annotation_type_mismatch { expected = kind; found = found_kind })
         nurse;
       Some found_kind
     | true -> Some kind)
;;

let check_statement =
  fun ~nurse ~context statement ->
  match (statement : Statement.t) with
  | Let (name, annotation, body) ->
    let+ body_kind = infer_kind_of_term ~nurse ~context body in
    let+ assigned_kind = compare_with_annotation ~nurse ~annotation body_kind in
    Some (Context.add name assigned_kind context)
  | Print term ->
    let+ _ = infer_kind_of_term ~nurse ~context term in
    Some context
;;

let rec check_program =
  fun ~nurse ~context program ->
  match program with
  | [] -> Some context
  | first :: rest ->
    let+ context = check_statement ~nurse ~context first in
    check_program ~nurse ~context rest
;;

let check =
  fun ?nurse ?context program ->
  let nurse = nurse or Nurse.create () in
  let context = context or Context.empty in
  Nurse.set_diagnosis_renderer Diagnosis.render nurse;
  check_program ~nurse ~context program
;;
