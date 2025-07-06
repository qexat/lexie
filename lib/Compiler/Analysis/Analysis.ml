open Batteries
open Custom
open Clinic
open Common
open AIL
module Context = Quickmap.Make (Name) (Type)

let nat = Name.of_string_exn "Nat"
let unit = Name.of_string_exn "Unit"

let fetch_term
  :  doctor:Doctor.t
  -> context:Context.t
  -> Name.t
  -> Type.t option
  =
  fun ~doctor ~context name ->
  Context.get name context
  |> Option.on_none (fun () ->
    Doctor.add_error (Diagnosis.Name_not_found name) doctor)
;;

let infer_sort_of_sort : Sort.t -> Sort.t option =
  fun _ -> Some Sort.Type
;;

let infer_type_of_primitive =
  fun prim ->
  (match (prim : Primitive.t) with
   | Nat _ -> nat
   | Unit -> unit)
  |> Term.var
  |> Type.term
  |> Option.some
;;

let infer_type_of_parameter : Parameter.t -> Type.t option =
  fun parameter ->
  match parameter with
  | Named (_, ty) -> Some ty
;;

let rec infer_type_of_type
  :  doctor:Doctor.t
  -> context:Context.t
  -> Type.t
  -> Type.t option
  =
  fun ~doctor ~context ty ->
  match ty with
  | Arrow _ -> Some (Sort Type)
  | Sort sort ->
    let+ sort_sort = infer_sort_of_sort sort in
    Some (Type.Sort sort_sort)
  | Term term -> infer_type_of_term ~doctor ~context term

and infer_type_of_term
  :  doctor:Doctor.t
  -> context:Context.t
  -> Term.t
  -> Type.t option
  =
  fun ~doctor ~context term ->
  match term with
  | App (func, arg) -> try_apply ~doctor ~context func arg
  | Fun ((Named (name, ty) as param), ret) ->
    let+ param_type = infer_type_of_parameter param in
    let+ ret_type =
      infer_type_of_term
        ~doctor
        ~context:(Context.add name ty context)
        ret
    in
    Some (Type.Arrow (Named (name, param_type), ret_type))
  | Hole ->
    Doctor.add_warning Diagnosis.Hole_found doctor;
    Some (Term Hole)
  | Primitive prim -> infer_type_of_primitive prim
  | Var name -> fetch_term ~doctor ~context name

and check_type
  : doctor:Doctor.t -> expected:Type.t -> Type.t -> bool
  =
  fun ~doctor ~expected found ->
  match ((expected, found) : Type.t * Type.t) with
  | ( Arrow (Named (_, e_type), e_ret)
    , Arrow (Named (_, f_type), f_ret) ) ->
    check_type ~doctor ~expected:e_type f_type
    && check_type ~doctor ~expected:e_ret f_ret
  | Sort e_sort, Sort f_sort ->
    check_sort ~expected:e_sort f_sort
  | Term Hole, _ -> true
  | _, Term Hole ->
    Doctor.add_info (Diagnosis.Expected_type expected) doctor;
    true
  | Term e_term, Term f_term ->
    check_term ~doctor ~expected:e_term f_term
  | _, _ -> false

and check_sort : expected:Sort.t -> Sort.t -> bool =
  fun ~expected found ->
  match ((expected, found) : Sort.t * Sort.t) with
  | Prop, Prop -> true
  | Type, Type -> true
  | _, _ -> false

and check_term
  : doctor:Doctor.t -> expected:Term.t -> Term.t -> bool
  =
  fun ~doctor ~expected found ->
  match ((expected, found) : Term.t * Term.t) with
  | App (e_func, e_arg), App (f_func, f_arg) ->
    check_term ~doctor ~expected:e_func f_func
    && check_term ~doctor ~expected:e_arg f_arg
  | Fun (e_param, e_ret), Fun (f_param, f_ret) ->
    check_parameter ~doctor ~expected:e_param f_param
    && check_term ~doctor ~expected:e_ret f_ret
  | Hole, _ -> true
  | _, Hole ->
    Doctor.add_info
      (Diagnosis.Expected_type (Term expected))
      doctor;
    true
  | Primitive e_prim, Primitive f_prim ->
    Primitive.equal e_prim f_prim
  | Var e_name, Var f_name -> Name.equal e_name f_name
  | _, _ -> false

and check_parameter
  :  doctor:Doctor.t
  -> expected:Parameter.t
  -> Parameter.t
  -> bool
  =
  fun ~doctor ~expected found ->
  match expected, found with
  | Named (_, e_type), Named (_, f_type) ->
    check_type ~doctor ~expected:e_type f_type

and try_apply
  :  doctor:Doctor.t
  -> context:Context.t
  -> Term.t
  -> Term.t
  -> Type.t option
  =
  fun ~doctor ~context func arg ->
  let+ func_type = infer_type_of_term ~doctor ~context func in
  let+ arg_type = infer_type_of_term ~doctor ~context arg in
  match (func_type : Type.t) with
  | Arrow (Named (name, ty), ret) ->
    (match check_type ~doctor ~expected:ty arg_type with
     | false ->
       Doctor.add_error
         (Diagnosis.Argument_type_mismatch
            { expected = ty; found = arg_type })
         doctor;
       None
     | true ->
       let right =
         propagate_parameter
           (Parameter.Named (name, Type.term arg))
           ret
       in
       Some right)
  | _ ->
    Doctor.add_error
      (Diagnosis.Non_functional_application
         { term = func; ty = func_type })
      doctor;
    None

and propagate_parameter : Parameter.t -> Type.t -> Type.t =
  fun (Named (param_name, param_type)) rest ->
  let propagate =
    propagate_parameter (Named (param_name, param_type))
  in
  match rest with
  | Arrow (Named (name, ty), ret) ->
    Type.arrow (Named (name, propagate ty)) (propagate ret)
  | Sort _ -> rest
  | Term term ->
    (match term with
     | Fun (Named (param_name', param_type'), ret') ->
       Type.term
         (Term.lambda
            (Named (param_name', propagate param_type'))
            ret')
     | Var name when Name.equal name param_name -> param_type
     | _ -> Type.term term)
;;

let compare_with_annotation
  :  doctor:Doctor.t
  -> annotation:Type.t option
  -> Type.t
  -> Type.t option
  =
  fun ~doctor ~annotation found_type ->
  match annotation with
  | None -> Some found_type
  | Some ty ->
    (match check_type ~doctor ~expected:ty found_type with
     | false ->
       Doctor.add_warning
         (Diagnosis.Annotation_type_mismatch
            { expected = ty; found = found_type })
         doctor;
       Some found_type
     | true -> Some ty)
;;

let check_statement
  :  doctor:Doctor.t
  -> context:Context.t
  -> Statement.t
  -> Context.t option
  =
  fun ~doctor ~context statement ->
  match (statement : Statement.t) with
  | Let (name, annotation, body) ->
    let+ body_type = infer_type_of_term ~doctor ~context body in
    let+ assigned_type =
      compare_with_annotation ~doctor ~annotation body_type
    in
    Some (Context.add name assigned_type context)
  | Print term ->
    let+ _ = infer_type_of_term ~doctor ~context term in
    Some context
;;

let rec check_program
  :  doctor:Doctor.t
  -> ?context:Context.t
  -> Program.t
  -> Context.t option
  =
  fun ~doctor ?(context = Context.empty) program ->
  match program with
  | [] -> Some context
  | first :: rest ->
    let+ context = check_statement ~doctor ~context first in
    check_program ~doctor ~context rest
;;

let intrinsics : Context.t =
  let _Nat = Name.of_string_exn "Nat" in
  let _Unit = Name.of_string_exn "Unit" in
  let _O = Name.of_string_exn "O" in
  let _S = Name.of_string_exn "S" in
  let n = Name.of_string_exn "n" in
  Context.empty
  |> Context.add _Nat (Type.sort Type)
  |> Context.add _Unit (Type.sort Type)
  |> Context.add _O (Type.term (Var _Nat))
  |> Context.add
       _S
       (Type.arrow
          (Named (n, Type.term (Var _Nat)))
          (Term (Var _Nat)))
;;
