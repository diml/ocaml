(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Ast_helper
open Parsetree
open Asttypes
open Longident

let noloc txt = { txt; loc = Location.none }

let ident_of_variable v =
  let cu = Variable.get_compilation_unit v in
  let name = Variable.unique_name v in
  let lid =
    if cu = Compilation_unit.get_current_exn () then
      Lident name
    else
      Ldot (Lident (Compilation_unit.get_persistent_ident cu |> Ident.name), name)
  in
  noloc lid

let evar v = Exp.ident (ident_of_variable v)
let pvar v = Pat.var (Variable.unique_name v |> noloc)

let eint n = Exp.constant (Const_int n)
let pint n = Pat.constant (Const_int n)

let estring s = Exp.constant (Const_string (s, None))
let pstring s = Pat.constant (Const_string (s, None))

let efloat s = Exp.constant (Const_float s)

let eid s = Exp.ident (noloc (Longident.parse s))

let eapply e l = Exp.apply e (List.map (fun x -> (Nolabel, x)) l)

let static_exception_id se =
  Longident.Lident
    ("SE" ^ string_of_int (Static_exception.to_int se))
  |> noloc

let read_var env v =
  match Variable.Map.find v env with
  | exception Not_found -> evar v
  | Flambda.Immutable -> evar v
  | Flambda.Mutable -> eapply (eid "!") [evar v]

let addivar env v = Variable.Map.add v Flambda.Immutable env

let extract_constant : Flambda.const -> expression = function
  | Const_base c -> Exp.constant c
  | Const_pointer n ->
    Exp.extension (noloc "ptr",
                   PStr [ Str.eval (eint n) ])
  | Const_float_array l -> Exp.array (List.map efloat l)
  | Const_immstring s -> estring s
  | Const_float f -> efloat (string_of_float f)

let string_of_primitive p =
  let (_ : string) = Format.flush_str_formatter () in
  Printlambda.primitive Format.str_formatter p;
  Format.flush_str_formatter ()

let rec extract env (f : Flambda.t) =
  match f with
  | Var v -> evar v
  | Let (let_kind, v, named, f) ->
    let e_named =
      let e_named = extract_named env named in
      match let_kind with
      | Immutable -> e_named
      | Mutable -> eapply (eid "ref") [e_named]
    in
    Exp.let_ Nonrecursive
      [ Vb.mk (pvar v) e_named ]
      (extract (Variable.Map.add v let_kind env) f)
  | Let_rec (bindings, f) ->
    let env =
      List.fold_left (fun env (v, _) -> addivar env v)
        env bindings
    in
    Exp.let_ Recursive
      (List.map
         (fun (v, named) ->
            Vb.mk (pvar v) (extract_named env named))
         bindings)
      (extract env f)
  | Apply { func; args; (* CR jdimino: deal with [kind] *) _ } ->
    eapply (read_var env func) (List.map (read_var env) args)
  | Send _ -> Exp.extension (noloc "send", PStr [])
  | Assign { being_assigned; new_value } ->
    eapply (eid ":=")
      [ read_var env being_assigned
      ; read_var env new_value
      ]
  | If_then_else (v, fthen, felse) ->
    Exp.ifthenelse (read_var env v)
      (extract env fthen)
      (Some (extract env felse))
  | Switch (v, { consts; blocks; failaction; _ }) ->
    let make_cases =
      List.map (fun (n, f) ->
        Exp.case (pint n) (extract env f))
    in
    let ev = read_var env v in
    let failcase =
      match failaction with
      | None -> []
      | Some f -> [ Exp.case (Pat.any ()) (extract env f) ]
    in
    begin match consts, blocks with
    | [], [] -> assert false
    | [], _ ->
      Exp.match_ (eapply (eid "Obj.tag") [ev])
        (make_cases blocks @ failcase)
    | _, [] ->
      Exp.match_ ev
        (make_cases consts @ failcase)
    | _ ->
      Exp.ifthenelse (eapply (eid "Obj.is_int") [ev])
        (Exp.match_ ev
           (make_cases consts @ failcase))
        (Some
           (Exp.match_ (eapply (eid "Obj.tag") [ev])
              (make_cases blocks @ failcase)))
    end
  | String_switch (v, cases, default) ->
    Exp.match_ (read_var env v)
      (List.map (fun (s, f) ->
         Exp.case
           (pstring s)
           (extract env f))
         cases
       @ match default with
       | None -> []
       | Some f -> [ Exp.case (Pat.any ()) (extract env f) ])
  | Static_raise (se, args) ->
    eapply (eid "raise")
      [ Exp.construct (static_exception_id se)
          (match args with
           | [] -> None
           | _  -> Some (Exp.tuple (List.map (extract env) args)))
      ]
  | Static_catch (se, args, body, catched) ->
    let e_catched =
      let env = List.fold_left addivar env args in
      extract env catched
    in
    Exp.try_ (extract env body)
      [ Exp.case
          (Pat.construct (static_exception_id se)
             (match args with
              | [] -> None
              | _  -> Some (Pat.tuple (List.map pvar args))))
          e_catched
      ]
  | Try_with (body, v, catched) ->
    Exp.try_ (extract env body)
      [ Exp.case
          (pvar v)
          (extract (addivar env v) catched)
      ]
  | While (cond, f) ->
    Exp.while_ (extract env cond) (extract env f)
  | For { bound_var; from_value; to_value; direction; body } ->
    Exp.for_ (pvar bound_var)
      (read_var env from_value)
      (read_var env to_value)
      direction
      (extract (addivar env bound_var) body)
  | Proved_unreachable ->
    Exp.assert_ (Exp.construct (noloc (Lident "false")) None)

and extract_named env (n : Flambda.named) =
  match n with
  | Symbol s ->
    eapply (eid "get_symbol")
      [ estring (Symbol.compilation_unit s |> Compilation_unit.get_persistent_ident |> Ident.name)
      ; estring (Symbol.label s |> Linkage_name.to_string)
      ]
  | Const n -> extract_constant n
  | Set_of_closures { function_decls; _ } ->
    let env =
      Variable.Map.fold (fun v _ env -> addivar env v)
        function_decls.funs env
    in
    Exp.record
      (Variable.Map.fold (fun v (fd : Flambda.function_declaration) acc ->
         let env = List.fold_left addivar env fd.params in
         let body = extract env fd.body in
         let func =
           List.fold_right
             (fun v acc -> Exp.fun_ Nolabel None (pvar v) acc)
             fd.params body
         in
         (ident_of_variable v, func) :: acc)
         function_decls.funs [])
      None
  | Project_closure { set_of_closures; closure_id } ->
    Exp.field (read_var env set_of_closures)
      (Closure_id.unwrap closure_id |> ident_of_variable)
  | Move_within_set_of_closures _ ->
    Exp.extension (noloc "move_within_set_of_closures", PStr [])
  | Project_var { closure; closure_id; var } ->
    Exp.field
      (Exp.field (read_var env closure)
         (Closure_id.unwrap closure_id |> ident_of_variable))
      (Var_within_closure.unwrap var |> ident_of_variable)
  | Prim (prim, args, _) ->
    let s = string_of_primitive prim in
    eapply (eid "prim")
      (estring s :: List.map (read_var env) args)
  | Expr f -> extract env f

let extract f = extract Variable.Map.empty f
