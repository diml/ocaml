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

let noloc txt = { txt; loc = Location.none }

let ident_of_variable v =
  let cu = Variable.get_compilation_unit v in
  let name = Variable.unique_name v in
  let lid =
    if cu = Compilation_unit.get_current_exn () then
      Lident name
    else
      Ldot (Lident cu, name)
  in
  noloc lid

let evar v = ident_of_variable v |> Ext.ident
let pvar v = ident_of_variable v |> Pat.ident

let eint n = Exp.constant (Const_int n)
let pint n = Pat.constant (Const_int n)

let estring s = Exp.constant (Const_string (s, None))
let pstring s = Pat.constant (Const_string (s, None))

let efloat s = Exp.constant (Const_float s)

let eid s = Exp.ident (noloc (Longident.parse s))

let eapply e l = Exp.apply e (List.map (fun x -> (Nolabel, x)) l)

let static_exception_id se =
  Longident.Lident
    ("SE" ^ Static_exception.to_int se)
  |> noloc

let read_var env v =
  match Variable.Map.find v env with
  | exception Not_found -> evar v
  | Immutable -> evar v
  | Mutable -> eapply (eid "!") [evar v]

let addivar env v = Variable.Map.add v Immutable env

let extract_constant : Flambda.constant -> expression = function
  | Const_base c -> Exp.constant c
  | Const_pointer n ->
    Exp.extension (noloc "ptr",
                   Pstr [ Str.eval (eint n) ])
  | Const_float_array l -> Exp.array (List.map efloat l)
  | Const_immstring s -> estring s
  | Const_float f -> efloat (string_of_float f)

let extract_primitive : Lambda.primitive -> expression = function
  | Pidentity -> eid "identity"
  | Pignore -> eid "ignore"
  | Prevapply _ -> eid "revapply"
  | Pdirapply _ -> eid "dirapply"
  | Ploc lk ->
    (match lk with
     | Loc_FILE -> eid "Loc_FILE"
     | Loc_LINE -> eid "Loc_LINE"
     | Loc_MODULE -> eid "Loc_MODULE"
     | Loc_LOC -> eid "Loc_LOC"
     | Loc_POS -> eid "Loc_POS")
  | Pgetglobal id -> eapply (eid "getglobal") [ eid (Ident.unique_name id) ]
  | Psetglobal id -> eapply (eid "setglobal") [ eid (Ident.unique_name id) ]
  | Pgetglobalfield (id, n) -> eapply (eid "getglobal") [ eid (Ident.unique_name id)
                                                        ; eint n ]
  | Psetglobalfield (Exported, n) ->
    eapply (eid "setglobalfield")
      [ Exp.construct (eid "Exported") None
      ; eid (Ident.unique_name id) ]
  | Psetglobalfield (Not_exported, n) ->
    eapply (eid "setglobalfield")
      [ Exp.construct (eid "Not_exported") None
      ; eid (Ident.unique_name id) ]
  | Pmakeblock (n, Immutable) ->
    eapply (eid "makeblock")
      [ eint n; Exp.construct (eid "Immutable") None ]
  | Pmakeblock (n, Mutable) ->
    eapply (eid "makeblock")
      [ eint n; Exp.construct (eid "Mutable") None ]
  | Pfield n -> eapply (eid "field") [ eint n ]
  | Psetfield (n, b) ->
    eapply (eid "makeblock")
      [ eint n; Exp.construct (eid (string_of_bool b) None ]
  | Pfloatfield n -> eapply (eid "floatfield") [ eint n ]
  | Psetfloatfield n -> eapply (eid "setfloatfield") [ eint n ]
  | _ -> Exp.extension (noloc "todo", Pstr [ Str.eval (eint n) ])


let rec extract env (f : Flambda.t) =
  match f with
  | Var v -> evar v
  | Let (let_kind, v, named, f) ->
    Ext.let_ Nonrecursive
      [ Vb.mk (pvar v) (extract_named env named) ]
      (let e = extract (Variable.Map.add v let_kind env) f in
       match let_kind with
       | Immutable -> e
       | Multiple -> eapply (eid "ref") [e])
  | Let_rec (bindings, f) ->
    let env =
      List.fold_left (fun env (v, _) -> addivar env v)
        env bindings
    in
    Ext.let_ Recursive
      (List.map
         (fun (v, named) ->
            (pvar v, extract_named env named))
         bindings)
  | Apply { func; args; (* CR jdimino: deal with [kind] *) _ } ->
    eapply (read_var env func) (List.map (read_var env) args)
  | Send _ -> Ext.extension (noloc "send", PStr [])
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
              | _  -> Some (Exp.tuple (List.map pvar args))))
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
  | Symbol s ->
    eapply (eid "get_symbol")
      [ estring (Symbol.compilation_unit s)
      ; estring (Symbol.linkage_name s)
      ]
  | Const n -> extract_constant n
  | Set_of_closures { function_decls; _ } ->
    let env =
      Variable.Map.fold (fun v _ env -> addivar env v)
        function_decls.funs env
    in
    Exp.record
      (Variable.Map.fold (fun v fd acc ->
         let env = List.fold_left addivar env fd.params in
         let body = extract env fd.body in
         let func =
           List.fold_right
             (fun v acc -> Exp.fun_ Nolabel None (pvar v) acc)
             fd.params body
         in
         (pvar v, func) :: acc)
         function_decls.funs [])
      None
  | Project_closure { set_of_closures; closure_id } ->
    Exp.field (evar env set_of_closures)
      (Closure_id.unwrap closure_id |> ident_of_variable)
  | Move_within_set_of_closures _ ->
    Ext.extension (noloc "move_within_set_of_closures", PStr [])
  | Project_var { closure; closure_id; var } ->
    Exp.field
      (Exp.field (evar env set_of_closures)
         (Closure_id.unwrap closure_id |> ident_of_variable))
      (Var_within_closure.unwrap var |> ident_of_variable)
  | Prim (prim, args, _) ->
    eapply (extract_primitive prim)
      (List.map (read_var env) args)
  | Expr f -> extract env f

let extract f = extract Variable.Map.empty f
