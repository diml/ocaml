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

let lid s = noloc (Longident.parse s)

let eid s = Exp.ident (lid s)
let tid s = Typ.constr (lid s) []

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

let array_kind (k : Lambda.array_kind) =
  let s =
    match k with
    | Pgenarray   -> "Pgenarray"
    | Paddrarray  -> "Paddrarray"
    | Pintarray   -> "Pintarray"
    | Pfloatarray -> "Pfloatarray"
  in
  Exp.construct (lid s) None

let ba_type (kind : Lambda.bigarray_kind) (layout : Lambda.bigarray_layout) =
  let caml_type, repr =
    match kind with
    | Pbigarray_unknown -> (Typ.any (), Typ.any ())
    | Pbigarray_float32 -> (tid "float", tid "float32_elt")
    | Pbigarray_float64 -> (tid "float", tid "float64_elt")
    | Pbigarray_sint8 -> (tid "int", tid "int8_signed_elt")
    | Pbigarray_uint8 -> (tid "int", tid "int8_unsigned_elt")
    | Pbigarray_sint16 -> (tid "int", tid "int8_signed_elt")
    | Pbigarray_uint16 -> (tid "int", tid "int8_unsigned_elt")
    | Pbigarray_int32 -> (tid "int", tid "int32_elt")
    | Pbigarray_int64 -> (tid "int", tid "int32_elt")
    | Pbigarray_caml_int -> (tid "int", tid "int_elt")
    | Pbigarray_native_int -> (tid "nativeint", tid "nativeint_elt")
    | Pbigarray_complex32 -> (tid "Complex.t", tid "complex32_elt")
    | Pbigarray_complex64 -> (tid "Complex.t", tid "complex64_elt")
  in
  let layout =
    match layout with
    | Pbigarray_unknown_layout -> Typ.any ()
    | Pbigarray_c_layout -> tid "c_layout"
    | Pbigarray_fortran_layout -> tid "fortran_layout"
  in
  Typ.constr (lid "Bigarray.t") [caml_type; repr; layout]

let ba_access func unsafe dim kind layout args =
  let func = if unsafe then "unsafe_" ^ func else func in
  let m =
    match dim with
    | 1 -> "Array1"
    | 2 -> "Array2"
    | 3 -> "Array3"
    | _ -> "Genarray"
  in
  let func = Printf.ksprintf eid "Bigarray.%s.%s" m func in
  let ty = ba_type kind layout in
  match args with
  | [] ->
    Exp.constraint_ func (Typ.arrow Nolabel ty (Typ.any ()))
  | x :: args ->
    eapply func (Exp.constraint_ x (ba_type kind layout) :: args)

let extract_primitive (p : Lambda.primitive) args =
  let (_ : string) = Format.flush_str_formatter () in
  Printlambda.primitive Format.str_formatter p;
  let s = Format.flush_str_formatter () in
  match p with
  | Ploc _
  | Pidentity | Pignore | Pnot | Prevapply _ | Pdirapply _
  | Psequand | Psequor | Pnegint | Paddint | Psubint
  | Pmulint | Pdivint | Pmodint | Pandint | Porint
  | Pxorint | Plslint | Plsrint | Pasrint
  | Pintcomp _
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp _
  | Plazyforce
  | Pccall _ | Praise _
  | Pisint | Pisout | Pbittest
  | Pbintofint _
  | Pintofbint _
  | Pcvtbint _
  | Pnegbint _
  | Paddbint _
  | Psubbint _
  | Pmulbint _
  | Pdivbint _
  | Pmodbint _
  | Pandbint _
  | Porbint _
  | Pxorbint _
  | Plslbint _
  | Plsrbint _
  | Pasrbint _
  | Pbintcomp _
  | Pbswap16 | Pbbswap _
  | Pint_as_pointer
    -> eapply (eid s) args

  | Pabsfloat -> eapply (eid "abs_float") args

  | Pstringlength -> eapply (eid "String.length"    ) args
  | Pstringrefu   -> eapply (eid "String.unsafe_get") args
  | Pstringsetu   -> eapply (eid "String.unsafe_set") args
  | Pstringrefs   -> eapply (eid "String.get"       ) args
  | Pstringsets   -> eapply (eid "String.set"       ) args

  | Parraylength k -> eapply (eid "Array.length"    ) (array_kind k :: args)
  | Pmakearray   k -> eapply (eid "Array.make"      ) (array_kind k :: args)
  | Parrayrefu   k -> eapply (eid "Array.unsafe_get") (array_kind k :: args)
  | Parraysetu   k -> eapply (eid "Array.unsafe_set") (array_kind k :: args)
  | Parrayrefs   k -> eapply (eid "Array.get"       ) (array_kind k :: args)
  | Parraysets   k -> eapply (eid "Array.set"       ) (array_kind k :: args)

  | Pctconst c ->
    let const_name =
      match c with
      | Big_endian    -> "big_endian"
      | Word_size     -> "word_size"
      | Int_size      -> "int_size"
      | Max_wosize    -> "max_wosize"
      | Ostype_unix   -> "ostype_unix"
      | Ostype_win32  -> "ostype_win32"
      | Ostype_cygwin -> "ostype_cygwin"
    in
    eapply (eid ("Sys." ^ const_name)) args

  | Pmakeblock(tag, Immutable) -> eapply (eid "makeblock"  ) (eint tag :: args)
  | Pmakeblock(tag, Mutable  ) -> eapply (eid "makemutable") (eint tag :: args)

  | Pgetglobal id ->
    eapply (eid "getglobal") (estring (Ident.unique_name id) :: args)
  | Psetglobal id ->
    eapply (eid "setglobal") (estring (Ident.unique_name id) :: args)
  | Pgetglobalfield (id, i) ->
    eapply (eid "getglobalfield") (estring (Ident.unique_name id) :: eint i :: args)
  | Psetglobalfield (Exported, i) ->
    eapply (eid "setglobalfield_exported") (eint i :: args)
  | Psetglobalfield (Not_exported, i) ->
    eapply (eid "setglobalfield") (eint i :: args)

  | Pfield n -> eapply (eid "field") (eint n :: args)
  | Psetfield (n, ptr) ->
    let instr = if ptr then "setfield_ptr" else "setfield_imm" in
    eapply (eid instr) (eint n :: args)
  | Pfloatfield n -> eapply (eid "floatfield") (eint n :: args)
  | Psetfloatfield n -> eapply (eid "floatfield") (eint n :: args)

  | Pduprecord (rep, size) ->
    let rep =
      match rep with
      | Record_regular   -> Exp.construct (lid "Record_regular"  )  None
      | Record_float     -> Exp.construct (lid "Record_float"    )  None
      | Record_inlined n -> Exp.construct (lid "Record_inlined"  ) (Some (eint n))
      | Record_extension -> Exp.construct (lid "Record_extension")  None
    in
    eapply (eid "duprecord") (rep :: eint size :: args)

  | Poffsetint n ->
    eapply (eid "+" ) (match args with [] -> [eint n] | x :: args -> x :: eint n :: args)
  | Poffsetref n ->
    eapply (eid "+=") (match args with [] -> [eint n] | x :: args -> x :: eint n :: args)

  | Pbigarrayref (unsafe, n, kind, layout) ->
    ba_access "get" unsafe n kind layout args
  | Pbigarrayset (unsafe, n, kind, layout) ->
    ba_access "set" unsafe n kind layout args

  | Pbigarraydim n -> eapply (Printf.ksprintf eid "Bigarray.dim_%d" n) args

  | Pstring_load_16(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_get16" else "String.get16")) args
  | Pstring_load_32(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_get32" else "String.get32")) args
  | Pstring_load_64(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_get64" else "String.get64")) args
  | Pstring_set_16(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_set16" else "String.set16")) args
  | Pstring_set_32(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_set32" else "String.set32")) args
  | Pstring_set_64(unsafe) ->
    eapply (eid (if unsafe then "String.unsafe_set64" else "String.set64")) args
  | Pbigstring_load_16(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.Array1.unsafe_get16"
                 else "Bigarray.Array1.get16")) args
  | Pbigstring_load_32(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.Array1.unsafe_get32"
                 else "Bigarray.Array1.get32")) args
  | Pbigstring_load_64(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.Array1.unsafe_get64"
                 else "Bigarray.Array1.get64")) args
  | Pbigstring_set_16(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.Array1.unsafe_set16"
                 else "Bigarray.Array1.set16")) args
  | Pbigstring_set_32(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.Array1.unsafe_set32"
                 else "Bigarray.Array1.set32")) args
  | Pbigstring_set_64(unsafe) ->
    eapply (eid (if unsafe then "Bigarray.array1.unsafe_set64"
                 else "Bigarray.array1.set64")) args

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
  | Prim (prim, args, _) -> extract_primitive prim (List.map (read_var env) args)
  | Expr f -> extract env f

let extract f = extract Variable.Map.empty f
