(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of primitive functions *)

open Misc
open Parsetree

type unbox =
  | Do_not_unbox
  | Unbox_float
  | Unbox_int32
  | Unbox_int64
  | Unbox_nativeint

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_unbox_args: unbox list;
    prim_native_unbox_res : unbox }

type error =
  | Unbox_attribute_has_payload
  | Conflicting_unbox_attributes
  | Float_with_unbox_attribute

exception Error of Location.t * error

let is_unbox = function
  | Do_not_unbox -> false
  | Unbox_float
  | Unbox_int32
  | Unbox_int64
  | Unbox_nativeint -> true

let rec make_unbox_args arity x =
  if arity = 0 then
    []
  else
    x :: make_unbox_args (arity - 1) x

let simple ~name ~arity ~alloc =
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = alloc;
   prim_native_name = "";
   prim_native_unbox_args = make_unbox_args arity Do_not_unbox;
   prim_native_unbox_res = Do_not_unbox}

let core_type_has_attribute name ty =
  match
    List.find (fun (n, payload) -> n.Location.txt = name) ty.ptyp_attributes
  with
  | exception Not_found -> false
  | (_, PStr []) -> true
  | (n, _) -> raise (Error (n.Location.loc, Unbox_attribute_has_payload))

let unbox_of_core_type ty =
  match
    core_type_has_attribute "unbox_float" ty,
    core_type_has_attribute "unbox_int32" ty,
    core_type_has_attribute "unbox_int64" ty,
    core_type_has_attribute "unbox_nativeint" ty
  with
  | false, false, false, false -> Do_not_unbox
  | true , false, false, false -> Unbox_float
  | false, true , false, false -> Unbox_int32
  | false, false, true , false -> Unbox_int64
  | false, false, false, true  -> Unbox_nativeint
  | _ -> raise (Error (ty.ptyp_loc, Conflicting_unbox_attributes))

let rec unbox_of_arrow_type ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (_, a, b) ->
      let unbox_args, unbox_res = unbox_of_arrow_type b in
      (unbox_of_core_type a :: unbox_args, unbox_res)
  | _ ->
      ([], unbox_of_core_type ty)

let parse_declaration valdecl =
  let unbox_args, unbox_res = unbox_of_arrow_type valdecl.pval_type in
  let arity = List.length unbox_args in
  let name, native_name, noalloc, float =
    match valdecl.pval_prim with
    | name :: "noalloc" :: name2 :: "float" :: _ -> (name, name2, true, true)
    | name :: "noalloc" :: name2 :: _ -> (name, name2, true, false)
    | name :: name2 :: "float" :: _ -> (name, name2, false, true)
    | name :: "noalloc" :: _ -> (name, "", true, false)
    | name :: name2 :: _ -> (name, name2, false, false)
    | name :: _ -> (name, "", false, false)
    | [] ->
        fatal_error "Primitive.parse_declaration"
  in
  if float && (List.exists is_unbox unbox_args || is_unbox unbox_res) then
    raise (Error (valdecl.pval_loc, Float_with_unbox_attribute));
  let unbox_args, unbox_res =
    if float then
      (make_unbox_args arity Unbox_float, Unbox_float)
    else
      (unbox_args, unbox_res)
  in
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = not noalloc;
   prim_native_name = native_name;
   prim_native_unbox_args = unbox_args;
   prim_native_unbox_res = unbox_res}

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list =
    let is_unbox_float x = x = Unbox_float in
    if List.for_all is_unbox_float p.prim_native_unbox_args &&
       is_unbox_float p.prim_native_unbox_res then
      "float" :: list
    else
      list
  in
  List.rev list

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name

let report_error ppf err =
  let open Format in
  match err with
  | Unbox_attribute_has_payload ->
      fprintf ppf "Wrong attribute format: \
                   payload not allowed in [@unbox_... ] attributes"
  | Conflicting_unbox_attributes ->
      fprintf ppf "Conflicting [@unbox_... ] attributes"
  | Float_with_unbox_attribute ->
      fprintf ppf "Cannot use \"float\" in conjunction with [@unbox_... ]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
