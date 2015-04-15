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

let make
      ~name
      ~alloc
      ~native_name
      ~native_unbox_args
      ~native_unbox_res
  =
  let arity = List.length native_unbox_args in
  { prim_name = name;
    prim_arity = arity;
    prim_alloc = alloc;
    prim_native_name = native_name;
    prim_native_unbox_args = native_unbox_args;
    prim_native_unbox_res = native_unbox_res }

let rec make_unbox_list arity x =
  if arity = 0 then
    []
  else
    x :: make_unbox_list (arity - 1) x

let all_float_unboxed arity = make_unbox_list arity Unbox_float

let make_byte ~name ~arity ~alloc =
  { prim_name = name;
    prim_arity = arity;
    prim_alloc = alloc;
    prim_native_name = "";
    prim_native_unbox_args = make_unbox_list arity Do_not_unbox;
    prim_native_unbox_res = Do_not_unbox }

let has name ty =
  match List.find (fun (n, payload) -> n.Location.txt = name) ty.ptyp_attributes with
  | exception Not_found -> false
  | (_, PStr []) -> true
  | (n, _) -> Location.raise_errorf ~loc:n.Location.loc "payload not allowed here"

let unbox_of_core_type ty =
  match
    has "unbox_float" ty,
    has "unbox_int32" ty,
    has "unbox_int64" ty,
    has "unbox_nativeint" ty
  with
  | false, false, false, false -> Do_not_unbox
  | true , false, false, false -> Unbox_float
  | false, true , false, false -> Unbox_int32
  | false, false, true , false -> Unbox_int64
  | false, false, false, true  -> Unbox_nativeint
  | _ ->
    Location.raise_errorf ~loc:ty.ptyp_loc "conflicting [@@unbox_XX ] attributes"

let rec unbox_of_arrow_type ty =
  match ty.ptyp_desc with
  | Ptyp_arrow (_, a, b) ->
    let unbox_args, unbox_res = unbox_of_arrow_type b in
    (unbox_of_core_type a :: unbox_args, unbox_res)
  | _ ->
    ([], unbox_of_core_type ty)

let parse_declaration arity decl ty =
  let prim_native_unbox_args, prim_native_unbox_res =
    unbox_of_arrow_type ty
  in
  match decl with
  | name :: "noalloc" :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2;
       prim_native_unbox_args = all_float_unboxed arity;
       prim_native_unbox_res = Unbox_float}
  | name :: "noalloc" :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = name2;
       prim_native_unbox_args; prim_native_unbox_res}
  | name :: name2 :: "float" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2;
       prim_native_unbox_args = all_float_unboxed arity;
       prim_native_unbox_res = Unbox_float}
  | name :: "noalloc" :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = false;
       prim_native_name = "";
       prim_native_unbox_args; prim_native_unbox_res}
  | name :: name2 :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = name2;
       prim_native_unbox_args; prim_native_unbox_res}
  | name :: _ ->
      {prim_name = name; prim_arity = arity; prim_alloc = true;
       prim_native_name = "";
       prim_native_unbox_args; prim_native_unbox_res}
  | [] ->
      fatal_error "Primitive.parse_declaration"

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list =
    if List.for_all (function Unbox_float -> true | _ -> false) p.prim_native_unbox_args
       && p.prim_native_unbox_res = Unbox_float then
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
