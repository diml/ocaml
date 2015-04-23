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

type machtype_component =
    Addr
  | Int
  | Float

type machtype = machtype_component array

let typ_void = ([||] : machtype_component array)
let typ_addr = [|Addr|]
let typ_int = [|Int|]
let typ_float = [|Float|]

let size_component = function
    Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge

let negate_comparison = function
    Ceq -> Cne | Cne -> Ceq
  | Clt -> Cge | Cle -> Cgt
  | Cgt -> Cle | Cge -> Clt

let swap_comparison = function
    Ceq -> Ceq | Cne -> Cne
  | Clt -> Cgt | Cle -> Cge
  | Cgt -> Clt | Cge -> Cle

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word
  | Single
  | Double
  | Double_u
  | Int64

type operation =
    Capply of machtype * Debuginfo.t
  | Cextcall of string * machtype * bool * Debuginfo.t
  | Cload of memory_chunk
  | Calloc
  | Cstore of memory_chunk
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of comparison
  | Cadda | Csuba
  | Ccmpa of comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of comparison
  | Craise of Lambda.raise_kind * Debuginfo.t
  | Ccheckbound of Debuginfo.t

type expression =
    Cconst_int of int
  | Cconst_natint of nativeint
  | Cconst_float of float
  | Cconst_symbol of string
  | Cconst_pointer of int
  | Cconst_natpointer of nativeint
  | Cconst_blockheader of nativeint
  | Cvar of Ident.t
  | Clet of Ident.t * expression * expression
  | Cassign of Ident.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array
  | Cloop of expression
  | Ccatch of int * Ident.t list * expression * expression
  | Cexit of int * expression list
  | Ctrywith of expression * Ident.t * expression

type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression;
    fun_fast: bool;
    fun_dbg : Debuginfo.t; }

type data_item =
    Cdefine_symbol of string
  | Cdefine_label of int
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Clabel_address of int
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list
