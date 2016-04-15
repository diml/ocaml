(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2016 Jane Street Group LLC                               *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Lesser General Public License version 2.1, with the        *)
(*  special exception on linking described in the file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

external bits_of_int64 : int64 -> string = "test_bits_of_int64"

let () =
  let bits = bits_of_int64 0x0102030405060708L in
  let expected =
    if Sys.big_endian then
      "\x01\x02\x03\x04\x05\x06\x07\x08"
    else
      "\x08\x07\x06\x05\x04\x03\x02\x01"
  in
  if bits = expected then
    print_endline "OK"
  else
    Printf.printf "expected: %S, got: %S\n" expected bits

