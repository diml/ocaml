(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "-40"]

type t =
  { for_type : Ident.t
  }

let for_type t = t.for_type

let make (v : Types.value_description) =
  if not (Builtin_attributes.has_toplevel_printer v.val_attributes) then
    None
  else begin
    (* Extract the type constructor this printer is for *)
    let rec extract_id ty =
      match (Btype.repr ty).desc with
      | Tarrow (_, ty1, ty2, _) ->
          let ty2_is_unit =
            match (Btype.repr ty2).desc with
            | Tconstr (p, [], _) -> Path.same p Predef.path_unit
            | _ -> false
          in
          if not ty2_is_unit then
            extract_id ty2
          else begin
            match (Btype.repr ty1).desc with
            | Tconstr (Pident id, _, _) -> Some id
            | _ -> None
          end
      | _ -> None
    in
    match extract_id v.val_type with
    | None -> None
    | Some id -> Some { for_type = id }
  end
