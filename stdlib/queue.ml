(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Francois Pottier, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

exception Empty

(* OCaml currently does not allow the components of a sum type to be
   mutable. Yet, for optimal space efficiency, we must have cons cells
   whose [next] field is mutable. This leads us to define a type of
   cyclic lists, so as to eliminate the [Nil] case and the sum
   type. *)

type ('a, 'what) cell =
  | Nil  :                                                        (_ , [> `Nil  ]) cell
  | Cell : { content: 'a; mutable next: ('a, [ `Cell ]) cell } -> ('a, [> `Cell ]) cell

(* A queue is a reference to either nothing or some cell of a cyclic
   list. By convention, that cell is to be viewed as the last cell in
   the queue. The first cell in the queue is then found in constant
   time: it is the next cell in the cyclic list. The queue's length is
   also recorded, so as to make [length] a constant-time operation.

   The [tail] field should really be of type ['a cell option], but
   then it would be [None] when [length] is 0 and [Some] otherwise,
   leading to redundant memory allocation and accesses. We avoid this
   overhead by filling [tail] with a dummy value when [length] is 0.
   Of course, this requires bending the type system's arm slightly,
   because it does not have dependent sums. *)

type 'a t = {
    mutable length: int;
    mutable tail: ('a, [ `Nil | `Cell ]) cell
  }

let create () = {
  length = 0;
  tail = Nil
}

let clear q =
  q.length <- 0;
  q.tail <- Nil

let upcast (Cell _ as cell : (_, [ `Cell ]) cell) = cell

let add x q =
  match q.tail with
  | Nil ->
    let rec cell = Cell {
      content = x;
      next = cell
    } in
    q.length <- 1;
    q.tail <- upcast cell
  | Cell tail ->
    let head = tail.next in
    let cell = Cell {
      content = x;
      next = head
    } in
    q.length <- q.length + 1;
    tail.next <- cell;
    q.tail <- upcast cell

let push =
  add

let peek q =
  match q.tail with
  | Nil -> raise Empty
  | Cell { next = Cell { content } } -> content

let top =
  peek

let take q =
  match q.tail with
  | Nil -> raise Empty
  | Cell tail ->
    q.length <- q.length - 1;
    let (Cell head) = tail.next in
    if Cell head == q.tail then
      q.tail <- Nil
    else
      tail.next <- head.next;
    head.content

let pop =
  take

let copy q =
  match q.tail with
  | Nil -> create ()
  | Cell tail ->
    let rec tail' = Cell {
      content = tail.content;
      next = tail'
    } in

    let rec copy (Cell prev) cell =
      if cell != Cell tail
      then
        let (Cell cell) = cell in
        let res = Cell {
          content = cell.content;
          next = tail'
        } in prev.next <- res;
        copy res cell.next
    in

    copy tail' tail.next;
    {
      length = q.length;
      tail = upcast tail'
    }

let is_empty q =
  q.length = 0

let length q =
  q.length

let iter f q =
  match q.tail with
  | Nil -> ()
  | Cell tail ->
    let rec iter (Cell cell) =
      f cell.content;
      if Cell cell != Cell tail then
        iter cell.next
    in
    iter tail.next

let fold f accu q =
  match q.tail with
  | Nil -> accu
  | Cell tail ->
    let rec fold accu (Cell cell) =
      let accu = f accu cell.content in
      if Cell cell == Cell tail then
        accu
      else
        fold accu cell.next in
    fold accu tail.next

let transfer q1 q2 =
  match q1.tail with
  | Nil -> ()
  | Cell tail1 ->
    clear q1;
    begin match q2.tail with
    | Nil -> ()
    | Cell tail2 ->
      let head1 = tail1.next in
      let head2 = tail2.next in
      tail1.next <- head2;
      tail2.next <- head1
    end;
    q2.length <- q2.length + q1.length;
    q2.tail <- Cell tail1
