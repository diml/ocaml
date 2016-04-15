/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 Jeremie Dimino, Jane Street Europe                  */
/*                                                                     */
/*  Copyright 2016 Jane Street Group LLC                               */
/*                                                                     */
/*  All rights reserved.  This file is distributed under the terms of  */
/*  the GNU Lesser General Public License version 2.1, with the        */
/*  special exception on linking described in the file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>

CAMLprim value test_bits_of_int64(value n)
{
  char buf[9];
  *(int64_t*)buf = Int64_val(n);
  buf[8] = 0;
  return caml_copy_string(buf);
}
