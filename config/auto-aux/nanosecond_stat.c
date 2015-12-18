/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 Jeremie Dimino, Jane Street Group, LLC              */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define DONT_INCLUDE_CAML_CONFIG_H
#include "../../otherlibs/unix/nanosecond_stat.h"

int main() {
  struct stat buf;
  double a, m, c;
  a = (double)GET_NANOSECOND_STAT(buf, a);
  m = (double)GET_NANOSECOND_STAT(buf, m);
  c = (double)GET_NANOSECOND_STAT(buf, c);
  return 0;
}
