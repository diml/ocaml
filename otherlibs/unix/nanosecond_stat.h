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

#ifndef __NANOSECOND_STAT_H
#define __NANOSECOND_STAT_H

/* <private> */
/* This file is used by the configure test program nanosecond_stat.c
   and stat.c in this directory */
#if !defined(DONT_INCLUDE_CAML_CONFIG_H)
/* </private> */
#include <caml/config.h>
/* <private> */
#endif
/* </private> */

/* GET_NANOSECOND_STAT(buffer, field) where "buffer" is a "struct stat"
   and "field" is one of:
   - 'm' for mtime
   - 'a' for atime
   - 'c' for ctime
*/
#if HAS_NANOSECOND_STAT == 1
#  define GET_NANOSECOND_STAT(buf, field) (buf).st_##field##tim.tv_nsec
#elif HAS_NANOSECOND_STAT == 2
#  define GET_NANOSECOND_STAT(buf, field) (buf).st_##field##timespec.tv_nsec
#elif HAS_NANOSECOND_STAT == 3
#  define GET_NANOSECOND_STAT(buf, field) (buf).st_##field##timensec
#else
#  define GET_NANOSECOND_STAT(buf, field) 0
#endif

#endif /* __NANOSECOND_STAT_H */
