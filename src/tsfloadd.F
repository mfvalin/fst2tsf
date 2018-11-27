*/* Useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2018  Recherche en Prevision Numerique
* *
* * This is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
***s/p tsfloadd -load RPN data dictionary
*
      subroutine tsfloadd(varname,varunit,dictlen,dictmax,
     $     type,typelen,typemax)
#include "impnone.cdk"
      integer dictlen,dictmax,typelen,typemax
      character*48 varname(dictmax)
      character*16 varunit(dictmax)
      character*16 type(typemax)

*
*AUTHOR   Yves Chartier                      October 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     loads RPN dictionary of variables
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     varname    O     C    A   dictionary of variables
*     varunit    O     C    A   dictionary of units
*     dictlen    O     I    S   number of entries read
*     dictmax    I     I    S   number of entries allocated
*     type       O     C    A   dictionary of field types
*     typelen    O     I    S   number of entries read
*     typemax    I     I    S   number of entries allocated
*
*IMPLICIT     
*
*MODULES 
      external c_tsfloadd
*
**

#if !defined (NEC)
      call c_tsfloadd(varname,varunit,dictlen,dictmax,
     $     type,typelen,typemax)
#endif
      return
      end
