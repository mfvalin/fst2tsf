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
***s/p tsfdictt -returns a meaningfull name for RPN codes used 
*                for identification of meteorological variables
*
      subroutine tsfdictv(varname,dictvar,dictlen)
#include "impnone.cdk"
      integer dictlen
      character*48 varname,dictvar(dictlen)
      
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     returns a meaningfull name for RPN codes used 
*     identification of meteorological variables
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     varname    O     C    S   Meaningfull name associated 
*                               with the variable
*     dictvar    I     C    A   Dictionary of variables
*     dictlen    I     I    S   length of dictionary
*
*IMPLICIT     
*
*MODULES 
*
**
      character*48 empty
      character*2  nomvar
      integer i
      logical found
      
      data empty /'                                                '/

      nomvar = varname(1:2)
      varname = empty
      if (dictlen.eq.0) then
         varname = nomvar
         return
      endif
     
      found=.false.
      i = 1
 100  if (nomvar.eq.dictvar(i)(1:2)) then
         found = .true.
         varname = dictvar(i)
      endif
      
      i = i + 1
      if (.not.found.and.i.lt.dictlen) goto 100
      
      if (.not.found) varname=nomvar
      return
      end
      
