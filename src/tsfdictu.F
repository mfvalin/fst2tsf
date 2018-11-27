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
***s/p tsfdictp -returns the units associated to a variable
*
      subroutine tsfdictu(units,nomvar,dictu,dictlen)
#include "impnone.cdk"
      integer dictlen
      character*2  nomvar
      character*16 units,dictu(dictlen)

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     returns the units associated to a variable
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     units      O     C    S   Units associated with the variable
*     nomvar     I     C    S   FST code of the variable
*     dictu      I     C    A   Dictionary of units
*     dictlen    I     I    S   length of dictionary
*
*IMPLICIT     
*
*MODULES 
*
**
      integer i
      logical found

      units = 'UNKNOWN          '
      if (dictlen.eq.0) return

      found = .false.
      i = 1
 100  if (dictu(i)(1:2).eq.nomvar) then
         found = .true.
         units(1:13)=dictu(i)(4:16)
      endif
      
      i = i + 1
      if (.not.found.and.i.lt.dictlen) goto 100
      
      return
      end
