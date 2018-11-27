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
***s/p pdfdictt -returns a meaningfull name for RPN codes used 
*                in field types
*
      subroutine pdfdictt(typenam,typedict,dictlen)
#include "impnone.cdk"
      integer dictlen
      character*16 typenam,typedict(dictlen)
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     returns a meaningfull name for RPN codes used in field types
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     typenam    O     C    S   FST code used for field type
*     typedict   I     C    A   Dictionary of field types
*     dictlen    I     I    S   length of dictionary
*
*IMPLICIT     
*
*MODULES 
*
**


      integer i
      logical found

      if (dictlen.eq.0) then
         typenam(4:16)='UNKNOWN'
         return
      endif

      found = .false.
      i = 1
 100  if (typedict(i)(1:1).eq.typenam(1:1)) then
         found = .true.
         typenam= typedict(i)
      endif
      
      i = i + 1
      if (.not.found.and.i.lt.dictlen) goto 100
      
      if (.not.found) then
         typenam(4:16)='UNKNOWN'
      endif
      
      return
      end
