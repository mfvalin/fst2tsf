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
***s/p pdfdictp -returns a meaningfull name for RPN codes used 
*                in geographical projections
*
      subroutine pdfdictp(projnam,projcode)
#include "impnone.cdk"
      character*16 projnam,projcode
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     returns a meaningfull name for FST codes used in geographical 
*     projections
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     projnam    O     C    S   FST projection code
*     projcode   I     C    S   PDF projection code + meaningfull name
*
*IMPLICIT     
*
*MODULES 
*
**
      
      integer dictlen
      parameter (dictlen=9)

      integer i
      logical found

      character*32 dictunit(dictlen)
      save dictunit

      data dictunit /'N (POL.ST.NORTH)',
     $     'S (POL.ST.SOUTH)',
     $     'G (GAUSSIAN)',
     $     'A (LATLON HEMIS.)',
     $     'L (LATLON)',
     $     'B (LATLON HEMIS.)',
     $     'Z (IRREG. CRTSIAN)',
     $     'Y (UNSTRUCTURED)',
     $     'X (UNKNOWN)'/
      
      found = .false.
      i = 1
 100  if (dictunit(i)(1:1).eq.projcode(1:1)) then
         found = .true.
         projnam = dictunit(i)
      endif
      
      i = i + 1
      if (.not.found.and.i.lt.dictlen) goto 100
      
      if (.not.found) then
         projnam='  UNKNOWN'
         projnam(1:1)=projcode(1:1)
      endif
      
      return
      end
