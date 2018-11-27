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
***s/p pdfrirec -writes the data part of a PDF record
*
      subroutine pdfwirec(fld,nis,njs,nks,ni,nj,nk,iun,base,
     $     form,digits,imin,imax)
#include "impnone.cdk"
      integer nis,njs,nks,ni,nj,nk,digits,iun,base
      integer fld(nis,njs,nks),imin,imax
      character*16 form

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     writes the data of a PDF record
*     data type is integer
*
*FILES 
*     tape(iun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fld        I     I    A   field values
*     nis        I     I    S   max. dimension of grid along X
*     njs        I     I    S   max. dimension of grid along Y
*     nks        I     I    S   max. dimension of grid along Z
*     ni         I     I    S   dimension of grid along X
*     nj         I     I    S   dimension of grid along Y
*     nk         I     I    S   dimension of grid along Z
*     iun        I     I    S   fortran unit name
*     base       I     I    S   base used to store the data (10 or 90)
*     form       I     C    S   FORTRAN format used to write the data to
*                               disk (base 10)
*     digits     I     I    S   number of digits used to write the data to
*                               disk (base 90)
*     imin       I     I    S   min. value of fld
*     imax       I     I    S   max. value of fld
*
*IMPLICIT     
*
*MODULES 
      external pdfwr90i
*
**

      integer i,j,k

      if (base.eq.90) then
         call pdfwr90i(fld,nis,njs,nks,ni,nj,nk,iun,digits,
     $        imin,imax)
      else
         write(iun,'(a10)') 'START_DATA'
         write(iun,*)(((fld(i,j,k),i=1,ni),j=1,nj),k=1,nk)
         write(iun,'(a8)')  'END_DATA'
      endif
      
      return
      end

