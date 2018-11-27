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
***function tsfrirec -reads the data part of a TSF record
*
      integer function tsfrirec(fld,nis,njs,nks,ni,nj,nk,
     $     iun,base,form,digits,imin,imax)
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
*     reads the data of a TSF record
*     data type is integer
*
*FILES 
*     tape(iun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fld        O     I    A   field values
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
#include "tsf.cdk"
*
*MODULES 
      integer tsfrd90i
      external tsfrd90i
*
**

      integer i,j,k
      character*80 pcline
      
      if (qdfmode.eq.0) then
         read (iun) fld
      else
         if (base.eq.90) then
            tsfrirec=tsfrd90i(fld,nis,njs,nks,ni,nj,nk,iun,digits,
     $           imin,imax)
            return
         else
 15         read (iun,'(a80)',end=10) pcline
            if (pcline.ne.'START_DATA') goto15
            
            read(iun,*,err=16,end=17)(((fld(i,j,k),i=1,ni),j=1,nj),
     $           k=1,nk)
            
            read (iun,'(a80)',end=10) pcline
            if (pcline(1:8).ne.'END_DATA') goto 18
            tsfrirec = 0
            return
         endif
      endif
      
 16   print *, 
     $     '(TSFRIREC) READ ERROR ENCOUNTERED--- READ INCOMPLETE'
      tsfrirec = -1
      return
      
 17   print *, 
     $     '(TSFRIREC) END OF FILE--- READ INCOMPLETE'
      tsfrirec = -1
      return
      
 18   print *, 
     $     '(TSFRIREC) NOT ENOUGH DATA READ--- READ INCOMPLETE'
      tsfrirec = -1
      return
      
 10   print *, 
     $     '(TSFRIREC) END OF FILE REACHED - NO RECORD READ'
      tsfrirec = -1
      return
      
      end

