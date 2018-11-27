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
***function tsfrd90i -reads the data of a TSF record stored in base 90
*
      integer function tsfrd90i(fld,nis,njs,nks,ni,nj,nk,
     $     iun,digits,imin,imax)
#include "impnone.cdk"
      integer nis,njs,nks,ni,nj,nk,digits,iun
      integer fld(nis,njs,nks),imin,imax
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     reads the data of a TSF record stored in base 90
*     data type is integer
*
*FILES 
*     tape(iun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fld        O     I    A   field values
*     iun        I     I    S   fortran unit name
*     nis        I     I    S   max. dimension of grid along X
*     njs        I     I    S   max. dimension of grid along Y
*     nks        I     I    S   max. dimension of grid along Z
*     ni         I     I    S   dimension of grid along X
*     nj         I     I    S   dimension of grid along Y
*     nk         I     I    S   dimension of grid along Z
*     digits     I     I    S   number of digits used to store a value
*     imin       I     I    S   min. value of fld
*     imax       I     I    S   max. value of fld
*
*IMPLICIT     
*
*MODULES 
*
**


      integer nbperline
      character*8 form
      character*76 ligne

      integer p90(5)
      integer i,j,k,m,n,m1,n1,var,icount
      integer ibase
      data p90  /1,90,8100,729000,65610000/

      integer delta

      ibase = p90(digits+1)-1

      if (imin.eq.imax) then
         print *, '(TSFRD90) WARNING -- MIN = MAX'
         print *, '(TSFRD90) ALL VALUES WILL BE SET TO MIN'
      endif

      delta = imax - imin

      nbperline = 76 / digits
      write(form,99) nbperline*digits
 15   read(iun,'(a76)',end=10) ligne
      if (ligne.ne.'START_DATA') goto 15

      m=1
      do 50 k=1,nk
         do 50 j=1,nj
            do 50 i=1,ni
               icount=i+ni*(j-1)+ni*nj*(k-1)
               if (mod(icount-1,nbperline).eq.0) then
                  read(iun,form,end=10)ligne
                  m=1
               endif
               m1=digits*(m-1)
               var = 0
               do 70 n=digits,1,-1
                  n1=digits-n+m1+1
                  var=var+p90(n)*(ichar(ligne(n1:n1))-33)
 70            continue
               fld(i,j,k)=var+imin
               m = m+1
 50   continue
 99   format('(a',i2.2,')')
      read(iun,'(a8)',end=10) ligne
      if (ligne.ne.'END_DATA') goto11

      tsfrd90i= 0
      return
 10   print *, '(TSFRD90) EOF - NO DATA READ'
      tsfrd90i= -1
      return

 11   print *,'(TSFRD90) CHECK FOR END_DATA FAILED'
      print *,'(TSFRD90) CORRUPTED FILE'
      tsfrd90i= -1
      end
      
