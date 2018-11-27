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
***function pdfwr90 -writes the data of a PDF record stored in base 90
*
      subroutine pdfwr90(fld,nis,njs,nks,ni,nj,nk,iun,digits,rmin,rmax)
#include "impnone.cdk"
      integer nis,njs,nks,ni,nj,nk,digits,iun
      real fld(nis,njs,nks),rmin,rmax
      integer nbperline,nbcur
      character*8 form

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     writes the data of a PDF record stored in base 90
*     data type is real
*
*FILES 
*     tape(iun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fld        I     R    A   field values
*     iun        I     I    S   fortran unit name
*     nis        I     I    S   max. dimension of grid along X
*     njs        I     I    S   max. dimension of grid along Y
*     nks        I     I    S   max. dimension of grid along Z
*     ni         I     I    S   dimension of grid along X
*     nj         I     I    S   dimension of grid along Y
*     nk         I     I    S   dimension of grid along Z
*     digits     I     I    S   number of digits used to store a value
*     rmin       I     R    S   min. value of fld
*     rmax       I     R    S   max. value of fld
*
*IMPLICIT     
*
*MODULES 
*
**
      character*76 ligne

      integer p90(5),frac,part
      integer i,j,k,n,var,iligne
      real*8 rbase
      data p90 /1,90,8100,729000,65610000/

      real delta

      rbase = p90(digits+1)-1

      if (rmin.eq.rmax) then
         print *, 'ERROR IN PDFWR90 -- MIN = MAX'
         print *, 'SETTING MIN=0, MAX=1'
         rmin = 0.0
         rmax = 1.0
         delta = 1.0
      else
         delta = rmax - rmin
      endif

      nbperline = 76 / digits
      write(form,99) nbperline*digits
      write(iun,'(a10)') 'START_DATA'

      nbcur = 0
      do 10 k=1,nk
         do 10 j=1,nj
            do 10 i=1,ni
               var = nint(rbase*(fld(i,j,k)-rmin)/delta)
               do 20 n=digits,1,-1
                  part=var/p90(n)
                  frac=mod(var,p90(n))
                  var=var-part*p90(n)
                  iligne=nbcur*digits+digits-n+1
                  ligne(iligne:iligne)=char(part+33)
 20            continue
               nbcur=nbcur+1
               if (nbcur.eq.nbperline) then
                  write(iun,form) ligne
                  nbcur=0
               endif
 10   continue
      if (nbcur.ne.0) then
         write(form,99) nbcur*digits
         write(iun,form) ligne
      endif
 99      format('(a',i2.2,')')
      write(iun,'(a8)') 'END_DATA'
      return
      end
