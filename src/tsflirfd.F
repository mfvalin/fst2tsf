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
***function tsflirfd -reads the data part of a TSF record
*
      integer function tsflirfd(frbuf,fniun,fnnis,fnnjs,fnnks,
     $     fnni,fnnj,fnnk)
#include "impnone.cdk"
      
      integer fnnis,fnnjs,fnnks
      integer fnni,fnnj,fnnk,fniun

      real frbuf(fnnis,fnnjs,fnnks)
      integer ier
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     reads the data part of a TSF record*
*FILES 
*     tape(fniun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     frbuf      I     R    A   field values
*     fniun      I     I    S   fortran unit name
*     fnnis      I     I    S   max. dimension of grid along X
*     fnnjs      I     I    S   max. dimension of grid along Y
*     fnnks      I     I    S   max. dimension of grid along Z
*     fnni       I     I    S   dimension of grid along X
*     fnnj       I     I    S   dimension of grid along Y
*     fnnk       I     I    S   dimension of grid along Z
*
*IMPLICIT     
#include "ctsf.cdk"
#include "tsf.cdk"
*
*MODULES 
      integer  tsfrirec,tsfrdrec,tsfinfo
      external tsfrirec,tsfrdrec
*
**

      integer i,j,k
      real, dimension(:,:,:), allocatable :: tmprec
      allocate(tmprec(fnni,fnnj,fnnk))

      if (qdfdtp.eq.2.or.qdfdtp.eq.4) then
         ier= tsfrirec(tmprec,qdfni,qdfnj,qdfnk,qdfni,qdfnj,qdfnk,
     $        fniun,qdfbase,qdfform,qdfdgt,int(qdfmin),nint(qdfmax))
      else
         ier= tsfrdrec(tmprec,qdfni,qdfnj,qdfnk,qdfni,qdfnj,qdfnk,
     $        fniun,qdfbase,qdfform,qdfdgt,qdfmin,qdfmax)
      endif

      do k=1,qdfnk
         do j=1,qdfnj
            do i=1,qdfni
               frbuf(i,j,k) = tmprec(i,j,k)
            enddo
         enddo
      enddo

      deallocate(tmprec)
            
      
      if (ier.lt.0) then
         print *, 
     $        '(TSFLIRFD) READ ERROR ENCOUNTERED--- READ INCOMPLETE'
         tsflirfd = -1
      else
         tsflirfd = 0
         ier= tsfinfo(qdfvar, qdfnat,qdfstmp,
     $        qdfni,qdfnj,qdfnk,qdflvl,qdflvl2,qdfvc,
     $        qdfdate,qdfts,qdfstno,qdft,qdft2,
     $        qdfgtp,qdfxp,qdfyp,qdfmshp,qdfrot,
     $        qdfmsla,qdfmslo,qdfswla,qdfswlo,
     $        qdfmin,qdfmax,qdfbase,qdfform,qdfdgt,qdfun,'READ')
      endif
      return
      
 10   print *, '(TSFLIRFD) END OF FILE REACHED'
      tsflirfd = -1
      return
      
 13   print *,'(TSFLIRFD) READ ERROR'
      tsflirfd = -1
      return

      end
