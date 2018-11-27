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
***function tsfecrf -writes a TSF record to disk
*
      integer function tsfecrf(frbuf,fniun,fcvariable,fcnature,fcstamp,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
#include "impnone.cdk"

      integer fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,fniun,fntimestep,
     $     fnstepno,fntime,fntime2,fnbase,fndigits,fndatyp,fnnbits,
     $     fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      integer fnxtra(*),fnnxtra
      
      character*48 fcvariable
      character*16 fcnature, fcstamp, fcvcoord, 
     $     fcmapproj,fcform,fcdate,fcunits
      real frbuf(fnnis,fnnjs,fnnks), frlevel,frlevel2,
     $     frxpole,frypole,frmeshps,
     $     frmaprot,frmeshlat,frmeshlon,frswlat,frswlon,frmin,frmax
      
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     writes to disk a record coded in TSF data format
*
*FILES 
*     tape(fniun): FORTRAN unit number used to write the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     frbuf      I     R    A   field values
*     fniun      I     I    S   fortran unit name
*     fcvar      I     C    S   TSF variable name
*     fcnature   I     C    S   TSF variable type
*     fcstamp    I     C    S   TSF descriptive attribute
*     fnnis      I     I    S   max. dimension of grid along X
*     fnnjs      I     I    S   max. dimension of grid along Y
*     fnnks      I     I    S   max. dimension of grid along Z
*     fnni       I     I    S   dimension of grid along X
*     fnnj       I     I    S   dimension of grid along Y
*     fnnk       I     I    S   dimension of grid along Z
*     frlevel    I     R    S   TSF lower level of a layer
*     frlevel2   I     R    S   TSF upper level of a layer
*     fcvcoord   I     C    S   TSF vertical coordinate
*     fcdate     I     C    S   TSF date of origin
*     fntimestep I     I    S   TSF time step length (seconds)
*     fnstepno   I     I    S   TSF current step number
*     fntime     I     I    S   TSF start forecast time from date of 
*                               origin
*     fntime2    I     I    S   TSF end forecast time from date of origin
*     fcmapproj  I     C    S   TSF type of geographical projection
*     frxpole    I     R    S   TSF X pos. of pole on the grid (PS grid)
*     frypole    I     R    S   TSF Y po. of pole on the grid (PS grid)
*     frmeshps   I     R    S   TSF grid length (meters) (PS grid)
*     frmaprot   I     R    S   TSF angle btn X axis and Greenwich mer.
*                               (PS grid)
*     frmeshlat  I     R    S   TSF grid length (degrees) (latlon grid)
*     frmeshlon  I     R    S   TSF grid length (degrees) (latlon grid)
*     frswlat    I     R    S   TSF lat of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frswlon    I     R    S   TSF lon of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frmin      I     R    S   Min. value of array frbuf
*     frmax      I     R    S   Max. value of array frbuf
*     fnbase     I     I    S   base used to store the data (10 or 90)
*     fcform     I     C    S   FORTRAN format used to write the data to
*                               disk (base 10)
*     fndigits   I     I    S   number of digits used to write the data to
*                               disk (base 90)
*     fcunits    I     C    S   TSF units associated with variable
*     fndatyp    I     I    S   TSF value of FST data type   
*     fnnbits    I     I    S   TSF value of FST number of bits 
*                               used for compaction
*     fnipdesc1  I     I    S   TSF original value of FST item IP1   
*     fnipdesc2  I     I    S   TSF original value of FST item IP2
*     fnipdesc3  I     I    S   TSF original value of FST item IP3
*     fnmapdesc1 I     I    S   TSF original value of FST item IG1
*     fnmapdesc2 I     I    S   TSF original value of FST item IG2
*     fnmapdesc3 I     I    S   TSF original value of FST item IG3
*     fnmapdesc4 I     I    S   TSF original value of FST item IG4
*     fnextra    I     I    A   Array of supplementary attributes
*     fnnextra   I     I    S   Number of supplementary attributes
*
*IMPLICIT     
#include "tsf.cdk"
#include "ctsf.cdk"
#include "rogntudju.cdk"
*
*MODULES 
      integer  tsfgcom,tsfpcom,tsfwirec,tsfwrrec,tsfinfo
      external tsfgcom,tsfpcom,tsfwirec,tsfwrrec,tsfinfo
*
**
      integer ni,nj,nk,timestep,stepno,time,time2,digits
      integer datyp,nbits,ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4,base
      integer ier,i,j,k
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,date,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max

      real, dimension(:,:,:), allocatable :: tmprec

      character*40 header

      data header /'!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()'/ 
      namelist /pdfparam/ 
#include "tsfparam.cdk"

      ier = tsfpcom(fcvariable,fcnature,fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
      
      ier = tsfgcom(variable,nature,stamp,ni,nj,nk,level,level2,
     $     vertcoord,date,timestep,stepno,time,time2,
     $     mapproj,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max,base,form,
     $     digits,units,datyp,nbits,ipdesc1,ipdesc2,ipdesc3,
     $     mapdesc1,mapdesc2,mapdesc3,mapdesc4,fnxtra,fnnxtra)
      
      if (qdfinfo.eq.1) then
         if (qdfmode.ne.0) then
            if (base.eq.90) then
               form='*'
            endif
            write (fniun, '(a40)') header
            write (fniun, nml=pdfparam)
         else
            write (fniun) rogntudju
            write (fniun)
#include "tsfparam.cdk"
         endif
      else
         write (fniun, 100) fcvariable(1:2),fcnature(1:1),fcstamp(1:12),fnni,
     $        fnnj, fnnk, fcdate, fnipdesc1,fnipdesc2,fnipdesc3, 
     $        fntimestep, fnstepno,fndatyp,fcmapproj,
     $        fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      endif

 100  format(a2,1x,a1,1x,a12,1x,i5,1x,i5,1x,i5,1x,a16,1x,i8,1x,i8,
     $    1x,i8,1x,i6,1x,i6,1x,i2,1x,a1,1x,i5,1x,i5,1x,i5,1x,i5)
      
      allocate(tmprec(fnni,fnnj,fnnk))
      do k=1,fnnk
         do j=1,fnnj
            do i=1,fnni
               tmprec(i,j,k) = frbuf(i,j,k)
            enddo
         enddo
      enddo

      if (datyp.eq.2.or.datyp.eq.4) then
         ier= tsfwirec(tmprec,ni,nj,nk,ni,nj,nk,
     $        fniun,base,form,digits,nint(min),nint(max))
      else
         ier= tsfwrrec(tmprec,ni,nj,nk,ni,nj,nk,
     $        fniun,base,form,digits,min,max)
      endif
      
      deallocate(tmprec)


      if (ier.lt.0) then
         print *, 
     $        '(TSFECRF) WRITE ERROR ENCOUNTERED--- WRITE INCOMPLETE'
         tsfecrf = -1
      else
         ier= tsfinfo(fcvariable, fcnature, fcstamp,
     $        fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $        fcdate,fntimestep,fnstepno,fntime,fntime2,
     $        fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $        frmeshlat,frmeshlon,frswlat,frswlon,
     $        frmin,frmax,fnbase,fcform,fndigits,fcunits,'WRITE')
         tsfecrf = 0
      endif
      return
      end
