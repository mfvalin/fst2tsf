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
***function tsfecr -writes a TSF record to disk, short version of TSFECRF
*
      integer function tsfecr(frbuf,fniun, fcvar, fcnature, fcstamp,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,fnextra,fnnextra)
      
#include "impnone.cdk"
      
      integer fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,fniun,fntimestep,
     $    fnstepno,fntime,fntime2,fnbase,fndigits,fnextra(*),fnnextra

      character*48 fcvar 
      character*16 fcstamp, fcnature, fcvcoord, 
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
*     shortened version of tsfecrf
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
*     fnextra    I     I    A   Array of supplementary attributes
*     fnnextra   I     I    S   Number of supplementary attributes
*
*IMPLICIT     
*
*MODULES 
      integer tsfecrf
      external tsfecrf
*
**
      integer fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      
      integer ier
      
      fndatyp=1
      fnnbits=-16
      fnipdesc1=-1
      fnipdesc2=-1
      fnipdesc3=-1
      fnmapdesc1=-1
      fnmapdesc2=-1
      fnmapdesc3=-1
      fnmapdesc4=-1

      ier= tsfecrf(frbuf,fniun, fcvar, fcnature, fcstamp,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnextra,fnnextra)
      
      if (ier.lt.0) then
         tsfecr = -1
      else
         tsfecr = 0
      endif
      return
      end
      

