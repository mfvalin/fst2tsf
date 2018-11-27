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
***function tsflirp -shortened version of tsflirfp
*
      integer function tsflirp(fniun,fcvar,fcnature,fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fnextra,fnnextra)
#include "impnone.cdk"
      
      integer fnni,fnnj,fnnk,fniun,fntimestep,
     $    fnstepno,fntime,fntime2,fnbase,fndigits,fnextra(*),fnnextra

      character*48 fcvar 
      character*16 fcstamp, fcnature, fcvcoord, 
     $     fcmapproj,fcform,fcdate,fcunits
      real frlevel,frlevel2,
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
*     reads from disk the header of a TSF record
*
*FILES
*     tape(fniun): FORTRAN unit number used to read the data
*
*ARGUMENTS
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fniun      O     I    S   fortran unit name
*     fcvar      O     C    S   TSF variable name
*     fcnature   O     C    S   TSF variable type
*     fcstamp    O     C    S   TSF descriptive attribute
*     fnni       O     I    S   dimension of grid along X
*     fnnj       O     I    S   dimension of grid along Y
*     fnnk       O     I    S   dimension of grid along Z
*     frlevel    O     R    S   TSF lower level of a layer
*     frlevel2   O     R    S   TSF upper level of a layer
*     fcvcoord   O     C    S   TSF vertical coordinate
*     fcdate     O     C    S   TSF date of origin
*     fntimestep O     I    S   TSF time step length (seconds)
*     fnstepno   O     I    S   TSF current step number
*     fntime     O     I    S   TSF start forecast time from date of
*                               origin
*     fntime2    O     I    S   TSF end forecast time from date of origin
*     fcmapproj  O     C    S   TSF type of geographical projection
*     frxpole    O     R    S   TSF X pos. of pole on the grid (PS grid)
*     frypole    O     R    S   TSF Y po. of pole on the grid (PS grid)
*     frmeshps   O     R    S   TSF grid length (meters) (PS grid)
*     frmaprot   O     R    S   TSF angle btn X axis and Greenwich mer.
*                               (PS grid)
*     frmeshlat  O     R    S   TSF grid length (degrees) (latlon grid)
*     frmeshlon  O     R    S   TSF grid length (degrees) (latlon grid)
*     frswlat    O     R    S   TSF lat of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frswlon    O     R    S   TSF lon of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frmin      O     R    S   Min. value of array frbuf
*     frmax      O     R    S   Max. value of array frbuf
*     fnbase     O     I    S   base used to store the data (10 or 90)
*     fcform     O     C    S   FORTRAN format used to write the data to
*                               disk (base 10)
*     fndigits   O     I    S   number of digits used to write the data to
*                               disk (base 90)
*     fcunits    O     C    S   TSF units associated with variable
*     fnextra    O     I    A   Array of supplementary attributes
*     fnnextra   O     I    S   Number of supplementary attributes
*
*IMPLICIT
*
*MODULES
      integer tsflirfp,ier
      external tsflirfp
*
**
      integer fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      
      
      ier= tsflirfp(fniun, fcvar, fcnature, 
     $     fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnextra,fnnextra)
      
      if (ier.lt.0) then
         tsflirp = -1
      else
         tsflirp = 0
      endif

      return
      end
      
      

