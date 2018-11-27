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
***function tsflirf -reads a TSF record to disk
*
      integer function tsflirf(frbuf,fniun,fcvariable,fcnature,fcstamp,
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
**    

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     reads from disk a record coded in TSF data format
*
*FILES 
*     tape(fniun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     frbuf      O     R    A   field values
*     fniun      O     I    S   fortran unit name
*     fcvar      O     C    S   TSF variable name
*     fcnature   O     C    S   TSF variable type
*     fcstamp    O     C    S   TSF descriptive attribute
*     fnnis      O     I    S   max. dimension of grid along X
*     fnnjs      O     I    S   max. dimension of grid along Y
*     fnnks      O     I    S   max. dimension of grid along Z
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
*     fcunits    O     C    S   TSF units associated with variable
*     fndatyp    O     I    S   TSF value of FST data type   
*     fnnbits    0     I    S   TSF value of FST number of bits 
*                               used for compaction
*     fnipdesc1  O     I    S   TSF original value of FST item IP1   
*     fnipdesc2  O     I    S   TSF original value of FST item IP2
*     fnipdesc3  O     I    S   TSF original value of FST item IP3
*     fnmapdesc1 O     I    S   TSF original value of FST item IG1
*     fnmapdesc2 O     I    S   TSF original value of FST item IG2
*     fnmapdesc3 O     I    S   TSF original value of FST item IG3
*     fnmapdesc4 O     I    S   TSF original value of FST item IG4
*     fnextra    O     I    A   Array of supplementary attributes
*     fnnextra   O     I    S   Number of supplementary attributes
*
*IMPLICIT     
*
*MODULES 
      integer  tsflirfp, tsflirfd
      external tsflirfp, tsflirfd
*
**
      integer ier

      ier = tsflirfp(fniun, fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
      if (ier.lt.0) then
         tsflirf = ier
         return
      endif
      
      ier =  tsflirfd(frbuf,fniun,fnnis,fnnjs,fnnks,fnni,fnnj,fnnk)
      tsflirf = ier
      return
      end
