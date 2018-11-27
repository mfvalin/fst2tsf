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
***s/p pdfprm -returns data descriptors of the current PDF record
*
      integer function pdfprm(fniun,fcvariable,fcnature,fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,fndatyp,fnxtra,fnnxtra)
#include "impnone.cdk"

      integer fniun
      integer fnni,fnnj,fnnk,fntimestep,
     $     fnstepno,fntime,fntime2,fnbase,fndigits,fndatyp,fnnbits,
     $     fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      integer fnxtra(*),fnnxtra
      
      character*48 fcvariable
      character*16 fcnature, fcstamp, fcvcoord, 
     $     fcmapproj,fcform,fcdate,fcunits
      real frlevel,frlevel2,
     $     frxpole,frypole,frmeshps,
     $     frmaprot,frmeshlat,frmeshlon,frswlat,frswlon,frmin,frmax

      integer ier
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     returns data descriptors of the current PDF record
*
*FILES 
*     tape(fniun): FORTRAN unit number used to read the data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fniun      O     I    S   fortran unit name
*     fcvar      O     C    S   PDF variable name
*     fcnature   O     C    S   PDF variable type
*     fcstamp    O     C    S   PDF descriptive attribute
*     fnni       O     I    S   dimension of grid along X
*     fnnj       O     I    S   dimension of grid along Y
*     fnnk       O     I    S   dimension of grid along Z
*     frlevel    O     R    S   PDF lower level of a layer
*     frlevel2   O     R    S   PDF upper level of a layer
*     fcvcoord   O     C    S   PDF vertical coordinate
*     fcdate     O     C    S   PDF date of origin
*     fntimestep O     I    S   PDF time step length (seconds)
*     fnstepno   O     I    S   PDF current step number
*     fntime     O     I    S   PDF start forecast time from date of 
*                               origin
*     fntime2    O     I    S   PDF end forecast time from date of 
*                               origin
*     fcmapproj  O     C    S   PDF type of geographical projection
*     frxpole    O     R    S   PDF X pos. of pole on the grid (PS grid)
*     frypole    O     R    S   PDF Y po. of pole on the grid (PS grid)
*     frmeshps   O     R    S   PDF grid length (meters) (PS grid)
*     frmaprot   O     R    S   PDF angle btn X axis and Greenwich mer.
*                               (PS grid)
*     frmeshlat  O     R    S   PDF grid length (degrees) (latlon grid)
*     frmeshlon  O     R    S   PDF grid length (degrees) (latlon grid)
*     frswlat    O     R    S   PDF lat of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frswlon    O     R    S   PDF lon of southwest corner of the grid
*                               (degrees) (latlon grid)
*     fcunits    O     C    S   PDF units associated with variable
*     fndatyp    O     I    S   PDF value of FST data type   
*                               used for compaction
*     fnextra    O     I    A   Array of supplementary attributes
*     fnnextra   O     I    S   Number of supplementary attributes
*
*IMPLICIT     
#include "pdf.cdk"
#include "cpdf.cdk"
*
*MODULES 
      integer  pdfgcom
      external pdfgcom
*
**
      ier =  pdfgcom(fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
      
      
      pdfprm = 0
      return
      end
      
