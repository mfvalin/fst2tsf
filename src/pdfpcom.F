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
***function pdfpcom -puts values into comdecks pdf and cpdf
*
      integer function pdfpcom(fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
#include "impnone.cdk"
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

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     puts values into comdecks pdf and cpdf
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fcvar      I     C    S   PDF variable name
*     fcnature   I     C    S   PDF variable type
*     fcstamp    I     C    S   PDF descriptive attribute
*     fnni       I     I    S   dimension of grid along X
*     fnnj       I     I    S   dimension of grid along Y
*     fnnk       I     I    S   dimension of grid along Z
*     frlevel    I     R    S   PDF lower level of a layer
*     frlevel2   I     R    S   PDF upper level of a layer
*     fcvcoord   I     C    S   PDF vertical coordinate
*     fcdate     I     C    S   PDF date of origin
*     fntimestep I     I    S   PDF time step length (seconds)
*     fnstepno   I     I    S   PDF current step number
*     fntime     I     I    S   PDF start forecast time from date of 
*                               origin
*     fntime2    I     I    S   PDF end forecast time from date of origin
*     fcmapproj  I     C    S   PDF type of geographical projection
*     frxpole    I     R    S   PDF X pos. of pole on the grid (PS grid)
*     frypole    I     R    S   PDF Y po. of pole on the grid (PS grid)
*     frmeshps   I     R    S   PDF grid length (meters) (PS grid)
*     frmaprot   I     R    S   PDF angle btn X axis and Greenwich mer.
*                               (PS grid)
*     frmeshlat  I     R    S   PDF grid length (degrees) (latlon grid)
*     frmeshlon  I     R    S   PDF grid length (degrees) (latlon grid)
*     frswlat    I     R    S   PDF lat of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frswlon    I     R    S   PDF lon of southwest corner of the grid
*                               (degrees) (latlon grid)
*     frmin      I     R    S   Min. value of array frbuf
*     frmax      I     R    S   Max. value of array frbuf
*     fnbase     I     I    S   base used to store the data (10 or 90)
*     fcform     I     C    S   FORTRAN format used to write the data to
*                               disk (base 10)
*     fndigits   I     I    S   number of digits used to write the data to
*                               disk (base 90)
*     fcunits    I     C    S   PDF units associated with variable
*     fndatyp    I     I    S   FST data type   
*     fnnbits    I     I    S   FST number of bits used for compaction
*     fnipdesc1  I     I    S   PDF original value of FST item IP1   
*     fnipdesc2  I     I    S   PDF original value of FST item IP2
*     fnipdesc3  I     I    S   PDF original value of FST item IP3
*     fnmapdesc1 I     I    S   PDF original value of FST item IG1
*     fnmapdesc2 I     I    S   PDF original value of FST item IG2
*     fnmapdesc3 I     I    S   PDF original value of FST item IG3
*     fnmapdesc4 I     I    S   PDF original value of FST item IG4
*     fnextra    I     I    A   Array of supplementary attributes
*     fnnextra   I     I    S   Number of supplementary attributes
*
*IMPLICIT     
#include "pdf.cdk"
#include "cpdf.cdk"
*
*MODULES 
*
**
      qdfvar = fcvariable
      qdfnat = fcnature
      qdfstmp = fcstamp
      qdfvc = fcvcoord
      qdfgtp = fcmapproj
      qdfform = fcform
      qdfdate = fcdate
      qdfun = fcunits
      
      qdfni = fnni
      qdfnj = fnnj
      qdfnk = fnnk
      qdfts =fntimestep 
      qdfstno = fnstepno
      qdft = fntime
      qdft2 = fntime2
      qdfdgt = fndigits
      qdfdtp = fndatyp
      qdfnbts = fnnbits
      qdfip1 = fnipdesc1
      qdfip2 = fnipdesc2
      qdfip3 = fnipdesc3
      qdfmap1 = fnmapdesc1
      qdfmap2 = fnmapdesc2
      qdfmap3 = fnmapdesc3
      qdfmap4 = fnmapdesc4
      qdfbase = fnbase
      qdflvl = frlevel
      qdflvl2 = frlevel2
      qdfxp = frxpole
      qdfyp = frypole
      qdfmshp = frmeshps
      qdfrot = frmaprot
      qdfmsla = frmeshlat
      qdfmslo = frmeshlon
      qdfswla = frswlat
      qdfswlo = frswlon
      qdfmin = frmin
      qdfmax = frmax
      
      pdfpcom = 0 
      return
      end
