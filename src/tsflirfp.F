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
***function tsflirfp -reads the header part of a TSF record
*
      integer function tsflirfp(fniun, fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
#include "impnone.cdk"
      
      integer fnni,fnnj,fnnk,fniun,fntimestep,
     $     fnstepno,fntime,fntime2,fnbase,fndigits,fndatyp,fnnbits,
     $     fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      integer fnxtra(*),fnnxtra

      character*80 ligne
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
#include "ctsf.cdk"
#include "tsf.cdk"
#include "rogntudju.cdk"
*MODULES     
      integer  tsfgcom,tsfpcom
      external tsfgcom,tsfpcom
*
**
      character*40 header

      integer ni,nj,nk,timestep,stepno,time,time2,digits
      integer datyp,nbits,ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4,base
      integer ier
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,date,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max


      data header /'!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()'/ 
      data ni/-1/,nj/-1/,nk/-1/,timestep/-1/,stepno/-1/,
     $     time/-1/,time2/-1/,
     $     digits/-1/,variable/' '/,nature/' '/,stamp/'  '/,
     $     vertcoord/' '/,
     $     mapproj/' '/,form/'(5g14.6)'/,
     $     date/' '/,level/-1./,xpole/-1.0/,ypole/-1.0/,
     $     meshps/-1.0/,maprot/-1.0/,
     $     meshlat/-1.0/,meshlon/-1.0/,swlat/-1.0/,
     $     swlon/-1.0/,min/-1.0/,max/-1.0/,datyp/1/,
     $     ipdesc1/-1/,ipdesc2/-1/,ipdesc3/-1/,nbits/16/,
     $     mapdesc1/-1/,mapdesc2/-1/,mapdesc3/-1/,mapdesc4/-1/
      
      namelist /pdfparam/ variable,nature,stamp,units,date,time,time2,
     $     timestep,stepno,level,level2,vertcoord,ni,nj,nk,
     $     mapproj,xpole,ypole,meshps,maprot,meshlat,meshlon,
     $     swlat,swlon,base,form,digits,min,max,
     $     datyp,nbits,ipdesc1,ipdesc2,ipdesc3,
     $     mapdesc1,mapdesc2,mapdesc3,mapdesc4
      
      if (qdfmode.eq.1) then
 999     read (fniun,'(a80)',err=13,end=10) ligne
         if (ligne(1:40).ne.header) goto 999
#if defined (NEC)
            read (fniun,pdfparam,err=13,end=10)
#else
            read (fniun,nml=pdfparam,err=13,end=10) 
#endif
      else
         read (fniun,end=10,err=13) rogntudju
         read (fniun,end=10,err=13) 
#include "tsfparam.cdk"
      endif
    
      ier = tsfpcom(variable,nature,stamp,ni,nj,nk,level,level2,
     $    vertcoord,date,timestep,stepno,time,time2,
     $    mapproj,xpole,ypole,meshps,maprot,
     $    meshlat,meshlon,swlat,swlon,min,max,base,form,
     $    digits,units,datyp,nbits,ipdesc1,ipdesc2,ipdesc3,
     $    mapdesc1,mapdesc2,mapdesc3,mapdesc4,fnxtra,fnnxtra)
      
      ier =  tsfgcom(fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fndatyp,fnnbits,fnipdesc1,fnipdesc2,fnipdesc3,
     $     fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4,fnxtra,fnnxtra)
      tsflirfp = 0
      return 

	
 10   print *, '(TSFLIRFP) END OF FILE REACHED'
      tsflirfp = -1
      return
      
 13   print *,'(TSFLIRFP) READ ERROR'
      tsflirfp = -1
      return

      end
