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
***function tsfinfo -print formatted information about tsf tags.
*
      integer function tsfinfo(fcvariable, fcnature, fcstamp,
     $     fnni,fnnj,fnnk,frlevel,frlevel2,fcvertcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fcmessage)
#include "impnone.cdk"
      integer fnni,fnnj,fnnk,fntimestep,
     $    fnstepno,fntime,fntime2,fndigits,fnbase
      
      character*48 fcvariable
      character*16 fcnature, fcstamp, fcvertcoord, 
     $     fcmapproj,fcform,fcdate,fcunits
      character*(*) fcmessage
      real frlevel,frlevel2,frxpole,frypole,frmeshps,
     $     frmaprot,frmeshlat,frmeshlon,frswlat,frswlon,frmin,frmax

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     print formatted information about tsf tags.
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fcvar      I     C    S   TSF variable name
*     fcnature   I     C    S   TSF variable type
*     fcstamp    I     C    S   TSF descriptive attribute
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
*     fcmessage  I     C    S   message printed on the diagnostic 
*                               (READ or WRITE)
*
*IMPLICIT     
#include "tsf.cdk"
*
*MODULES 
*
**
      integer i
      character*132 stars

      data plverb /.true./
      
      if (.not.plverb) then
         tsfinfo = 0
         return
      endif

      do 2 i=1,132
 2       stars(i:i) = '*'


      do 1 i=1,len(fcmessage)
 1       stars(i+5:i+5) = fcmessage(i:i)

      print 10, stars
      print 20, fcvariable, fcnature, fcstamp, fcunits
      print 30, fcvertcoord, frlevel,frlevel2
      print 40, fnni,fnnj,fnnk,frmin,frmax
      print 50, fcdate,fntimestep,fnstepno,fntime,fntime2

      if (fcmapproj(1:1).eq.'N'.or.fcmapproj(1:1).eq.'S') then
         print 60,fcmapproj,frxpole,frypole,frmeshps,frmaprot
      endif
      
      if (fcmapproj(1:1).eq.'L') then
         print 70,fcmapproj,frmeshlat,frmeshlon,frswlat,frswlon
      endif
      
      print 80, fnbase,fcform,fndigits
*     print 85,fndatyp,fnnbits
*     print 90, fnipdesc1,fnipdesc2,fnipdesc3
*     print 100,fnmapdesc1,fnmapdesc2,fnmapdesc3,fnmapdesc4
      
 10   format(' ',a132)
 20   format(' ','Variable=',a48,/,1x,'Nature  =',a16,2x,
     $     '|Stamp   =',a16,2x,'|Units   =',a16,2x,'|')
 30   format(' ','Vertcord=',a16,2x,'|Level   =',g10.4,
     $     8x,'|Level2  =',g10.4,8x,'|',9x,18x,'|')
 40   format(' ','NI=',i4,'  NJ=',i4,'  NK=',i3,3x,
     $     '|Min     =',g12.5,6x,'|Max     =',g12.5,6x,'|',
     $     9x,18x,'|')
 50   format(' ','date    =',a16,2x,'|timestep=',i5,13x,
     $     '|stepno  =',i5,13x,'|time    =',i5,13x,
     $     '|time2   =',i5)
 60   format(' ','Mapproj =',a16,2x, '|xpole   =',f7.2,11x,
     $     '|ypole   =',f7.2,11x,'|mesh    =',f10.2,8x,
     $     '|maprot  =',f7.2)
 70   format(' ','Mapproj =',a16,2x,'|meshlat =',f7.2,11x,
     $     '|meshlon =',f7.2,11x,
     $     '|swlat   =',f7.2,11x,'|swlon   =',f7.2)
 80   format(' ','Base    =',i2,16x,'|Format  =',a16,2x,
     $     '|digits  =',i1,17x,'|',27x,'|')
 90   format(' ','ipdesc1 =',i4,14x,
     $     '|ipdesc2 =',i5,13x,
     $     '|ipdesc3 =',i5,13x,
     $     '|',27x,'|')
 100  format(' ','mapdesc1=',i5,13x,
     $     '|mapdesc2=',i5,13x,
     $     '|mapdesc3=',i5,13x,
     $     '|mapdesc4=',i4)
      return
      end
      

