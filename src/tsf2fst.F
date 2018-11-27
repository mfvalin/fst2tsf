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
***s/p tsf2fst -Converts TSF data descriptors to FST data descriptors
*
      subroutine tsf2fst(nomvar,typvar,etiket,date,deet,npas,
     $     ip1,ip2,ip3,grtyp,ig1,ig2,ig3,ig4,
     $     variable, nature, stamp,mc2date,timestep,
     $     stepno,time,time2,level,level2,vcoord, 
     $     mapproj,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,
     $     ipdesc1,ipdesc2,ipdesc3,
     $     mapdesc1,mapdesc2,mapdesc3,mapdesc4)      
#include "impnone.cdk"
      character*48 variable
      character*16 stamp,vcoord,mapproj,mc2date,nature
      character*8  etiket
      character*2 nomvar
      character*1 typvar,grtyp
      real level,level2
      real xpole,ypole,meshps,maprot,meshlat,meshlon,swlat,swlon
      integer date,ip1,ip2,ip3,deet,npas,ig1,ig2,ig3,ig4
      integer time,time2,timestep,stepno
      integer ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4
      
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Converts data descriptors used in the TSF file format
*     into FST format 
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     nomvar     I     C    S   FST data descriptor (variable name) 
*     typvar     I     C    S   FST data descriptor (variable type)   
*     etiket     I     C    S   FST data descriptor (desc. stamp)
*     date       I     C    S   FST data descriptor (date of origin)
*     deet       I     C    S   FST data descriptor (time step length)
*     npas       I     C    S   FST data descriptor (time step no.)
*     ip1        I     I    S   FST data descriptor (level)
*     ip2        I     I    S   FST data descriptor (forecast time)
*     ip3        I     I    S   FST data descriptor (user defined)
*     grtyp      I     C    S   FST data descriptor (grid type)
*     ig1        I     I    S   FST data descriptor (map descriptor)
*     ig2        I     I    S   FST data descriptor (map descriptor)
*     ig3        I     I    S   FST data descriptor (map descriptor)
*     ig4        I     I    S   FST data descriptor (map descriptor)
*     variable   O     C    A   TSF variable name
*     nature     O     C    A   TSF variable type
*     stamp      O     C    A   TSF descriptive attribute 
*     level      O     R    S   TSF lower level of a layer
*     level2     O     R    S   TSF upper level of a layer
*     vcoord     O     C    S   TSF vertical coordinate
*     mc2date    O     C    S   TSF date of origin
*     timestep   O     I    S   TSF time step length (seconds)
*     stepno     O     I    S   TSF current step number
*     time       O     I    S   TSF start forecast time from date of 
*                               origin
*     time2      O     I    S   TSF end forecast time from date of origin
*     mapproj    O     C    S   TSF type of geographical projection
*     xpole      O     R    S   TSF X pos. of pole on the grid (PS grid)
*     ypole      O     R    S   TSF Y po. of pole on the grid (PS grid)
*     meshps     O     R    S   TSF grid length (meters) (PS grid)
*     maprot     O     R    S   TSF angle btn X axis and Greenwich mer.
*                               (PS grid)
*     meshlat    O     R    S   TSF grid length (degrees) (latlon grid)
*     meshlon    O     R    S   TSF grid length (degrees) (latlon grid)
*     swlat      O     R    S   TSF lat of southwest corner of the grid
*                               (degrees) (latlon grid)
*     swlon      O     R    S   TSF lon of southwest corner of the grid
*                               (degrees) (latlon grid)
*     ipdesc1    O     I    S   TSF original value of IP1
*     ipdesc2    O     I    S   TSF original value of IP2
*     ipdesc3    O     I    S   TSF original value of IP3
*     mapdesc1   O     I    S   TSF original value of IG1
*     mapdesc2   O     I    S   TSF original value of IG2
*     mapdesc3   O     I    S   TSF original value of IG3
*     mapdesc4   O     I    S   TSF original value of IG4
*
*IMPLICIT     
*
*MODULES
      integer p2fptch,ier
      external p2fptch
**


      character*16 temp

      nomvar=variable(1:2)
      typvar=nature(1:1)
      etiket=stamp(1:8)
      grtyp=mapproj(1:1)

      call low2up(nomvar,nomvar)
      call low2up(typvar,typvar)
      call low2up(etiket,etiket)
      call low2up(mapproj,mapproj)

      if (ipdesc1.eq.-1) then
         temp=vcoord
         call low2up(temp,temp)
         ip1 = nint(level)
         if (temp.eq.'SIGMA') then
            ip1=nint(2000.0+level*10000.0)
         endif
         if (temp.eq.'METERS'.or.temp.eq.'GAL-CHEN') then
            ip1=nint(12001.0+level/5.0)
         endif
      else
         ip1=ipdesc1
      endif

      call datp2f(date,mc2date)
      deet=timestep
      npas=stepno
      if (ipdesc2.eq.-1) then
         ip2=(deet*npas+1800)/3600
      else
         ip2=ipdesc2
      endif
      
      if (ipdesc3.ne.-1) then
         ip3=ipdesc3
      else
         ip3=0
      endif


      if (mapdesc1.eq.-1) then
         if (grtyp.eq.'N'.or.grtyp.eq.'S') then
            call cxgaig(grtyp,ig1,ig2,ig3,ig4,
     $           xpole,ypole,meshps,maprot)
         elseif (grtyp.eq.'L') then
            call cxgaig(grtyp,ig1,ig2,ig3,ig4,
     $           swlat,swlon,meshlat,meshlon)
         else
            ig1=0
            ig2=0
            ig3=0
            ig4=0
         endif
      else
         ig1=mapdesc1
         ig2=mapdesc2
         ig3=mapdesc3
         ig4=mapdesc4
      endif

      if (ipdesc1.eq.-1.or.ipdesc2.eq.-1) then
         ier = p2fptch(ip1,ip2,ip3,
     $        time,time2,level,level2,vcoord,nomvar)
      endif
      return
      end
