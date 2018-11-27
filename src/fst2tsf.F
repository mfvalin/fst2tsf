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
***s/p fst2tsf -Converts RPN data descriptors to TSF data descriptors
*
      subroutine fst2tsf(variable,nature,stamp,level,level2,vcoord, 
     $     mc2date,timestep,stepno,time,time2,
     $     mapproj,xpole,ypole,meshps,
     $     maprot,meshlat,meshlon,swlat,swlon,
     $     ipdesc1,ipdesc2,ipdesc3,
     $     mapdesc1,mapdesc2,mapdesc3,mapdesc4,
     $     nomvar,typvar,etiket,date,deet,npas,
     $     ip1,ip2,ip3,grtyp,ig1,ig2,ig3,ig4)
#include "impnone.cdk"
      character*16 variable,mc2date,nature
      character*16 stamp,vcoord,mapproj
      character*8  etiket
      character*2 nomvar
      character*1 typvar,grtyp
      real level,level2
      real xpole,ypole,meshps,maprot,meshlat,meshlon,swlat,swlon
      integer date,deet,npas,ip1,ip2,ip3,ig1,ig2,ig3,ig4
      integer ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4
      integer timestep,stepno,time,time2
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Converts some of the RPN standard file data descriptors 
*     returned by FSTPRM to those used in the TSF file format
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     variable   O     C    S   TSF variable name
*     nature     O     C    S   TSF variable type
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
*
*IMPLICIT     
*
*MODULES
      integer  f2pptch
      external f2pptch,datf2p,cigaxg 
**


      integer ier,kind

      variable=nomvar
      nature=typvar
      stamp=etiket
      mapproj=grtyp

      ipdesc1=ip1
      ipdesc2=ip2
      ipdesc3=ip3
      mapdesc1=ig1
      mapdesc2=ig2
      mapdesc3=ig3
      mapdesc4=ig4
      
      time=ip2
      time2=ip2

      call datf2p(mc2date,date)
      timestep=deet
      stepno=npas
      call convip( ip1, level, kind, -1, vcoord, .true.)

      level2=level

      meshlat=0.0
      meshlon=0.0
      swlat=0.0
      swlon=0.0
      xpole=0.0
      ypole=0.0
      meshps=0.0
      maprot=0.0

      if (grtyp.eq.'N'.or.grtyp.eq.'S') then
         call cigaxg(grtyp,xpole,ypole,meshps,maprot,ig1,ig2,ig3,ig4)
      elseif (grtyp.eq.'L') then
         call cigaxg(grtyp,swlat,swlon,meshlat,meshlon,ig1,ig2,ig3,ig4)
      else
      endif

      ier = f2pptch(time,time2,level,level2,ip1,ip2,ip3,vcoord,nomvar)
      return
      end

      
