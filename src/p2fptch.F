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
***s/p p2fptch -spcial encoding of FST ip1-ip2-ip3
*
      integer function p2fptch(ip1,ip2,ip3,time,time2,level,level2,
     $     vcoord,nomvar)
#include "impnone.cdk"
      integer ip1,ip2,ip3
      character*2 nomvar
      character*16 vcoord
      integer time,time2
      real level,level2

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     special encoding of ip1-ip2-ip3 for the variables present 
*     in this code
*     For PR, ip2 = time,    ip3 = time2
*     For ST, ip2 = time,    ip3 = time2
*     For DZ, ip1 = level1,  ip2 = level2
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     ip1        I     I    S   FST data descriptor (normally level)
*     ip2        I     I    S   FST data descriptor (normally 
*                               forecast time)
*     ip3        I     I    S   FST data descriptor (user defined)
*     time       O     I    S   start forecast time from date of 
*                               origin
*     time2      O     I    S   end forecast time from date of origin
*     level      O     R    S   lower level of a layer
*     level2     O     R    S   upper level of a layer
*     vcoord     I     C    S   TSF vertical coordinate
*     nomvar     I     C    S   FST data descriptor (variable name) 
*
*IMPLICIT     
*
*MODULES
      external lvl2ip1 
**
      integer kind

      if (nomvar(1:2).eq.'PR') then
         ip1=time
         ip2=time2
      endif

      if (nomvar(1:2).eq.'ST') then
         ip1=time
         ip2=time2
      endif

      if (nomvar(1:2).eq.'DZ') then
         call convip( ip1, level, kind, +1, vcoord, .true.)
         call convip( ip2, level2, kind, +1, vcoord, .true.)
         ip3=time
      endif

      p2fptch = 0
      return
      end
