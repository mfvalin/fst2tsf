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
***s/p difdatsd
*
      subroutine difdatsd(diff,date1,date2)
#include "impnone.cdk"
      real*8 diff
      character*16 date1,date2

      real*8 rdate1,rdate2
      integer sign

      integer yy,mo,dd,hh,mm,ss
      
      call prsdate(yy,mo,dd,hh,mm,ss,sign,date1)
      call tsfjdate(rdate1,yy,mo,dd,hh,mm,ss)
      call prsdate(yy,mo,dd,hh,mm,ss,sign,date2)
      call tsfjdate(rdate2,yy,mo,dd,hh,mm,ss)

      diff = rdate2 - rdate1

      return
      end
