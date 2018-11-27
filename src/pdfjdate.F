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
      subroutine pdfjadte(jdate,yyyy,mo,dd,hh,mm,ss)
      implicit none
      real*8 jdate
      integer yyyy,mo,dd,hh,mm,ss


*
*  calculate julian calendar day
*  see cacm letter to editor by fliegel and flandern 1968
*  page 657
*
      integer jd,jyy,jmo,jdd
      jd(jyy,jmo,jdd)=jdd-32075+1461*(jyy+4800+(jmo-14)/12)/4
     x     +  367*(jmo-2-(jmo-14)/12*12)/12 - 3
     x     *((jyy+4900+(jmo-14)/12)/100)/4


      jdate = jd(yyyy,mo,dd)
      jdate = jdate + (hh*3600+mm*60+ss)/86400.0
      return
      end

