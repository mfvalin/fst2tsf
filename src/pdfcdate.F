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
      subroutine pdfcdate(yyyy,mo,dd,hh,mm,ss,jdate)
      implicit none
      real*8 jdate
      integer yyyy,mo,dd,hh,mm,ss,seconds

      real*8 f,rj

      rj = int(jdate)
      f = jdate - rj
      seconds = nint(f * 86400.0)
      hh = seconds / 3600
      mm = (seconds - hh * 3600) / 60
      ss = seconds - hh * 3600 - mm * 60

      call datec(int(rj),yyyy,mo,dd)

      return
      end
