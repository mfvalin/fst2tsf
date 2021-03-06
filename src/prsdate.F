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
      subroutine prsdate(yy,mo,dd,hh,mm,ss,sign,date)
      implicit none
      integer yy,mo,dd,hh,mm,ss,sign
      character*16 date
      character*16 tmpdate
      character*4 cyy
      character*2 cmo,cdd,chh,cmm,css

      if (date(1:1).eq.'-') then
         sign = -1
         tmpdate=date(2:16)
      else
         if (date(1:1).eq.' ') then
            sign = 1
            tmpdate=date(2:16)
         else
            sign = 1
            tmpdate=date(1:15)
         endif
      endif

      cyy=tmpdate(1:4)
      cmo=tmpdate(5:6)
      cdd=tmpdate(7:8)
      chh=tmpdate(10:11)
      cmm=tmpdate(12:13)
      css=tmpdate(14:15)

      read(cyy,'(I4)') yy
      read(cmo,'(I2)') mo
      read(cdd,'(I2)') dd
      read(chh,'(I2)') hh
      read(cmm,'(I2)') mm
      read(css,'(I2)') ss
      
      return
      end


