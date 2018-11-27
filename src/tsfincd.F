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
      subroutine tsfincd
      implicit none
      
      character*16 cle(10),def(10),val(10)
      character*8 dtyy,dtmo,dtdd,dthh,dtmm,dtss
      data cle /'date','dt','dtstep','nsteps','dtss',
     $          'dtmm','dthh','dtdd','dtmo','dtyy'/
      data def /10 * '0'/
      data val /10 * '0'/

      integer yy,mo,dd,hh,mm,ss

      real*8 rdt
      integer dtstep,nsteps

      character*16 olddate, dt, newdate
      character*1 csign
      integer ipos

      call ccard(cle,def,val, 10, ipos)

      dtyy = val(10)
      dtmo = val( 9)
      dtdd = val( 8)
      dthh = val( 7)
      dtmm = val( 6)
      dtss = val( 5)

      csign = ' '
      dt = val(2)
      olddate = val(1)

      read(val(3),'(i8)') dtstep
      read(val(4),'(i8)') nsteps
      read(val(5),'(i8)') ss
      read(val(6),'(i8)') mm
      read(val(7),'(i8)') hh
      read(val(8),'(i8)') dd
      read(val(9),'(i8)') mo
      read(val(10),'(i8)') yy

      if (dt.eq.'0') then
         if (nsteps.eq.0) then
            if (yy.eq.0.and.mo.eq.0) then
               rdt= dd+(hh*3600.0+mm*60.0+float(ss))/86400.0
               call incdatsd(newdate,olddate,rdt)
            else
               if (ss.lt.0.or.mm.lt.0.or.hh.lt.0.or.
     $              dd.lt.0.or.mo.lt.0.or.yy.lt.0) then
                  csign='-'
               endif
               
               write(dt,12) csign,abs(yy),abs(mo),abs(dd),
     $                      abs(hh),abs(mm),abs(ss)
               call incdatss(newdate,olddate,dt)
            endif
         else
	    rdt = nsteps
            rdt = rdt * dtstep
            rdt = rdt / 86400.0
            call incdatsd(newdate,olddate,rdt)
         endif
      else
         call incdatss(newdate,olddate,dt)
      endif
      
 12   format(a1,i4.4,i2.2,i2.2,'.',i2.2,i2.2,i2.2)

      
      write (6,13) newdate
 13   format(a15)

      stop
      end

