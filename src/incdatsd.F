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
      subroutine incdatsd(newdate, olddate, dt)
      implicit none
      character*16 newdate,olddate
      real *8 dt

      real*8 jolddate,jnewdate

      integer newyy,newmo,newdd,newhh,newmm,newss
      integer oldyy,oldmo,olddd,oldhh,oldmm,oldss,oldsign      
      
      call prsdate(oldyy,oldmo,olddd,oldhh,oldmm,oldss,oldsign,olddate)

      call tsfjdate(jolddate,oldyy,oldmo,olddd,oldhh,oldmm,oldss)
      jnewdate=jolddate+dt

      call tsfcdate(newyy,newmo,newdd,newhh,newmm,newss,jnewdate)

      write(newdate,12) newyy,newmo,newdd,newhh,newmm,newss
 12   format(i4.4,i2.2,i2.2,'.',i2.2,i2.2,i2.2)
      return
      end
      

