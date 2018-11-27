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
      subroutine incdatss(newdate, olddate, dt)
      implicit none
      character*16 newdate,olddate,dt

      real*8 jolddate,jnewdate

      integer newyy,newmo,newdd,newhh,newmm,newss,i
      integer oldyy,oldmo,olddd,oldhh,oldmm,oldss,oldsign      
      integer dtyy,dtmo,dtdd,dthh,dtmm,dtss,dtsign
      
      call prsdate(oldyy,oldmo,olddd,oldhh,oldmm,oldss,oldsign,olddate)
      call prsdate(dtyy,dtmo,dtdd,dthh,dtmm,dtss,dtsign,dt)

      call tsfjdate(jolddate,oldyy,oldmo,olddd,oldhh,oldmm,oldss)
      jnewdate=jolddate+dtsign*(dtdd+
     $     (dthh*3600.0+dtmm*60.0+float(dtss))/86400.0)

      call tsfcdate(newyy,newmo,newdd,newhh,newmm,newss,jnewdate)

      do 10 i=1,dtmo
         newmo=newmo+1*dtsign
         call tsfjdate(jnewdate,newyy,newmo,newdd,newhh,newmm,newss)
         call tsfcdate(newyy,newmo,newdd,newhh,newmm,newss,jnewdate)
 10   continue

      do 20 i=1,dtyy
         newyy=newyy+dtsign*i
         call tsfjdate(jnewdate,newyy,newmo,newdd,newhh,newmm,newss)
         call tsfcdate(newyy,newmo,newdd,newhh,newmm,newss,jnewdate)
 20   continue

      write(newdate,12) newyy,newmo,newdd,newhh,newmm,newss
 12   format(i4.4,i2.2,i2.2,'.',i2.2,i2.2,i2.2)
      return
      end
      

