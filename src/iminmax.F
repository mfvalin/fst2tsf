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
***s/p iminmax -Returns the min and max of an integer 3D array
*
      subroutine iminmax(min, max,fld,nis,njs,nks,ni,nj,nk)
#include "impnone.cdk"
      integer nis,njs,nks,ni,nj,nk
      integer i,j,k
      integer min, max, fld(nis,njs,nks)
      
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Returns the min and max of an integer array
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     min        O     I    S   minimum value of field
*     max        O     I    S   maximum value of field        
*     fld        I     I    A   input field
*     nis        I     I    S   max. dimension of grid along X
*     njs        I     I    S   max. dimension of grid along Y
*     nks        I     I    S   max. dimension of grid along Z
*     ni         I     I    S   dimension of grid along X
*     nj         I     I    S   dimension of grid along Y
*     nk         I     I    S   dimension of grid along Z
*
*IMPLICIT     
*
*MODULES 
**

      min = fld(1,1,1)
      max = fld(1,1,1)
      
      do 100 k=1,nk
         do 100 j=1,nj
            do 100 i=1,ni
               if (fld(i,j,k).lt.min) then
                  min = fld(i,j,k)
               endif
               if (fld(i,j,k).gt.max) then
                  max = fld(i,j,k)
               endif
 100  continue
      return
      end
