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
***s/p pdfbksp -unreads a record
*
      integer function pdfbksp(iun)
#include "impnone.cdk"
      integer iun
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Repositions the file pointer to the beginning of a valid PDF record
*     by successive calls to the backspace I/O intrinsic.
*
*FILES 
*     tape(iun) - current unit number
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     iun        I     I    S   fortran unit number     
*
*IMPLICIT     
*
*MODULES 
*
**

      character*80 ligne(0:1)
      character*40 header
      integer i
      data header /'!@#$%^&*()!@#$%^&*()!@#$%^&*()!@#$%^&*()'/ 
*     
*     We verify that we are not at the beginning of the file
*     It is assumed that two identical lines do not exist in the file.
*     If the lines are found to be identical, it means that we are
*     at the beginning of the file.
*     
*     We look for the magic header string
*      
      ligne(0) = 'a'
      ligne(1) = 'b'
      
      i = 0
      backspace(iun,err=999)
      backspace(iun,err=999)
 1000 read(iun,'(A80)')ligne(i)
      if (ligne(i)(1:40).eq.header) then
         backspace(iun,err=999)
         pdfbksp = 0
         return
      elseif (ligne(0).eq.ligne(1)) then
         pdfbksp = 0
         return
      else
         backspace(iun,err=999)
         backspace(iun,err=999)
         i = mod(i+1,2)
         goto 1000
      endif
 999  pdfbksp = -1
      return
      end
      
