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
***function tsfverb  -turns on TSF diagnostic message printing
*
      subroutine tsfmode(mode)
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      Nov 1994
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     switches data write to/from binary from/to ascii
*
*FILES 
*
*ARGUMENTS 
      character*1 mode
*
*IMPLICIT     
#include "tsf.cdk"
*
*MODULES 
*
**
      if (mode.eq.'a'.or.mode.eq.'A') then
         qdfmode = 1
      else
         qdfmode = 0
      endif

      
      return
      end
