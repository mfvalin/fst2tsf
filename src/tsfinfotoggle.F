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
***function tsfinfotoggle  -turns on TSF diagnostic message printing
*
      subroutine tsfinfotoggle(mode)
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
      character*(*) mode
*
*IMPLICIT     
#include "tsf.cdk"
*
*MODULES 
*
**
      if (mode(1:1).eq.'l'.or.mode(1:1).eq.'L') then
         qdfinfo = 1
      else
         qdfinfo = 0
      endif

      
      return
      end
