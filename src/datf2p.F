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
***s/p datf2p
*
      subroutine datf2p(mc2date,fstdate)
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT (datf2p)
*     This module encodes a date in the TSF format from a date
*     defined in the RPN standard file format. 
*     The RPN format for the date is 'MOYYDDHHR'.
*     The TSF format for the date is 'YYYYMODD.HHMMSS'
*     YY:   Year (1970-2069)
*     YYYY: Year (-9999-+9999)
*     MO:   Month (1-12)
*     DD:   Day (1-31)
*     HH:   Hour (00-23)
*     MM:   Minutes (00-59)
*     SS:   Seconds (00-59)
*    
*
*FILES (none)
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S DESCRIPTION
*
*     mc2date    O     C    S  date encoded in TSF format
*     fstdate    I     I    S  date encoded in RPN standard file
*                              format
*
*IMPLICIT (none)    
*
*MODULES 
*
**
      character*16 mc2date
      integer fstdate
      integer yymmdd, hhmmss
      integer mode

      mode = -3
      call newdate(fstdate, yymmdd, hhmmss, mode)

      write(mc2date,10) yymmdd,hhmmss/100
 10   format(i8.8,'.',i6.6)

      return
      end


