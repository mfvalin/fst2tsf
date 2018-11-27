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
***s/p datp2f
      subroutine datp2f(fstdate,mc2date)
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT (datp2f)
*     This module encodes a date in the RPN standard file format
*     from a date defined in the TSF format. 
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
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     fstdate    O     I    S   date encoded in RPN standard file 
*                               format
*     mc2date    I     C    S   date encoded in TSF format
*
*IMPLICIT (none)    
*
*MODULES 
      external incdat
*
**
      
      character*16 mc2date
      integer fstdate
      integer yy,mo,dd,hh,mm,ss,yymmdd,hhmmss
      
      character*4 cyy
      character*2 cmo,cdd,chh,cmm,css

      cyy=mc2date(1:4)
      cmo=mc2date(5:6)
      cdd=mc2date(7:8)
      chh=mc2date(10:11)
      cmm=mc2date(12:13)
      css=mc2date(14:15)

      read(cyy,'(I4)') yy
      read(cmo,'(I2)') mo
      read(cdd,'(I2)') dd
      read(chh,'(I2)') hh
      read(cmm,'(I2)') mm
      read(css,'(I2)') ss
      
      yymmdd = 10000 * yy + 100 * mo + dd
      hhmmss = 1000000 * hh   + 10000 * mm + 100 * ss
      call newdate(fstdate, yymmdd, hhmmss, 3)

      return
      end


