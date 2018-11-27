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
***function pdfquiet -turns off PDF diagnostic message printing
*
      integer function pdfquiet()
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     turns off PDF diagnostic message printing
*
*FILES 
*
*ARGUMENTS 
*
*IMPLICIT     
#include "pdf.cdk"
*
*MODULES 
*
**
      plverb = .false.

      pdfquiet = 0

      return
      end
