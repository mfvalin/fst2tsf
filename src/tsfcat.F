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
***s/p tsfcat -compression/decompression utility for the TSF file format
*
      subroutine tsfcat(tsffile,itsfmode,obinfile,otsfmode,
     $     pform, cdigits, compress)
#include "impnone.cdk"
      character*128 tsffile,obinfile
      integer itsfmode,otsfmode
      character*32 pform
      character*8 cdigits
      character*8 compress

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     compression,decompression and re-encoding of a TSF file
*
*FILES 
*     Unit 2: TSF file tsffile (when -tsf option is used)
*     Unit 5: TSF file (when redirection of i/o is used)
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     tsffile    I     C    S   name of the file to re-encode
*     pform      I     C    S   FORTRAN format used when encoding 
*                               in plain format (base 10) 
*     cdigits    I     C    S   number of digits used when encoding
*                               in compressed format (base 90)  
*     compress   I     C    S   switch to select between compressed
*                               and uncompressed format
*
*IMPLICIT     
*
*MODULES 
*
      integer tsflirf,tsfquiet,tsfecrf
      external tsflirf,tsfquiet,tsfecrf
*
**

      integer nis,njs,nks
      parameter (nis=512)
      parameter (njs=512)
      parameter (nks=1)
      
      integer ni,nj,nk,time,time2,timestep,stepno,digits,idigits
      integer base,base2
      integer datyp,ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4,nbits
      character*48 variable
      character*80 header
      character*16 nature,stamp,vertcoord,mapproj,form,
     $     date,units,form2
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,rmin,rmax

      integer ier,ierecr,iun,oun
      character*1 imode,omode
      
      real buf(nis,njs,nks)
      
      form2=pform
      if (compress.eq.'OFF'.or.compress.eq.'off') then
         base2 = 10
      else
         base2 = 90
      endif
      
      read(cdigits,*)idigits
      
      if (itsfmode.eq.1) then
         imode = 'a'
         if (tsffile.eq.'bidon') then
            iun = 5
            ier = 0
         else
            iun = 2
            open (iun,file=tsffile,access='SEQUENTIAL',
     $           form='FORMATTED',status='OLD',iostat=ier)
         endif
      else
         imode = 'b'
         iun = 2
         open (iun,file=tsffile,access='SEQUENTIAL',
     $        form='UNFORMATTED',status='OLD',iostat=ier)
      endif


      if (ier.ne.0) then
         print *, '(TSFCAT): Error opening TSF file ', tsffile
         stop
      endif
      
      if (otsfmode.eq.1) then
         omode = 'a'
         oun = 6
      else
         omode = 'b'
         oun = 3
         open (oun,file=obinfile,access='SEQUENTIAL',
     $        form='UNFORMATTED',status='NEW',iostat=ier)
      endif
      
      if (ier.ne.0) then
         print *, '(TSFCAT): Error opening TSF binary file ', obinfile
         stop
      endif
      
      ier = tsfquiet()
      ier = 0
 10   if (ier.ge.0) then
         call tsfmode(imode)
         ier= tsflirf(buf,iun, variable, nature, 
     $        stamp,
     $        nis,njs,nks,ni,nj,nk,level,level2,vertcoord,
     $        date,timestep,stepno,time,time2,
     $        mapproj,xpole,ypole,meshps,maprot,
     $        meshlat,meshlon,swlat,swlon,
     $        rmin,rmax,base,form,digits,units,
     $        datyp,nbits,ipdesc1,ipdesc2,ipdesc3,
     $        mapdesc1,mapdesc2,mapdesc3,mapdesc4)

         if (ier.ge.0) then
            form=form2
            if (base2.eq.90.and.idigits.eq.0) then
               digits=min(4,int(log(2.0**nbits)/log(90.0))+1)
            else
               digits=idigits
            endif
            call tsfmode(omode)
            ierecr= tsfecrf(buf,oun, variable, nature, 
     $           stamp,
     $           nis,njs,nks,ni,nj,nk,level,level2,vertcoord,
     $           date,timestep,stepno,time,time2,
     $           mapproj,xpole,ypole,meshps,maprot,
     $           meshlat,meshlon,swlat,swlon,
     $           rmin,rmax,base2,form,digits,units,
     $           datyp,nbits,ipdesc1,ipdesc2,ipdesc3,
     $           mapdesc1,mapdesc2,mapdesc3,mapdesc4)
         endif
         goto 10
      endif
      
 11   format(A16)
      
      return 
      end
      
      
