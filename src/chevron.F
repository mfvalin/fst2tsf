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
***s/p p2f -Converts a TSF file into an RPN standard file
*
      subroutine chevron
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Converts a TSF file info a random RPN standard file.
*
*FILES 
*     Unit 2: TSF file (when -tsf option is used)
*     Unit 5: TSF file (when redirection of i/o is used)
*     Unit 1: FST file
*
*ARGUMENTS 
*
*IMPLICIT     
#include "rogntudju.cdk"
*
*MODULES
      integer  tsfverb,tsflirfp,tsflirfd,fnom,fstouv,fstecr,fstfrm
      external ccard,tsfverb,fnom,tsflirfp,tsflirfd,
     $     memoirh,tsf2fst,fstfrm
**
      integer nis,njs,nks
      character*128 cle(2),def(2),val(2)

      integer ni,nj,nk,time,time2,timestep,stepno,base
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,mc2date,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max

      real xlat0,xlon0,dlat,dlon
      integer i,j
      real buf(25000)
      character*1 typvar,grtyp
      character*2 nomvar
      character*8 etiket
      character*80 header
      integer deet, npas, nbits, datyp, ier 
      integer ip1, ip2, ip3 
      integer ig1, ig2, ig3, ig4
      integer date,ipos
      integer ipdesc1,ipdesc2,ipdesc3
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4
      
      integer ibuf,idigits,iunread
      
      data cle /'fst.','tsf.'/
      data def /'scrap','scrap'/
      data val /'scrap','scrap'/
      
      call ccard(cle,def,val, 2, ipos)
      
      ier = fnom(1,val(1),'RND',0)
      ier = fstouv(1, 'RND')
      
      xlat0 = 42.5
      xlon0 = 2.62
      dlat  = 0.005
      dlon  = 0.005

      nbits=-24
      nomvar='>>'
      typvar='C'
      etiket='CHEVRON'
      date = 0
      ip1 = 0
      ip2 = 0
      ip3 = 1
      

      deet  = 0
      npas  = 0
      ig1   = 100
      ig2   = 100
      ig3   = 0
      ig4   = 0
      grtyp ='L'
      ni = 1972
      nj = 1200

      do i=1,1972
         buf(i) = xlon0 + (i-1)*dlon
      enddo
      
      
      ier = fstecr(buf,buf,nbits,1,date,deet,npas,
     $     ni,1,1,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,
     $     ig1,ig2,ig3,ig4,datyp,.false.)
         
      
      do i=1,1200
         buf(i) = xlat0 + (i-1)*dlat
      enddo
      
      
      ier = fstecr(buf,buf,nbits,1,date,deet,npas,
     $     1,nj,1,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,
     $     ig1,ig2,ig3,ig4,datyp,.false.)
         
      
      ier=fstfrm(1)

 11   format(A16)
      
      stop 
      end
      
      
      
