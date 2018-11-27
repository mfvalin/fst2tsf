***s/p p2f -Converts a TSF file into an RPN standard file
*
      program p2f
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
     $     tsf2fst,fstfrm
**
      integer nis,njs,nks
      character*128 cle(2),def(2),val(2)

      integer ni,nj,nk,time,time2,timestep,stepno,base
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,mc2date,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max

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
      
      real, allocatable, dimension(:) :: buf
      integer ibuf,idigits,iunread
      
      data cle /'fst.','tsf.'/
      data def /'scrap','scrap'/
      data val /'scrap','scrap'/
      
      call ccard(cle,def,val, 2, ipos)
      
      ier = fnom(1,val(1),'RND',0)
      ier = fstouv(1, 'RND')
      
      if (val(2).eq.'scrap') then
         iunread = 5
         ier = tsfverb()
         call tsfmode('a')
      else
         iunread = 2
*        call tsfmode('b')
*        open (iunread,file=val(2),access='SEQUENTIAL',
*    $        form='UNFORMATTED',status='OLD',iostat=ier)
*        if (ier.ne.0) then
*           close(iunread)
            call tsfmode('a')
            ier = fnom(iunread,val(2),'FMT+SEQ+OLD+R/O',0)
*           rewind(iunread)
*        endif
      endif
      
 10   ier=tsflirfp(iunread,variable,nature,stamp,
     $     ni,nj,nk,level,level2,vertcoord,
     $     mc2date,timestep,stepno,time,time2,
     $     mapproj,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,min,max,
     $     base,form,idigits,units,datyp,nbits,
     $     ipdesc1,ipdesc2,ipdesc3,
     $     mapdesc1,mapdesc2,mapdesc3,mapdesc4)
      
      nis=ni
      njs=nj
      nks=nk
      
      if (ier.ge.0) then
!          call memoirh(buf,ibuf,ni*nj*nk)
         allocate(buf(ni*nj*nk))
         ibuf = 1
         
         ier=tsflirfd(buf(ibuf),iunread,nis,njs,nks,ni,nj,nk)
         call tsf2fst(nomvar,typvar,etiket,date,deet,npas,
     $        ip1,ip2,ip3,grtyp,
     $        ig1,ig2,ig3,ig4,
     $        variable, nature, stamp,mc2date,timestep,
     $        stepno,time,time2,level,level2,vertcoord, 
     $        mapproj,xpole,ypole,meshps,maprot,
     $        meshlat,meshlon,swlat,swlon,
     $        ipdesc1,ipdesc2,ipdesc3,
     $        mapdesc1,mapdesc2,mapdesc3,mapdesc4)
         
         if (nbits.lt.0) then
            nbits = -nbits
         endif
         
         ier = fstecr(buf(ibuf),buf(ibuf),-nbits,1,date,deet,npas,
     $        ni,nj,nk,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,
     $        ig1,ig2,ig3,ig4,datyp,.false.)
         
!          call memoirh(buf,ibuf,0)
         deallocate(buf)
         goto 10
      endif
      
      ier=fstfrm(1)

 11   format(A16)
      
      stop 
      end
      
      
      
