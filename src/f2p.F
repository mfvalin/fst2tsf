***s/p f2p
*
      program f2p
#include "impnone.cdk"
*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT (f2p)
*     This module is the main part of a program that 
*     converts 1 or more (up to 40) RPN standard files 
*     to the TSF file format.
*
*FILES
*     tape1: TSF file
*     tape10-49: RPN standard files 
*
*ARGUMENTS 
*
*IMPLICIT     
#include "tsf.cdk"
#include "rogntudju.cdk"
*
*MODULES
      external ccard,fstlnk,fstinf,fstprm,fstluk,fst2tsf,
     $    tsfdictu,tsfdictv,tsfdictt,tsfdictp,iminmax,rminmax,
     $    tsfecrf,fstsui
      
      
      integer dictmax,typemax
      parameter (dictmax = 180)
      parameter (typemax = 20)
      integer dictlen,typelen

      character*48 varname(dictmax)
      character*16 varunit(dictmax)
      character*16 type(typemax)

      character*128 cle(46),def(46),val(46)
      data cle /40*'fst.','tsf.','format.','c','digits','bin','info'/
      data def /40*'scrap','scrap','(5g14.6)','on' ,'0','bin','short'/
      data val /40*'scrap','scrap','(5g14.6)','off','0','ascii','long'/
      

      integer fnom,ier,fstouv,fstprm,fstinf,fstsui,fstluk,fstopi
      integer tsfecrf,tsfverb,tsfquiet
      
      character*16 mc2date
      integer i,ipos,nf
      
      integer ni,nj,nk,base,stepno,time,time2,timestep
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,rmin,rmax
      integer imin,imax

      equivalence (rmin,imin)
      equivalence (rmax,imax)

      
      character*1 typvar,grtyp
      character*2 nomvar
      character*8 etiket

      character*80 header
      integer key, date0, deet, npas, nbits, datyp 
      integer ip1, ip2, ip3, swa, lng, dltf, ubc
      integer ig1, ig2, ig3, ig4, extra1, extra2, extra3
      integer ipdesc1,ipdesc2,ipdesc3 
      integer mapdesc1,mapdesc2,mapdesc3,mapdesc4
      
      integer lnkdiun(40)
      data lnkdiun /10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
     *     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     *     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
     *     41, 41, 42, 43, 44, 45, 46, 47, 48, 49 /
      
*     real buf(1)
      real, allocatable, dimension(:) :: buf
      integer ibuf,idigits,iunwrite
      
      ier = fstopi('MSGLVL', 10, .false.)
      call ccard(cle,def,val, 46, ipos)
      
      form=val(42)
      if (val(43).eq.'OFF'.or.val(43).eq.'off') then
         base = 10
      else 
         base = 90
      endif

      if (val(45).eq.'BIN'.or.val(45).eq.'bin') then
         call tsfmode('b')
      else
         call tsfmode('a')
      endif


      read (val(44),*)idigits
      
      nf = 1
 33   if (val(nf).ne.def(nf).and.nf.le.40) then
         nf = nf +1
         goto 33
      endif
      
      if (val(41).eq.'scrap'.and.qdfmode.ne.0) then
         iunwrite = 6
         ier = tsfquiet()
      else
         iunwrite = 1
	 ier = tsfverb()
         if (qdfmode.eq.1) then
            ier = fnom(iunwrite,val(41),'FMT+SEQ+R/W',0)
         else
            ier = fnom(iunwrite,val(41),'FTN+R/W',0)
         endif
      endif
      
      call tsfinfotoggle(val(46))

      nf = nf -1
      do 34 i=1, nf
         ier = fnom(lnkdiun(i),val(i),'RND+OLD+R/O',0)
         if (ier.lt. 0) then
            print *, '************************************************'
            print *, ' probleme avec fichier ',val(i),' inexistant - ' 
            print *, '************************************************'
            stop
         endif
 34   continue
      
      do 35 i=1,nf
         ier = fstouv(lnkdiun(i), 'RND')
         if (ier.lt.0) then
            print *, '**********************************************'
            print *, '* le fichier #',val(i), 
     *           'n''est pas standard random'
            print *, '**********************************************'
            stop
         endif
 35   continue
      call fstlnk(lnkdiun, nf)   
      key = fstinf(lnkdiun(1), ni, nj, nk,  -1, ' ', 
     $     -1, -1, -1, ' ', ' ')


      call tsfloadd(varname,varunit,dictlen,dictmax,
     $              type,typelen,typemax)


 10   if (key.ge.0) then
*        call memoirh(buf,ibuf,ni*nj*nk)
         ibuf = 1
         allocate(buf(ni*nj*nk+10))
         ier = fstprm(key, date0, deet, npas, ni, nj, nk, nbits,
     *        datyp, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, 
     *        ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, 
     *        extra1, extra2, extra3)
         ier = fstluk(buf(ibuf),key,ni,nj,nk)

         call fst2tsf(variable,nature,stamp,level,level2,vertcoord, 
     $        mc2date,timestep,stepno,time,time2,
     $        mapproj,xpole,ypole,meshps,
     $        maprot,meshlat,meshlon,swlat,swlon,
     $        ipdesc1,ipdesc2,ipdesc3,
     $        mapdesc1,mapdesc2,mapdesc3,mapdesc4,
     $        nomvar,typvar,etiket,date0,deet,npas,
     $        ip1,ip2,ip3,grtyp,ig1,ig2,ig3,ig4)

         call tsfdictu(units,variable(1:2),varunit,dictlen)
         call tsfdictv(variable,varname,dictlen)
         call tsfdictt(nature,type,typelen)
         call tsfdictp(mapproj,mapproj)

         if (datyp.eq.2.or.datyp.eq.4) then
            call iminmax(imin,imax,buf(ibuf),ni,nj,nk,ni,nj,nk)
            rmin=real(imin)
            rmax=real(imax)
         else
            call rminmax(rmin,rmax,buf(ibuf),ni,nj,nk,ni,nj,nk)
         endif
         
         if (base.eq.90) then
            if (idigits.eq.0) then
               idigits=min(4,int(log(2.0**nbits)/log(90.0))+1)
            endif
            form='*'
         endif

         ier=tsfecrf(buf(ibuf),iunwrite,variable,nature,stamp,
     $        ni,nj,nk,ni,nj,nk,level,level2,vertcoord,
     $        mc2date,timestep,stepno,time,time2,
     $        mapproj,xpole,ypole,
     $        meshps,maprot,
     $        meshlat,meshlon,swlat,swlon,rmin,rmax,
     $        base,form,idigits,units,datyp,nbits,
     $        ipdesc1,ipdesc2,ipdesc3,
     $        mapdesc1,mapdesc2,mapdesc3,mapdesc4)
*        call memoirh(buf,ibuf,0)
         deallocate(buf)
         key = fstsui(lnkdiun(1),ni,nj,nk)
         goto 10
      endif
      
 11   format(A16)
      
      stop 
      end
      
      
