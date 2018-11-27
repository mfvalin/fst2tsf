***s/p tsfdemo -sample decoding of a TSF file
*
      program tsfdemo
#include "impnone.cdk"

*
*AUTHOR   Yves Chartier                      July 1993
* 
*REVISION
*
*LANGUAGE:  fortran 77
*
*OBJECT
*     Sample decoding of a TSF file.
*     This program reads each record of a TSF file, 
*     and computes its arithmetic average 
*
*FILES 
*     Unit 5: TSF file (redirection of i/o is assumed)
*             i.e 'tsfdemo < tsffile'
*
*ARGUMENTS 
*
*IMPLICIT     
*
*MODULES 
      integer tsflir
      external tsflir
*
**

      integer nis,njs,nks
      parameter (nis=512)
      parameter (njs=512)
      parameter (nks=1)
      
      
      integer ni,nj,nk,time,time2,timestep,stepno,digits,base
      character*48 variable
      character*16 nature,stamp,vertcoord,mapproj,form,date,units
      real level,level2,xpole,ypole,meshps,maprot,
     $     meshlat,meshlon,swlat,swlon,rmin,rmax

      integer i,j,k,ier
      
      real buf(nis,njs,nks)
      real sum,average
      
      ier = tsfmode('A')
      ier = 0
 10   if (ier.ge.0) then
         ier= tsflir(buf,5, variable, nature, stamp,
     $        nis,njs,nks,ni,nj,nk,level,level2,vertcoord,
     $        date,timestep,stepno,time,time2,
     $        mapproj,xpole,ypole,meshps,maprot,
     $        meshlat,meshlon,swlat,swlon,
     $        rmin,rmax,base,form,digits,units)

         sum = 0.0
         do 25 k=1,nk
            do 25 j=1,nj
               do 25 i=1,ni
                  sum = sum + buf(i,j,k)
 25      continue
         average = sum/(ni*nj*nk)

         print *, '**************************************************'
         print *
         print 100,variable(1:2),level,date,average
         print *
         print *, '**************************************************'
         goto 10
      endif
 100  format(' ','Variable: ',a4,'Level: ',f9.3,
     $     'Date: ',a18,'Average: ',g15.6)
      stop
      end
      
      
