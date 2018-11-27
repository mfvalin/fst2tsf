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
***function tsfnext
*
      integer function tsfnext(frbuf,fniun,fcvar,fcnature,fcstamp,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,frlevel,frlevel2,fcvcoord,
     $     fcdate,fntimestep,fnstepno,fntime,fntime2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fnextra,fnnextra)
#include "impnone.cdk"
      
      integer fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,fniun,fntimestep,
     $    fnstepno,fntime,fntime2,fnbase,fndigits,fnextra(*),fnnextra
      
      character*48 fcvar 
      character*16 fcstamp, fcnature, fcvcoord, 
     $     fcmapproj,fcform,fcdate,fcunits
      character*8 fcaction
      real frbuf(fnnis,fnnjs,fnnks), frlevel,frlevel2,
     $     frxpole,frypole,frmeshps,
     $     frmaprot,frmeshlat,frmeshlon,frswlat,frswlon,frmin,frmax
      
*OBJECT
*     enhanced version of tsflir
*     contains selection criteria
*     decodes a record coded in TSF data format
*     
*METHOD
*     
*EXTERNALS
*     
*AUTHORY. Chartier	                Jun/July  1993
*     
*HISTORY
**    
**    
      character*48 var 
      character*16 stamp, nature, date
      real level1,level2
      integer time1,time2

      character*48 var2 
      character*16 stamp2, nature2, date2
      real level12,level22
      integer time12,time22

      save var,stamp,nature,date,level1,level2,time1,time2

      integer tsflir,tsfquiet,tsfverb,tsfinfo,tsfsel,ier,tsfbksp
      external tsflir,tsfquiet,tsfverb,tsfinfo
      character*8 action

      logical varok,naturok,stampok,level1ok,level2ok,
     $     dateok,time1ok,time2ok

      
      ier=tsfquiet()
 15   ier= tsflir(frbuf,fniun,var2,nature2,stamp2,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,level12,level22,fcvcoord,
     $     date2,fntimestep,fnstepno,time12,time22,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fnextra,fnnextra)
      
      if (ier.lt.0) then
         ier=tsfverb()
         tsfnext = -1
         return
      endif


      varok = .false.
      naturok = .false.
      stampok = .false.
      level1ok = .false.
      level2ok = .false.
      dateok = .false.
      time1ok = .false.
      time2ok = .false.

      if (var(1:2).eq.' '.or.var(1:2).eq.var2(1:2)) varok = .true.
      if (nature(1:1).eq.' '.or.nature(1:1).eq.nature2(1:1)) 
     $     naturok = .true.
      if (stamp(1:8).eq.' '.or.stamp(1:8).eq.stamp2(1:8)) 
     $     stampok = .true.
      if (level1.eq.-1.0.or.level1.eq.level12) level1ok = .true.
      if (level2.eq.-1.0.or.level2.eq.level22) level2ok = .true.
      if (date.eq.' '.or.date.eq.date2) dateok = .true.
      if (time1.eq.-1.or.time1.eq.time12) time1ok = .true.
      if (time2.eq.-1.or.time2.eq.time12) time2ok = .true.
      
      if (varok.and.naturok.and.stampok.and.level1ok.and.level2ok.and.
     $     dateok.and.time1ok.and.time2ok) then
         ier= tsfinfo(var2, nature2, stamp2,
     $        fnni,fnnj,fnnk,level12,level22,fcvcoord,
     $        date2,fntimestep,fnstepno,time12,time22,
     $        fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $        frmeshlat,frmeshlon,frswlat,frswlon,
     $        frmin,frmax,fnbase,fcform,fndigits,fcunits)
         
         fcvar = var2
         fcnature = nature2
         fcstamp = stamp2
         frlevel = level12
         frlevel2=level22
         fcdate = date2
         fntime = time12
         fntime2= time22
         
         tsfnext = 0
         return
      else
         if (action.eq.'CONTINUE') then
            goto 15
         else
            ier = tsfbksp(fniun)
            tsfnext = -1
         endif
      endif
      
      ier=tsfverb()
      return

      entry tsfsel(fcvar,fcnature,fcstamp,
     $     frlevel,frlevel2,
     $     fcdate,fntime,fntime2,fcaction)
      
      var = fcvar
      stamp = fcstamp
      nature = fcnature
      date = fcdate
      level1 = frlevel
      level2 = frlevel2
      time1  = fntime
      time2  = fntime2

      call low2up(fcaction,action)
      
      return
      end
      

