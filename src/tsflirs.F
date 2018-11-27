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
***function tsflirs -version of tsflir with selection criteria
*
      integer function tsflirs(frbuf,fniun,fcvar,fcnature,fcstamp,
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
*AUTHOR     Y. Chartier	                Jun/July  1993
*
*HISTORY
**
**    
      character*48 var 
      character*16 stamp, nature, date
      real level1,level2
      integer time1,time2

      integer tsflir,tsfquiet,tsfverb,tsfinfo,ier
      external tsflir,tsfquiet,tsfverb,tsfinfo

      logical varok,naturok,stampok,level1ok,level2ok,
     $     dateok,time1ok,time2ok
      
      ier=tsfquiet()
 15   ier= tsflir(frbuf,fniun,var,nature,stamp,
     $     fnnis,fnnjs,fnnks,fnni,fnnj,fnnk,level1,level2,fcvcoord,
     $     date,fntimestep,fnstepno,time1,time2,
     $     fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $     frmeshlat,frmeshlon,frswlat,frswlon,
     $     frmin,frmax,fnbase,fcform,fndigits,fcunits,
     $     fnextra,fnnextra)
      
      if (ier.lt.0) then
         ier=tsfverb()
         tsflirs = -1
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

      if (fcvar(1:2).eq.' '.or.fcvar(1:2).eq.var(1:2)) varok = .true.
      if (fcnature(1:1).eq.' '.or.fcnature(1:1).eq.nature(1:1)) 
     $     naturok = .true.
      if (fcstamp(1:8).eq.' '.or.fcstamp(1:8).eq.stamp(1:8)) 
     $     stampok = .true.
      if (frlevel.eq.-1.0.or.frlevel.eq.level1) level1ok = .true.
      if (frlevel2.eq.-1.0.or.frlevel2.eq.level2) level2ok = .true.
      if (fcdate.eq.' '.or.fcdate.eq.date) dateok = .true.
      if (fntime.eq.-1.or.fntime.eq.time1) time1ok = .true.
      if (fntime2.eq.-1.or.fntime2.eq.time2) time2ok = .true.

      if (varok.and.naturok.and.stampok.and.level1ok.and.level2ok.and.
     $     dateok.and.time1ok.and.time2ok) then
         tsflirs = 0
         ier= tsfinfo(var, nature, stamp,
     $        fnni,fnnj,fnnk,level1,level2,fcvcoord,
     $        date,fntimestep,fnstepno,time1,time2,
     $        fcmapproj,frxpole,frypole,frmeshps,frmaprot,
     $        frmeshlat,frmeshlon,frswlat,frswlon,
     $        frmin,frmax,fnbase,fcform,fndigits,fcunits)
         return
      else
         goto 15
      endif
      
      ier=tsfverb()
      return
      end
      

