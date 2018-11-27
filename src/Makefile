
ifeq "$(BASE_ARCH)" "$(EC_ARCH)"
$(error ERROR: EC_ARCH is equal to BASE_ARCH, no compiler architecture is defined, ABORTING)
endif

.SUFFIXES : 

.SUFFIXES : .f .c .o .F

SHELL = /bin/bash

FFLAGS = 

CFLAGS = 

RMNLIB = rmn

.c.o:
	s.cc -c $(CFLAGS) $<

.F.o:
	s.f90 -c $(FFLAGS) $<

OBJECTS= \
         chevron.o       datf2p.o \
         datp2f.o       difdatsd.o       \
         f2pptch.o       fst2pdf.o       fst2tsf.o       iminmax.o \
         incdatsd.o       incdatss.o     jdate.o \
         p2fptch.o       pdf2fst.o \
         pdfbksp.o       pdfcdate.o        \
         pdfdictp.o       pdfdictt.o       pdfdictu.o       pdfdictv.o \
         pdfgcom.o       pdfincd.o \
         pdfinfo.o       tsfinfotoggle.o pdfjdate.o     pdfpcom.o       pdfprm.o \
         pdfquiet.o       pdfrd90.o       pdfrd90i.o       pdfrdrec.o \
         pdfrirec.o       pdfverb.o       pdfwirec.o       pdfwr90.o \
         pdfwr90i.o       pdfwrrec.o       prsdate.o       rminmax.o \
         tsf2fst.o       tsfbksp.o       tsfcat.o       tsfcdate.o \
         tsfdictp.o       tsfdictt.o       tsfdictu.o \
         tsfdictv.o       tsfecr.o       tsfecrf.o       tsfgcom.o \
         tsfincd.o       tsfinfo.o       tsfjdate.o       tsflir.o \
         tsflirf.o       tsflirfd.o       tsflirfp.o       tsflirp.o \
         tsflirs.o       tsfloadd.o       tsfmode.o       tsfnext.o \
         tsfpcom.o       tsfprm.o       tsfquiet.o       tsfrd90.o \
         tsfrd90i.o       tsfrdrec.o       tsfrirec.o       tsfverb.o \
         tsfwirec.o       tsfwr90.o       tsfwr90i.o       tsfwrrec.o \
	 c_tsfcat.o c_tsfloadd.o

all: libtsf.a fst2tsf2000  tsf2fst2000  

libtsf.a: $(OBJECTS)
	ar rc libtsf.a *.o

fst2tsf2000: libtsf.a f2p.F
	s.f90  f2p.F -L. -ltsf -o f2p -l$(RMNLIB) -o fst2tsf2000

tsf2fst2000: libtsf.a  p2f.F
	s.f90 p2f.F -o p2f -L. -ltsf  -l$(RMNLIB) -o tsf2fst2000

obj: $(OBJECTS)

install: fst2tsf2000  tsf2fst2000
	mv fst2tsf2000 ../bin/.
	mv tsf2fst2000 ../bin/.

clean:
	/bin/rm -f *.f *.o fst2tsf2000 tsf2fst2000 tsfcat libtsf.a
