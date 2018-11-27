/*
***function c_tsfcat -C front end to FORTRAN routine TSFCAT
*/

#include <stdio.h>
#include <string.h>
#include <rpnmacros.h>

void f77name(tsfcat)(char *tsfFile, int *ibinmode, char *obinfile, int *obinmode, char *format, char *digits, char *compress,
		     int len_tsfFile, int len_obinfile, int len_compress, int len_digits, int len_format);

void c_ccard(char **argv, int argc, char **liste, char *val, char **def, int n, int *npos) ;

int f77name(c_tsfcat)(int argc, char **argv)
{
  /*
*
*AUTHOR   Yves Chartier                      October 1993
* 
*REVISION
*
*LANGUAGE:  C
*
*OBJECT
*     C front end to FORTRAN routine TSFCAT
*     used to get calling arguments
*
*FILES 
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     argc       I     I    S   number of arguments
*     argv       I     C    A   argument list
*
**
*/
   int i,ibinmode,obinmode;
   char tsfFile[128],ibinfile[128],obinfile[128];
   char compress[8];
   char digits[8];
   char format[32];
   int ok;

   char *liste[6], *def[6], val[6][256]; 
   int n, npos, nptsRLE;
   FILE *in;
   
   liste[0] = "c.";
   liste[1] = "format.";
   liste[2] = "digits.";
   liste[3] = "ibin.";
   liste[4] = "obin.";
   liste[5] = "tsf.";

   
   def[0] = "on";
   def[1] = "(5g14.6)";
   def[2] = "0";
   def[3] = "ibidon";
   def[4] = "obidon";
   def[5] = "bidon";

   strcpy(val[0],"off");
   for (i=1; i < 6; i++)
      {
      strcpy(val[i],def[i]);
      }

   c_ccard(argv,argc,liste,&val[0][0],def,6,&npos);

   strcpy(digits,  val[2]);
   strcpy(tsfFile, val[5]);
   strcpy(ibinfile,val[3]);
   strcpy(obinfile,val[4]);
   strcpy(format,  val[1]);
   strcpy(compress,val[0]);

   if (0 != strcmp(ibinfile,def[3]))
      {
      ibinmode = 0;
      strcpy(tsfFile, val[3]);
      }
   else
      {
      ibinmode = 1;
      }

   if (0 != strcmp(obinfile,def[4]))
      {
      obinmode = 0;
      }
   else
      {
      obinmode = 1;
      }

   for (i=strlen(tsfFile);  i < 128; i++) tsfFile[i]  = ' ';
   for (i=strlen(ibinfile); i < 128; i++) ibinfile[i] = ' ';
   for (i=strlen(obinfile); i < 128; i++) obinfile[i] = ' ';
   for (i=strlen(format);   i < 32;  i++) format[i]   = ' ';
   for (i=strlen(digits);   i < 8;   i++) digits[i]   = ' ';
   for (i=strlen(compress); i < 8;   i++) compress[i] = ' ';

   
   f77name(tsfcat)(tsfFile,&ibinmode,obinfile,&obinmode,format,
                   digits,compress,128,128,32,8,8);
   return 0;
   }

