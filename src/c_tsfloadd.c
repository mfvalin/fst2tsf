/*
 ***function c_tsfloadd - loads RPN dictionary of variables
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <rpnmacros.h>

int f77name(c_tsfloadd)(varname,varunit,dictlen,dictmax,
                        type,typelen,typemax,lenvar,lenunit,lentype)
int *dictlen,*typelen,*dictmax,*typemax;
int lenvar,lenunit,lentype;
char varname[],varunit[],type[];
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
*     loads RPN dictionary of variables
*
*FILES 
*     inputFile: *FILE de $MC2/data/dict.data
*
*ARGUMENTS 
*     NAMES     I/O  TYPE  A/S  DESCRIPTION
*     varname    O     C    A   dictionary of variables
*     varunit    O     C    A   dictionary of units
*     dictlen    O     I    S   number of entries read
*     dictmax    I     I    S   number of entries allocated
*     type       O     C    A   dictionary of field types
*     typelen    O     I    S   number of entries read
*     typemax    I     I    S   number of entries allocated
*     lenvar     I     I    S   length of variable dictionary item
*     lenunit    I     I    S   length of unit dictionary item
*     lentype    I     I    S   length of type dictionary item
*
*IMPLICIT     
*
*MODULES 
*
**
*/
   char nomdict[128];
   char *mc2;
   FILE *inputFile;
   char car,ligne[128];
   int i,j,it;
   
   mc2 = (char *) getenv("MC2");
   if (mc2 == NULL)
      {
      mc2 = (char *) getenv("ARMNLIB");
      }

   strcpy(nomdict,mc2);
   strcat(nomdict,"/data/dict.data");
   inputFile = fopen(nomdict, "r");

   for (i=0; i < *dictmax; i++)
      {
      for (j=0; j < lenvar; j++)
         {
         varname[i*lenvar+j] = ' ';
         }
      
      for (j=0; j < lenunit; j++)
         {
         varunit[i*lenunit+j] = ' ';
         }
      }
   
   for (i=0; i < *typemax; i++)
      {
      for (j=0; j < lentype; j++)
         {
         type[i*lentype+j] = ' ';
         }
      }
   
   if (inputFile == NULL)
      {
      printf("**********************************************\n");
      printf("**********************************************\n");
      printf("***                                        ***\n");
      printf("\n    Can't open file %s/data/dict.data...\n\n",mc2);
      printf("*** RPN data dictionary not loaded.        ***\n");
      printf("***                                        ***\n");
      printf("**********************************************\n");
      printf("**********************************************\n");
      return 0;
      }

   i = 0;
   it = 0;
   while (!feof(inputFile) && i < *dictmax)
      {
      fgets(ligne,80,inputFile);
      ligne[strlen(ligne)-1] = '\0';
      if (ligne[0] != '-' && ligne[1] != '-')
         {
         for (j=0; j < strlen(ligne); j++)
            {
            if (ligne[j] == '\'')
               ligne[j] = ' ';
            }
         strncpy(&(varname[i*lenvar]),ligne,48);
         strncpy(&(varunit[i*lenunit]),ligne,3);
         
         
         
         if (strlen(ligne) > 52)
            {
            for (j=0; j < 13; j++)
               {
               car = ligne[52+j];
               if (isprint(car) && ((52+j)<strlen(ligne)))
                  {
                  varunit[i*lenunit+3+j]=car;
                  }
               else
                  {
                  varunit[i*lenunit+3+j]=' ';
                  }
               }
            }
         else
            {
            strncpy(&(varunit[i*lenunit+3]),"            ",13);
            }
         i++;
         }
      else
         {
         type[it*lentype] = ligne[2];
         type[it*lentype+1] = ' ';
         for (j=0; j < 14; j++)
            {
            if (isprint(ligne[6+j]) && ((6+j)<strlen(ligne)))
               {
               type[it*lentype+2+j]=ligne[6+j];
               }
            else
               {
               type[it*lentype+2+j]=' ';
               }
            }
         it++;
         }
      }
   
   *dictlen = i-1;
   *typelen = it -1;

   fclose(inputFile);
   return 0;

   }

