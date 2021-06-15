/*
 * BHTrim.c
 * Version 2.01   8/28/90
 * (c) 1990 InfoTech. Inc
 * Craig Fitzgerald
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys\types.h>
#include <sys\stat.h>
#include "csv.h"





/* the following strings are used in the init file */
#define ENTRYID        "BHTrim"
#define KEYFILEVAR     "KeyFile"
#define OUTFILEVAR     "OutFile"
#define AREASVAR       "Areas"
#define KEYPATH        "KeyPath"
#define OUTPATH        "OutPath"
#define DEFKEYFILE     "DefKeyFile"
#define DEFOUTFILE     "DefOutFile"
#define KEYSIZE        "KeySize"
#define INICOMMENTCHAR ';'


/* the following strings are used in the csv source file */
#define COUNTIES       "COUNTIES"
#define BIDCAT         "BIDCAT"
#define BIDCATELEMENT  "BIDCATELEMENT"
#define BIDAVERAGE     "BIDAVERAGE"
#define BIDREGRESSION  "BIDREGRESSION"
#define AREATYPE       "AREATYPE"

#define HighEstName    "HIGHEST"
#define HighEstVer     "1.01"
#define Header3        "CSV"
#define Header4        "CATALOG"
#define Header5        "SUPPLEMENT"


#define MaxOpenFiles   512
#define MemClearance   8192
#define StrSize        80
#define BigStrSize     1024

#define OK             0
#define OutOfHandles   1
#define OutOfMem       2
#define NoFilesFound   3
#define ReachedLimit   4
#define NoEntrys       1
#define Error          1

#define BOOL           int
#define FALSE          0
#define TRUE           1
#define ALNUM          0
#define SPACE          1

#define ALL            -1
#define ACTIVE         -2
#define ELEMENTS        0
#define ITEMS           1


typedef char  *PSZ;


typedef struct OutFileRec
         {  PSZ     pszAreaPtr;
            PSZ     pszDistName;
            FILE    *fpOutFile;
            fpos_t  fpIPos;
            fpos_t  fpEPos;
            int     iICount;
            int     iECount;
            PSZ     pszKeyBuff;
            int     iKeyCount;
            BOOL    bActive;
         }  OFR;


BOOL bCountiesProcessed = FALSE;
BOOL bBidCatProcessed;
BOOL bPrintHeader;


/*
 * this is the default key size for the item number keys.
 * If any state uses an item number larger than 12 chars,
 * this number will need to be altered accordingly.
 */

int   KeySize           = 12 + 1;
BOOL  KeySizeRedefined  = FALSE;


/***********************************************************************/
/*                         Utility Procedures                          */
/***********************************************************************/


/*
 * Con message displayed if parameters # 2
 *
 */
void Message (void)
   {
   printf ("\nUSAGE:  BHTRIM  [InitFile]  DataFile\n\n");
   printf ("WHERE:  InitFile contains info about the district files to be generated.\n");
   printf ("          (InitFile defaults to BHTRIM.INI if not specified)\n");
   printf ("        DataFile contains the Bid History source data.\n\n");
   printf ("        See the InitFile BHTRIM.INI for more information and examples.\n");
   exit (1);
   }



/*
 * Std error proc.
 *
 */
void Abort (PSZ szMsg, PSZ p1, PSZ p2)
   {
   printf ("\n Error: ");
   printf (szMsg, p1, p2);
   printf ("\n\n<Run Aborted>\n");
   exit (1);
   }


/*
 * Eats blanks from input file fp. Will also eat newlines
 * unless iMode = SPACE
 *
 */
char SkipBlanks (FILE *fp, int iMode)
   {
   char  c;

   while ((c = (char) getc (fp)) == ' ' || (c == '\n'&& iMode != SPACE))
      ;
   ungetc (c, fp);
   return c;
   }


/*
 * Strips trailing blanks from strings.
 *
 */
PSZ StripTBlanks (PSZ pszStr)
   {
   int   i;

   for (i = strlen (pszStr) - 1; i >= 0 && pszStr[i] == ' ' ; i--)
      pszStr[i] == '\0';
   return pszStr;
   }




/*
 * Checks for sufficient memory.
 * Return values:: OK, OutOfMem 
 */

int IsMem (int iSize)
   {
   void  *vMemPtr;

   if ((vMemPtr = malloc (iSize + MemClearance)) == NULL)
      return OutOfMem;
   free (vMemPtr);
   return OK;
   }






/*
 * Reads a line from input file fp.
 * Return values::: Error on EOF , OK if ok.
 * This proc reads a line into szStr, the file line is clipped if it
 * cannot fit into the str.
 * if iMode = SPACE, then this proc will not skip over line boundrys
 */
int ReadLine (FILE *fp, PSZ pszStr, int iMaxLen, int iMode)
   {
   char  c;

   SkipBlanks (fp, iMode);
   while (--iMaxLen > 0 && (c = (char) getc (fp)) != '\n' && c != EOF )
      *(pszStr++) = c;
   *pszStr = '\0';
   StripTBlanks (pszStr);
   while ( c != '\n' && c != EOF)
      c = (char) getc (fp);
   return  (c == EOF);
   }



/*
 * Reads delimeted word from input file fp.
 * Return values::: Error on EOF, OK if ok.
 * This proc reads a word into szStr
 * leading spaces are stripped
 * iModes: ALNUM = non alphanumeric is delimeter
 *         SPACE = whitespace is delemiter
 */
int ReadWord (FILE *fp, PSZ pszStr, int iMode)
   {
   char  c;

   SkipBlanks (fp, OK);
   while (   (iMode == ALNUM &&  isalnum (c = (char) getc (fp)))  ||
             (iMode == SPACE &&
              !isspace (c = (char) getc (fp)) && c != EOF))
      *(pszStr++) = c;
   *pszStr = '\0';
   StripTBlanks (pszStr);
   ungetc (c, fp);
   return  (c == EOF);
   }



/*
 * Next-char trap.
 * Returns OK, Error
 */
int SkipChar (FILE *fp, char ch)
   {
   char  c;

   while ((c = (char) getc (fp)) == ' ')
      ;
   return (c != ch);
   }




/* this resets the file pointer to the beginning of the file */
int NumLines (FILE *fpIn)
   {
   int   i;
   char  c;
   
   for (i = 0; (c = (char) getc (fpIn)) != EOF; i += !! (c == '\n'))
      ;
   rewind (fpIn);
   return i;
   }





/*
 * Eats the rest of a line.
 * Returns OK, Error on eof.
 */
int SkipToEOL (FILE *fp)
   {
   char  c;

   while ((c = (char) getc (fp)) != '\n' && c != EOF)
      ;
   return (c == EOF);
   }



/*
 * This proc echos a string to the output files.
 * if iMode = ALL,     pszStr is echoed to all open   outfiles, 
 * if iMode = ACTIVE,  pszStr is echoed to all active outfiles,
 * else                pszStr is echoed to the iMode'th outfile 
 */
void EchoWord (OFR* sOutRec, int iRecCount, PSZ pszStr, int iMode)
   {
   if ((iMode == ALL) || (iMode == ACTIVE))
      {
      for (iRecCount--; iRecCount >= 0; iRecCount--)
         {
         if ((iMode == ALL) ||
             (iMode == ACTIVE && sOutRec[iRecCount].bActive))
            fprintf (sOutRec[iRecCount].fpOutFile, pszStr);
         }
      }
   else
      fprintf (sOutRec[iMode].fpOutFile, pszStr);
   }





/*
 * This proc echos a line to the output files.
 * if iMode = ALL,     pszStr is echoed to all open   outfiles, 
 * if iMode = ACTIVE,  pszStr is echoed to all active outfiles,
 * else                pszStr is echoed to the iMode'th outfile 
 */
void EchoLine (OFR* sOutRec, int iRecCount, PSZ pszStr, int iMode)
   {
   EchoWord (sOutRec, iRecCount, pszStr, iMode);
   EchoWord (sOutRec, iRecCount, "\n", iMode);
   }



/*
 * The current file positions are saved in the descriptor record.
 * if iMode = ELEMENTS, the pos is saved in ->fpPos
 * if iMode = ITEMS,    the pos is saved in ->fpIPos
 */
void SavePositions (OFR* sOutRec, int iRecCount, int iMode)
   {
   for (iRecCount--; iRecCount >= 0; iRecCount--)
      if (iMode == ELEMENTS)
         fgetpos (sOutRec[iRecCount].fpOutFile, &sOutRec[iRecCount].fpEPos);
      else
         fgetpos (sOutRec[iRecCount].fpOutFile, &sOutRec[iRecCount].fpIPos);
   }





/*
 * This procedure backes an output file to a previously saved position
 * and writes a string, it then restores the files position 
 * if iMode = ELEMENTS,  fpEPos is used for the pointer &
 *                       iECount is used for the data                              
 * if iMode = ITEMS,     fpIPos is used for the pointer on active elements
 *                       iICount is used for the data                              
 */
void WriteAtPositions  (OFR* sOutRec, int iRecCount, int iMode)
   {
   char      szStr [StrSize];
   fpos_t    fpTmpPtr;

   for (iRecCount-- ; iRecCount >= 0; iRecCount--)
      {
      if (iMode == ELEMENTS || sOutRec[iRecCount].bActive)
         {
         fgetpos (sOutRec[iRecCount].fpOutFile, &fpTmpPtr);
         if (iMode == ELEMENTS)
            {
            fsetpos (sOutRec[iRecCount].fpOutFile,&sOutRec[iRecCount].fpEPos);
            itoa (sOutRec[iRecCount].iECount, szStr, 10);
            }
         else
            {
            fsetpos (sOutRec[iRecCount].fpOutFile,&sOutRec[iRecCount].fpIPos);
            itoa (sOutRec[iRecCount].iICount, szStr, 10);
            }
         fprintf (sOutRec[iRecCount].fpOutFile, szStr);
         fsetpos (sOutRec[iRecCount].fpOutFile, &fpTmpPtr);
         }
      }         
   }



/*
 * Word trap.
 * Aborts on error cond.
 *
 */
void Test (PSZ pszCSV, int i, PSZ pszCmpStr)
   {
   char  szStr      [StrSize];

   GetCSVField (i, pszCSV, szStr);
   if (stricmp (szStr, pszCmpStr) != OK)
      Abort ("Error in input data file: %s found where %s was expected", szStr, pszCmpStr);
   }




/*
 * Returns TRUE if pszStr is in the AREA list
 *
 */
BOOL StrInCSVList (OFR OutRec, PSZ pszStr)
   {
   int   i;
   char  szTmp [StrSize];

   if (pszStr[0] == '*')
      return TRUE;
   for (i= 1; GetCSVField (i, OutRec.pszAreaPtr, szTmp), szTmp[0]!= '\0'; i++)
      if (stricmp (szTmp, pszStr) == OK)
         return TRUE;
   return FALSE;      
   }   




/*
 * Returns TRUE if pszKey is in the Key List
 *
 */
BOOL KeyInKeyList (OFR OutRec, PSZ pszKey)
   {
   return (bsearch (pszKey, OutRec.pszKeyBuff,
                    OutRec.iKeyCount, KeySize,
                    stricmp                  ) != NULL);
   }



/*
 * Builds a file name out of associated parts, replaces '*' in pszDef
 * with pszDName name.
 */
void ExpandName (PSZ pszStr, PSZ pszPath, PSZ pszDef, PSZ pszDName)
   {
   int i,j,k;

   for (i=0; pszPath[i] != '\0'; i++)
      pszStr[i] = pszPath[i];
   for (j=0; pszDef[j] != '*' && pszDef[j] != '\0'; j++)
      pszStr[i++] = pszDef[j];
   for (k=0; pszDName[k] != '\0'; k++)
      pszStr[i++] = pszDName[k];
   for (; pszDef[j] != '\0'; j++)
      if (pszDef[j] != '*')
         pszStr[i++] = pszDef[j];
   pszStr[i] = '\0';
   }


/***********************************************************************/
/*                    Ini process Procedures                           */
/***********************************************************************/



/*
 * Return values:::  OK, NoEntrys
 *
 * scans ini file for line containing [pszIdent.pszSubItent]
 * where pszIdent is given and pszSubItent is returned.
 * (and most importantly the file ptr is set at the next line)
 */
int GetEntry (FILE *fpIni, PSZ pszIdent, PSZ pszSubIdent)
   {
   char  c;
   char szStr [StrSize];

   while (TRUE)
      {
      while (TRUE)
         {
         SkipBlanks (fpIni, OK);
         if ((c = (char) getc (fpIni)) == '[' || c == EOF)
            break;
         SkipToEOL (fpIni);
         }
      if (c == EOF)
         return NoEntrys;
      ReadWord (fpIni, szStr, ALNUM);
      if (stricmp (szStr, pszIdent) == OK)
         {
         if (SkipChar (fpIni, '.') == Error)
            Abort (" '.' expected after [BhTrim  specifier", NULL, NULL);
         ReadWord (fpIni, pszSubIdent, ALNUM);
         if (SkipChar (fpIni, ']') == Error)
            Abort (" ']' expected after [BhTrim.county  specifier", NULL, NULL);
         SkipToEOL (fpIni);
         return OK;
         }
      }
   }





/*
 * Return values:::  OK, NoEntrys
 *
 * Reads the next line from the ini file of the form
 * szVar = szVal.  It will skip blank lines and strip
 * leading/trailing blanks. NoEntrys is returned if an
 * attempt is made to read a new entry type.
 */
int GetEntryLine (FILE *fpIni, PSZ pszVar, PSZ pszVal, PSZ pszDName)
   {
   char  c;
   fpos_t  pIniPos;

   fgetpos (fpIni, &pIniPos);
   while ((ReadWord (fpIni, pszVar, ALNUM) == OK) && (pszVar[0] == '\0'))
      {
      if ((c = (char) getc (fpIni)) == INICOMMENTCHAR)
         SkipToEOL (fpIni);
      else if (c == ' ')
         ;
      else if (c == '[' || c == EOF)
         {
         fsetpos (fpIni, &pIniPos);
         return NoEntrys;
         }
      else Abort ("Illegal char '%c' encountered in Ini file at county %s", &c, pszDName);
      }
   if (pszVar[0] == '\0')
      return NoEntrys;
   if (SkipChar (fpIni, '=') == Error)
      Abort (" '=' expected in in ini file after %s in county %s", pszVar, pszDName);
   ReadLine (fpIni, pszVal, BigStrSize, SPACE);
   return OK;
   }



/***********************************************************************/
/*                         Key List Procedures                         */
/***********************************************************************/



/*
 * return values::: OK, OutOfHandles, OutOfMem
 *
 * this proc loads keyfile to a buffer and sorts it.
 */
/* OutRec should be a pointer to the correct record */
int BuildKeyList (PSZ pszKeyFile, OFR *sOutRec)

   {
   FILE   *fpIn;
   PSZ    pszStrPtr;
   char   szKey [StrSize];
   struct stat StatBuff;
   char   *pszTmp;
   int    i;

   /*** make sure the key list doesn't eat ALL the memory ***/
   if ((pszTmp = malloc (MemClearance)) == NULL)
      return OutOfMem;

   sOutRec->pszKeyBuff = NULL;
   sOutRec->iKeyCount = 0;

   if ((fpIn = fopen (pszKeyFile, "r")) == NULL)
      {
      if (stat (pszKeyFile, &StatBuff))
         Abort ("Key input file not found %s from district %s", pszKeyFile,
                 sOutRec->pszDistName);
      free (pszTmp);
      return OutOfHandles;
      }

     i = NumLines (fpIn);

  if ((long) i * (long) KeySize > 65535L)
      Abort ("Cannot allocate more than 64K for a keyfile in\ndistrict %s, file %s",
              sOutRec->pszDistName, pszKeyFile);
  if ((sOutRec->pszKeyBuff = malloc ((i + 1) * KeySize)) == NULL)
         {
         fclose (fpIn);
         free (pszTmp);
         return OutOfMem;
         }

  while (ReadWord (fpIn, szKey, SPACE) == OK)
      {
      SkipToEOL (fpIn);
      if (szKey[0] == INICOMMENTCHAR || szKey[0] == '\n')
         continue;

      if ((int) strlen (szKey) >= KeySize) 
         Abort ("Key: %s from keyfile: %s greater than max key size", szKey, pszKeyFile);
      sOutRec->iKeyCount++;
      pszStrPtr = sOutRec->pszKeyBuff + KeySize * (sOutRec->iKeyCount -1);
      strcpy (pszStrPtr, szKey); 
      }
   fclose (fpIn);
   qsort (sOutRec->pszKeyBuff, sOutRec->iKeyCount, KeySize, stricmp); 
   free (pszTmp);
   return OK;
   }




/*
 * De-allocates dynamic file descriptor array,
 * closes open output files.
 */
void ClearOutFileRec (OFR *sOutRec, int iRecCount)
   {
   for (iRecCount--; iRecCount >=0; iRecCount--)
      {
      fclose (sOutRec[iRecCount].fpOutFile);
      free   (sOutRec[iRecCount].pszAreaPtr);
      free   (sOutRec[iRecCount].pszKeyBuff);
      free   (sOutRec[iRecCount].pszDistName);
      free   (sOutRec+iRecCount);
      }
   }

/***********************************************************************/
/*                                                                     */
/***********************************************************************/


/*
 * Return values:::  OK, OutOfHandles, OutOfMem, NoFilesFound
 *
 * This procedure reads the ini file for information about an output file
 * and adds the file info to the OutFileRec array.  Its long and ugly.
 */

int AddOutFileRec (FILE *fpIni, OFR **sOutRec, int *iRecCount)
   {
   char    szVal     [BigStrSize];
   char    szVar     [StrSize];
   char    szOutFile [StrSize];
   char    szKeyFile [StrSize];
   char    szAreas   [BigStrSize];
   char    szDName   [StrSize];
   static  char      szKeyPath [StrSize] = "";
   static  char      szOutPath [StrSize] = "";
   static  char      szDefKey  [StrSize] = "*";
   static  char      szDefOut  [StrSize] = "*";
   static  int       iFilesProcessed     = 0;
   fpos_t  pIniPos;
   int     i;

   if (IsMem (sizeof (OFR)) == OutOfMem)
      return OutOfMem;
   fgetpos (fpIni, &pIniPos);
   if (GetEntry (fpIni, ENTRYID, szDName) != OK)
      return NoFilesFound;
   if (*iRecCount >= MaxOpenFiles)
      return ReachedLimit;

   szKeyFile[0] = szOutFile[0] = szAreas[0] = '\0';
   do
      {
      if (GetEntryLine (fpIni, szVar, szVal, szDName))
         break;
      if (strcmpi (KEYFILEVAR, szVar) == OK)
         strcat (strcpy (szKeyFile, szKeyPath), szVal);
      else if (stricmp (OUTFILEVAR, szVar) == OK)
         strcat (strcpy (szOutFile, szOutPath), szVal);
      else if (stricmp (AREASVAR, szVar) == OK)
         strcpy (szAreas, szVal);
      else if (stricmp (DEFKEYFILE, szVar) == OK)
         strcpy (szDefKey, szVal);
      else if (stricmp (DEFOUTFILE, szVar) == OK)
         strcpy (szDefOut, szVal);
      else if (stricmp (KEYSIZE, szVar) == OK)
         {
         if (!KeySizeRedefined)
            {
            KeySizeRedefined = TRUE;
            KeySize = atoi (szVal) + 1;
            }  
         else
            Abort ("Key size redefined in %s", szDName, NULL);
         }
      else if (stricmp (KEYPATH, szVar) == OK)
         strcat (strcpy (szKeyPath, szVal), (strlen (szVal) > 0 &&
                 szVal [strlen (szVal) - 1] != '\\') ? "\\" : "");
      else if (stricmp (OUTPATH, szVar) == OK)
         strcat (strcpy (szOutPath, szVal), (strlen (szVal) > 0 &&
                 szVal [strlen (szVal) - 1] != '\\') ? "\\" : "");
      else
         Abort ("Illegal var name: %s  in initialization file, district %s", szVar, szDName);
      }
   while (TRUE);

   KeySizeRedefined = TRUE;
   if (szKeyFile[0] == '\0')
      ExpandName (szKeyFile, szKeyPath, szDefKey, szDName);

   if (szOutFile[0] == '\0')
      ExpandName (szOutFile, szOutPath, szDefOut, szDName);

   if ((*sOutRec = (OFR *) realloc (*sOutRec, (*iRecCount + 1) * sizeof (OFR))) == NULL)
      Abort ("Insufficient memory error (1): please contact Info Tech.",NULL, NULL);

   (*sOutRec)[*iRecCount].pszDistName = strdup (szDName);
   i = BuildKeyList (szKeyFile, *sOutRec + *iRecCount);

   if (i == OutOfMem || i == OutOfHandles)
      {
      fsetpos (fpIni, &pIniPos);
      return i;
      }
   if (((*sOutRec)[*iRecCount].fpOutFile = fopen (szOutFile, "w")) == NULL)
      {
      fsetpos (fpIni, &pIniPos);
      return OutOfHandles;
      }

   (*sOutRec)[*iRecCount].pszAreaPtr = strdup (szAreas);
   (*sOutRec)[*iRecCount].iICount = 0;
   *iRecCount += 1;
   iFilesProcessed++;

   if (bPrintHeader)
      {
      bPrintHeader = FALSE;
      printf ("\n ##  District  KeyFile                       OutputFile\n");
      printf ("=============================================================================\n");
      }
   printf ("%c%.2d> %-9s %-29s %-29s\n", (szAreas[0] == '\0')? ' ' : '*',
            iFilesProcessed, szDName, szKeyFile, szOutFile);
   return OK;
   }


/***********************************************************************/
/*                            Trim Procs                               */
/***********************************************************************/



/*
 * Checks to see if pszArea is already in Area csv list. If not,
 * it is added to the list.
 */
void AddIfNeeded (OFR *OutRec, PSZ pszArea)
   {
   char  szStr  [BigStrSize];
   char  szJunk [StrSize];

   if (StrInCSVList (*OutRec, pszArea))
      return;

   strcpy (szStr, OutRec->pszAreaPtr);
   if (szStr[0] != '\0')
      strcat (szStr, ",");
   strcat (szStr, MakeCSVField (pszArea, szJunk));

   free (OutRec->pszAreaPtr);
   OutRec->pszAreaPtr = strdup (szStr);
   }



/*
 * Processes BIDAVERAGEELEMENTs in bidaverage lists
 *
 */
void EatAveragePieces (FILE *fpData, OFR* sOutRec, int iRecCount)
   {
   char  szStr  [BigStrSize];
   char  szWord [StrSize];
   int   i, j;

   ReadLine (fpData, szStr, BigStrSize, OK);
   Test (szStr, 1, BIDAVERAGE);

   EchoWord (sOutRec, iRecCount, GetCSVField (1, szStr, szWord), ACTIVE);
   EchoWord (sOutRec, iRecCount, ",", ACTIVE);
   SavePositions (sOutRec, iRecCount, ITEMS);
   EchoLine (sOutRec, iRecCount, "     ", ACTIVE);

   for (j = iRecCount -1; j >= 0; j--)
      sOutRec[j].iICount = 0;
   
   for (i = atoi (GetCSVField (2, szStr, szWord)); i > 0; i--)
      {
      ReadLine (fpData, szStr, BigStrSize, OK);
      GetCSVField (4, szStr, szWord);
      for (j = iRecCount -1; j >= 0; j--)
         if (sOutRec[j].bActive && StrInCSVList (sOutRec[j], szWord))
            {
            EchoLine (sOutRec, iRecCount, szStr, j);
            sOutRec[j].iICount += 1;
            }
      }
   WriteAtPositions (sOutRec, iRecCount, ITEMS);
   }



/*
 * Processes BIDREGRESSIONELEMENTs in regression lists
 *
 */
void EatRegressionPieces (FILE *fpData, OFR* sOutRec, int iRecCount)
   {
   char  szStr  [BigStrSize];
   char  szWord [StrSize];
   int   i, j;

   ReadLine (fpData, szStr, BigStrSize, OK);
   Test (szStr, 1, BIDREGRESSION);

   EchoWord (sOutRec, iRecCount, GetCSVField (1, szStr, szWord), ACTIVE);
   EchoWord (sOutRec, iRecCount, ",", ACTIVE);
   SavePositions (sOutRec, iRecCount, ITEMS);
   EchoLine (sOutRec, iRecCount, "     ", ACTIVE);

   for (j = iRecCount -1; j >= 0; j--)
      sOutRec[j].iICount = 0;
   
   for (i = atoi (GetCSVField (2, szStr, szWord)); i > 0; i--)
      {
      ReadLine (fpData, szStr, BigStrSize, OK);

      /* if value is 0.0000 then don't bother including in output */
      if (atof (GetCSVField (4, szStr, szWord)) == 0.0)
         continue;

      /* if the regression isn't an areatype then echo it */
      if (stricmp (GetCSVField (2, szStr, szWord), AREATYPE))
         for (j = iRecCount -1; j >= 0; j--)
            {
            if (sOutRec[j].bActive)
               {
               EchoLine (sOutRec, iRecCount, szStr, j);
               sOutRec[j].iICount += 1;
               }
            }

      /* if an areatype, make sure areas match before echoing */
      else
         {
         GetCSVField (3, szStr, szWord);
         for (j = iRecCount -1; j >= 0; j--)
            {
            if (sOutRec[j].bActive &&
                StrInCSVList (sOutRec[j], szWord))
               {
               EchoLine (sOutRec, iRecCount, szStr, j);
               sOutRec[j].iICount += 1;
               }
            }
         }
      }
   WriteAtPositions (sOutRec, iRecCount, ITEMS);
   }




/*
 * Processes BIDCATELEMENT
 *
 */
void EatBidCat (FILE *fpData, OFR* sOutRec, int iRecCount, int iECount)
   {
   char  szStr      [BigStrSize];
   char  szKey      [StrSize];
   int   i;

   for (i = iRecCount -1; i >= 0; i--)
      sOutRec[i].iECount = 0;
   printf ("\n00000 Items to trim for districts %s-%s.",
            sOutRec[0].pszDistName, sOutRec[iRecCount-1].pszDistName);
   for (iECount--; iECount >= 0; iECount--)
      {
      printf ("\x0D%5d",iECount);
      ReadLine (fpData, szStr, BigStrSize, OK);

      Test (szStr, 1, BIDCATELEMENT);
      GetCSVField (2, szStr, szKey);
      for (i = iRecCount -1; i >= 0; i--)
         if (sOutRec[i].bActive = KeyInKeyList (sOutRec[i], szKey))
            sOutRec[i].iECount++;
      EchoLine (sOutRec, iRecCount, szStr, ACTIVE);
      EatAveragePieces (fpData, sOutRec, iRecCount);
      EatRegressionPieces (fpData, sOutRec, iRecCount);
      }
   printf ("\x0D                                          \x0D");
   bBidCatProcessed = TRUE;
   }


/*
 * Processes COUNTY 
 *
 */
void EatCounties (FILE *fpData, OFR* sOutRec, int iRecCount, int iCCount)
   {
   char  szStr      [BigStrSize];
   char  szDistrict [StrSize];
   char  szArea     [StrSize];
   int   i;

   for (iCCount--; iCCount >=0; iCCount--)
      {
      ReadLine (fpData, szStr, BigStrSize, OK);
      EchoLine (sOutRec, iRecCount, szStr, ALL);

      if (bCountiesProcessed)
         continue;
      GetCSVField (3, szStr, szDistrict);
      GetCSVField (4, szStr, szArea);
      for (i = iRecCount - 1; i >= 0; i--)
         {
         if (stricmp (sOutRec[i].pszDistName, szDistrict) == OK)
            AddIfNeeded (&sOutRec[i], szArea);
         }
      }
   bCountiesProcessed = TRUE;
   }



/*
 * Processes entities in master csv data file
 * (excluding the header line)
 */
int EatObject (FILE *fpData, OFR* sOutRec, int iRecCount)
   {
   char  szStr   [BigStrSize];
   char  szWord  [StrSize];
   int   iCount;

   if (ReadLine (fpData, szStr, BigStrSize, OK) != OK)
      return NoEntrys;

   iCount = atoi (GetCSVField (2, szStr, szWord));
   GetCSVField (1, szStr, szWord);

   if (stricmp (szWord, COUNTIES) == OK)
      {
      EchoLine (sOutRec, iRecCount, szStr, ALL);
      EatCounties (fpData, sOutRec, iRecCount, iCount);

      }
   else if (stricmp (szWord, BIDCAT) == OK)
      {
      if (!bCountiesProcessed)
         Abort ("Error in Input Data file: Count info must preceed BIDCAT info", NULL, NULL);
      if (bBidCatProcessed)
         Abort ("Error in Input Data file: Multiple BIDCAT objects found", NULL, NULL);

      EchoWord (sOutRec, iRecCount, szWord, ALL);
      EchoWord (sOutRec, iRecCount, ",", ALL);
      SavePositions (sOutRec, iRecCount, ELEMENTS);
      EchoLine (sOutRec, iRecCount, "     ", ALL);
      EatBidCat (fpData, sOutRec, iRecCount, iCount);

      WriteAtPositions (sOutRec, iRecCount, ELEMENTS);
      }
   else
      Abort ("Unknown object encountered in data file : %s", szWord, NULL);
   return OK;
   }






/*
 * Processes header line in master csv data file
 */
void EatHeaderLine (FILE *fpData, OFR* sOutRec, int iRecCount)
   {
   char  szStr [BigStrSize];
   char  szWord [StrSize];

   if (ReadLine (fpData, szStr, BigStrSize, OK) != OK)
      Abort ("Unexpected EOF in input data file", NULL, NULL);
   EchoLine (sOutRec, iRecCount, szStr, ALL);

   if (stricmp (GetCSVField (1, szStr, szWord), HighEstName) != OK)
      Abort ("Data file is not a Highest Bid History file", NULL, NULL);
   if (stricmp (GetCSVField (2, szStr, szWord), HighEstVer) != OK)
      Abort ("Data file (%s) is an incompatible version (%s)", szStr, szWord);
   Test (szStr, 3, Header3);
   Test (szStr, 4, Header4);
   Test (szStr, 5, Header5);
   }



/*
 * performs the ginsu-knife function on files that are currently open.
 */
void  TrimFiles (FILE *fpData, OFR *sOutRec, int iRecCount)
   {
   bBidCatProcessed = FALSE;
   rewind (fpData);
   EatHeaderLine (fpData, sOutRec, iRecCount);
   while (EatObject (fpData, sOutRec, iRecCount) == OK)
      ;
   }





/***********************************************************************/
/*                                 Procs                               */
/***********************************************************************/




int cdecl main (int argc, PSZ argv[])
   {
   FILE *fpData;
   FILE *fpIni;
   OFR  *sOutRec;
   int  iRecCount;
   int  iResult;

   printf ("\nBHTrim    Bid History Trim Utility,      Ver 2.01     (c)1990, Info Tech, Inc.\n");
   if (argc != 2 && argc != 3)
      Message ();

   if ((fpIni = fopen ( (argc == 2 ? "bhtrim.ini" : argv[1]), "r")) == NULL)
      Abort ("Unable to open Ini file %s", (argc == 2 ? "bhtrim.ini" : argv[1]), NULL);
         
   if ((fpData = fopen (argv[argc - 1], "r")) == NULL)
      Abort ("Unable to open data file %s", argv[1], NULL);
   do
      {
      sOutRec = NULL;
      iRecCount = 0;
      bPrintHeader = TRUE;
      while ((iResult = AddOutFileRec (fpIni, &sOutRec, &iRecCount)) == OK)
         ;
      TrimFiles (fpData, sOutRec, iRecCount); 
      ClearOutFileRec (sOutRec, iRecCount);  /* closes files too */ 
      }
   while (iResult != NoFilesFound);
   printf ("\ndone.\n");
   return 0;
   }

/* the end */
