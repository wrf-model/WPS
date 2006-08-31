#include <stdio.h>

#ifdef _UNDERSCORE
#define cio_prints cio_prints_
#define cio_printf cio_printf_
#define cio_printi cio_printi_
#endif
#ifdef _DOUBLEUNDERSCORE
#define cio_prints cio_prints__
#define cio_printf cio_printf__
#define cio_printi cio_printi__
#endif

#ifdef _GEOGRID
char logfilename[] = "geogrid.log";
#endif
#ifdef _METGRID
char logfilename[] = "metgrid.log";
#endif

FILE * cio_out = 0;

void cio_printf(int * fd, float * f)
{
   if (*fd != 0) 
   {
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%f", *f);
      fflush(cio_out);
   }
   else
   {
      fprintf(stdout, "%f", *f);
      fflush(stdout);
   }
}

void cio_printi(int * fd, int * i)
{
   if (*fd != 0) 
   {
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%i", *i);
      fflush(cio_out);
   }
   else
   {
      fprintf(stdout, "%i", *i);
      fflush(stdout);
   }
}

void cio_prints(int * fd, char * s, int * n)
{
   if (*fd != 0) 
   {
      s[*n] = '\0';
      if (!cio_out) cio_out = fopen(logfilename,"w");
      fprintf(cio_out, "%s", s);
      fflush(cio_out);
   }
   else
   {
      s[*n] = '\0';
      fprintf(stdout, "%s", s);
      fflush(stdout);
   }
}
