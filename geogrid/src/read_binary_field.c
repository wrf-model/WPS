#include <stdio.h>

#ifdef _UNDERSCORE
#define read_binary_field read_binary_field_
#endif
#ifdef _DOUBLEUNDERSCORE
#define read_binary_field read_binary_field__
#endif

void read_binary_field(float * data, int * in_size, int * nitems, char * fname, int * len, int * is_signed, int * status)
{
   FILE * fp;
   unsigned char * c;
   int cnt, i, j, k;
   char local_fname[1024];
 
   strncpy(local_fname,fname,*len);
   local_fname[*len]='\0';
   if (!(fp = fopen(local_fname,"rb")))
   {
      *status = 1;
      return;
   }
 
   cnt = fread((void *)data,(*in_size),(*nitems),fp);
 
   if (cnt == 0) 
   {
      *status = 1;
      return;
   }
 
   fclose(fp);
 
   c = (unsigned char *)data;
 
   if (*in_size == 1) 
   {
      if (*is_signed == 1) 
      {
         for(i=(*nitems)-1; i>=0; i--)
         {  
            data[i] = (float)c[i];      
            if (data[i] >= 127.) data[i] -= 256.;
         }
      }
      else
      {
         for(i=(*nitems)-1; i>=0; i--)
           data[i] = (float)c[i]; 
      }
   }
 
   else if (*in_size == 2)
   {
      if (*is_signed == 1)
      {
         for(i=(*nitems)-1; i>=0; i--)
         {
            data[i] = (float)((c[2*i]<<8)|(c[2*i+1]));      
            if (data[i] >= 32768.) data[i] -= 65536.; 
         }
      }
      else
      {
         for(i=(*nitems)-1; i>=0; i--)
            data[i] = (float)((c[2*i]<<8)|(c[2*i+1]));      
      }
   }
 
   else if (*in_size == 3)
   {
      if (*is_signed == 1)
      {
         for(i=(*nitems)-1; i>=0; i--)
         {
            data[i] = (float)((c[3*i]<<16)|(c[3*i+1]<<8)|(c[3*i+2]));
            if (data[i] >= 8388608.) data[i] -= 16777216.; 
         }
      }
      else
      {
         for(i=(*nitems)-1; i>=0; i--)
            data[i] = (float)((c[3*i]<<16)|(c[3*i+1]<<8)|(c[3*i+2]));
      }
   }

   else if (*in_size == 4)
   {
      if (*is_signed == 1)
      {
         for(i=(*nitems)-1; i>=0; i--)
         {
            data[i] = (float)((c[4*i]<<24)|(c[4*i+1]<<16)|(c[4*i+2]<<8)|c[4*i+3]);
            if (data[i] >= 2147483648.) data[i] -= 4294967296.; 
         }
      }
      else
      {
         for(i=(*nitems)-1; i>=0; i--)
            data[i] = (float)((c[4*i]<<24)|(c[4*i+1]<<16)|(c[4*i+2]<<8)|c[4*i+3]);
      }
   }
 
   *status = 0;
}
