#include <stdlib.h>

#ifdef CRAY90
  #include <fortran.h>
  void ERREXIT (int a)
#endif
#ifdef HP
  void errexit (int a)
#endif
#ifdef SGI
  void errexit_ (int a)
#endif
#ifdef ALPHA
  void errexit_ (int a)
#endif
#ifdef LINUX
  void errexit_ (int a)
#endif
#ifdef LINUXG95
  void errexit_ (int a)
#endif
#ifdef LINUXF90
  void ERREXIT (int a)
#endif
#ifdef IBM4
  void errexit (int a)
#endif
#ifdef IBM8
  void errexit (int a)
#endif
#ifdef MAC
  void errexit (int a)
#endif
{
    exit (a);
}
