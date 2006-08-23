/*
      SUBROUTINE ERREXIT(IRET)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  ERREXIT       EXIT WITH A RETURN CODE
C   PRGMMR: IREDELL          ORG: NP23        DATE:1998-06-04
C
C ABSTRACT: EXIT WITH A RETURN CODE
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   1999-01-26  Gilbert     - changed to use XLF utility routine exit_(n)
C                             instead of exit(n).  exit_(n) will return
C                             the proper value ( n must be 4 byte int )
C                             to the sh/ksh shell status variable $?
C                             ( $status for csh ) on the IBM SP.
C
C USAGE:    CALL ERREXIT(IRET)
C   INPUT ARGUMENT LIST:
C     IRET     - INTEGER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   EXIT_      - EXITS FROM A FORTRAN PROGRAM
C
C ATTRIBUTES:
C   LANGUAGE: XLF FORTRAN 90
C   MACHINE: IBM SP
C
C$$$
      INTEGER IRET
      INTEGER(4) JRET
      JRET=IRET
      CALL exit_(JRET)
      END
*/
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
{
    exit (a);
}
