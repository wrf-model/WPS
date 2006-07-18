       Integer Function mova2i(a)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    mova2i      Moves a bit string from a char*1 to int
C   PRGMMR: Gilbert          ORG: W/NP11     DATE: 98-12-15                     
C                                                                               
C ABSTRACT: This Function copies a bit string from a Character*1 variable
C   to an integer variable.  It is intended to replace the Fortran Intrinsic
C   Function ICHAR, which only supports 0 <= ICHAR(a) <= 127 on the
C   IBM SP.  If "a" is greater than 127 in the collating sequence, 
C   ICHAR(a) does not return the expected bit value when the -qhot 
C   ( and therefore -qsmp) option is used when compiling. 
C   This function can be used for all values 0 <= ICHAR(a) <= 255 and
C   will work with or without the -qhot compiler option.
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   98-12-15  Gilbert
C 2001-06-11  Gilbert   -  added a step to fill an 8-byte character
C                          array with the same value so that the
C                          f90 transfer function is more predictable.
C                          All bytes will now contain the desired value.
C                                                                               
C USAGE:     I = mova2i(a)
C                                                                               
C   INPUT ARGUMENT :
C
C          a - Character*1 variable that holds the bitstring to extract
C
C   RETURN ARGUMENT :
C
C          mova2i - Integer value of the bitstring in character a
C                                                                               
C REMARKS:                                                                      
C                                                                               
C      None
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: IBM XL FORTRAN
C   MACHINE:  IBM SP                                                             
C                                                                               
C$$$                                                                            
C                                                                               
       integer    mold                                                      
       character(len=1) a  
       character(len=1) ctemp(8)

       ctemp(1:8)=a
c       mova2i=ishft(transfer(ctemp,mold),8-bit_size(mold))
       mova2i=iand(transfer(ctemp,mold),255)

       return
       end
