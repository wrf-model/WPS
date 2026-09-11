#! /usr/bin/python

# This script creates symbolic links for a list of grib files.

import sys
import glob
import os

def link_grib_file(filelist):
    
    # This function creates the links for a list of grib files.
    
    n = 0
    for ifile in filelist:
        if ifile!=".":
            file_suffix = file_alphabet(n)
            os.symlink(ifile, "GRIBFILE."+file_suffix)
            n = n + 1

def file_alphabet(number):
    
    # This function converts a number of a file to its three-letter number.
    
    alpha = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
    
    if number>26*26*26:
        print("Error: ran out of grib file suffixes.")
        sys.exit(1)
    else:
        i1 = number%26
        i2 = (number - i1)//26%26
        i3 = (number - i2*26 - i1)//26//26%26
        
        return alpha[i3] + alpha[i2] + alpha[i1]

if __name__=="__main__":
    
    varlist = sys.argv[1:]
    
    if len(varlist)==1 or len(varlist)==2 and varlist[1]=='.':
        os.system("rm -f GRIBFILE.???")
        filelist = glob.glob(varlist[0])
        link_grib_file(filelist)
    elif len(varlist)>1:
        os.system("rm -f GRIBFILE.???")
        filelist = varlist
        link_grib_file(filelist)
    elif len(varlist)==0:
        print("")
        print("Please provide some GRIB data to link") 
        print("usage: ./link_grib.py path_to_grib_data/*") 
        print("")





