#!/usr/bin/env python3

import sys

import numpy as np
from netCDF4 import Dataset

from SourceData import SourceData
from OrographyStats import OrographyStats


topo_data   = "/home/aislas/wrf-model/DATA/WPS_GEOG/topo_gmted2010_30s"
topo_source = SourceData( "gmted2010_30s", topo_data ) 

# For now just get cmdline arg
geo_files = sys.argv[1:]
box_size  = 30000

for geo in geo_files:
  print( f"Processing {geo}" )
  geo_data = Dataset( geo, "r+" )

  xlat = geo_data.variables[ "XLAT_M" ]
  xlon = geo_data.variables[ "XLONG_M" ]
  
  # Fields to overwrite
  con  = geo_data.variables[ "CON" ]
  var  = geo_data.variables[ "VAR" ]
  
  oa1  = geo_data.variables[ "OA1" ]
  oa2  = geo_data.variables[ "OA2" ]
  oa3  = geo_data.variables[ "OA3" ]
  oa4  = geo_data.variables[ "OA4" ]

  ol1  = geo_data.variables[ "OL1" ]
  ol2  = geo_data.variables[ "OL2" ]
  ol3  = geo_data.variables[ "OL3" ]
  ol4  = geo_data.variables[ "OL4" ]

  ns_size = xlat.shape[1]
  we_size = xlat.shape[2]

  for j in range( ns_size ):
    for i in range( we_size ):
      lat = xlat[0,j,i]
      lon = xlon[0,j,i]
      box = topo_source.get_box( lat, lon, box_size )

      oro_stats  = OrographyStats( box )
      con[0,j,i] = oro_stats.con_
      var[0,j,i] = oro_stats.std_

      oa1[0,j,i] = oro_stats.oa_[0]
      oa2[0,j,i] = oro_stats.oa_[1]
      oa3[0,j,i] = oro_stats.oa_[2]
      oa4[0,j,i] = oro_stats.oa_[3]

      ol1[0,j,i] = oro_stats.ol_[0]
      ol2[0,j,i] = oro_stats.ol_[1]
      ol3[0,j,i] = oro_stats.ol_[2]
      ol4[0,j,i] = oro_stats.ol_[3]

  geo_data.close()

print( "Done!" )
