#!/usr/bin/env python3
import os
import re
import argparse
from collections import OrderedDict

import netCDF4 as nc4

from SourceData import SourceData
from OrographyStats import OrographyStats


# https://stackoverflow.com/a/34325723
def progress_bar( iteration, total, prefix="", suffix="", precision=1, length=100, fill="*", empty="-", end="\r"):
  """Print out a progress bar based on iteration/total of fill
  iteration  : current iteration
  total      : total iterations
  prefix     : prefix string
  suffix     : suffix string
  decimals   : precision of float in printing
  length     : character length of bar
  fill       : bar fill character
  end        : end character (e.g. "\r", "\r\n")
  """
  progress = iteration / float( total )
  percent = ( f"{{0:.{precision}f}}").format( 100 * progress )
  current_length = int( length * progress )
  bar = fill * current_length + empty * ( length - current_length )
  print(f'\r{prefix} [{bar}] {percent}% {suffix}', end=end)
  if iteration == total:
    print( "" )


def read_nml( filename ):
  """Read a strict subset of Fortran 90 namelist
  This ONLY reads namelists in the format:
  &group
  key0 = value0,
  key1  =  value0, value1,
  key2='value0', 'value1',
  key3  =   "value0", "value1",
  /
  ...and so on...
  Index slices, multiple key-value pairs on the same line,
  and other features are NOT supported. For full support
  please utilize f90nml.
  Spaces between key-value assignment and commas separated values do not matter.
  Multivalue entries MUST be delimited by commas.
  """
  contents = ""
  with open( filename, "r" ) as f:
    contents = f.read()

  nml = OrderedDict()
  for match in re.finditer( r"&(?P<group>\w+)(?P<kv_pairs>.*?)\n*^[ ]*/", contents, re.S | re.M ):
    group = match.group( "group" )
    nml[group] = OrderedDict()
    for kv_match in re.finditer( r"^[ ]+(?P<key>\w+)[ ]*=[ ]*(?P<value>.*?)$", match.group("kv_pairs"), re.M ):
      key = kv_match.group( "key" )
      values = list(
                    filter(
                          None,
                          [
                            s.strip("\"' " )
                            for s in list( filter( None, kv_match.group( "value" ).split(",") ) )
                          ]
                          )
                    )
      nml[group][key] = values if len( values ) > 1 else values[0]
  return nml


def main():
  parser = argparse.ArgumentParser(
                                    "compute_gwdo.py",
                                    description="Compute GWDO ancillary fields directly and update geogrid files",
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter
                                    )
  parser.add_argument(
                      "-n", "--namelist",
                      help="WPS namelist",
                      type=str,
                      default="./namelist.wps"
                      )
  parser.add_argument(
                      "-d", "--dataset",
                      help="Topographic dataset to use, in geogrid format. Must be a global lat-lon dataset (i.e. projection=regular_ll covering entire globe)",
                      type=str,
                      default="topo_gmted2010_30s"
                      )
  parser.add_argument(
                      "-hp", "--hide_progress",
                      help="Hide the progress bar",
                      action="store_true"
                      )
  options = parser.parse_args()
  nml = read_nml( options.namelist )
  datapath= nml["geogrid"]["geog_data_path"]

  topo_data   = os.path.join( datapath, options.dataset )
  topo_source = SourceData( options.dataset, topo_data )

  # For now just get cmdline arg
  max_dom = 1
  if "max_dom" in nml["share"]:
    max_dom = int(nml["share"]["max_dom"])
  geo_files = [ f"geo_em.d{dom_id + 1:02d}.nc" for dom_id in range( max_dom ) ]

  # Scale-awareness, require min box size 4x4 using nominal dx of orographic data
  sg_delta = topo_source._subgrid_m_dx

  for geo in geo_files:
    print( f"Processing {geo}" )
    geo_data = nc4.Dataset( geo, "r+" )

    dc = min( geo_data.DX, geo_data.DY ) * 2
    hratio = dc / sg_delta
    if hratio < 4.0:
      dc = sg_delta * 4.0

    box_size_x  = dc
    box_size_y  = dc

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

    if "ELVMAX" in geo_data.variables:
      elvmax = geo_data.variables["ELVMAX"]
    else:
      elvmax = geo_data.createVariable( "ELVMAX", "f4", ( var.dimensions ) )
      # Initialize with same attributes as VAR, but setting correct
      # units and description
      elvmax_attrs = { k : var.getncattr(k) for k in var.ncattrs() }
      elvmax_attrs[ "units" ] = "m"
      elvmax_attrs[ "description" ] = "Maximum sub-grid orographic height"
      elvmax.setncatts( elvmax_attrs )

    ns_size = xlat.shape[1]
    we_size = xlat.shape[2]

    if not options.hide_progress:
      progress_bar( 0, ns_size * we_size, length=50 )
    for j in range( ns_size ):
      for i in range( we_size ):
        lat = xlat[0, j, i]
        lon = xlon[0, j, i]
        box = topo_source.get_box( lat, lon, box_size_x, box_size_y )

        oro_stats  = OrographyStats( box )
        con[0, j, i] = oro_stats.con
        # Correct std deviation for scale awareness changes
        var[0, j, i] = oro_stats.std * (hratio * 0.25 if ( hratio < 4.0 ) else 1)

        oa1[0, j, i] = oro_stats.oa[0]
        oa2[0, j, i] = oro_stats.oa[1]
        oa3[0, j, i] = oro_stats.oa[2]
        oa4[0, j, i] = oro_stats.oa[3]

        ol1[0, j, i] = oro_stats.ol[0]
        ol2[0, j, i] = oro_stats.ol[1]
        ol3[0, j, i] = oro_stats.ol[2]
        ol4[0, j, i] = oro_stats.ol[3]

        elvmax[0, j, i] = oro_stats.max
        if not options.hide_progress:
          progress_bar( i + j * we_size + 1, ns_size * we_size, length=50 )

    geo_data.close()

  print( "Done!" )


if __name__ == "__main__":
  main()
