#!/usr/bin/env python3
from collections import OrderedDict
import argparse
import importlib
import os
import re

import matplotlib as mpl
import matplotlib.patheffects as PathEffects
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature


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

  Spaces between key and value and commas does not matter.
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


def get_parser():
  parser = argparse.ArgumentParser(
                                    prog="./plot_domains.py",
                                    description="Plot expected domain layout from WPS namelist",
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter
                                    )
  parser.add_argument(
                      "-n", "--namelist",
                      help="WPS namelist",
                      type=str,
                      default="./namelist.wps"
                      )
  parser.add_argument(
                      "-b", "--border",
                      help="Extra margin to add when plotting in number of cells of primary domain",
                      type=int,
                      default=10
                      )
  parser.add_argument(
                      "-v", "--verify",
                      help="Verify output with geogrid output (requires running geogrid)",
                      action="store_true"
                      )
  parser.add_argument(
                      "--f90nml",
                      help="Use f90nml python module to read namelist if available",
                      action="store_true"
                      )
  parser.add_argument(
                      "-hb", "--hide_borders",
                      help="Hide country",
                      action="store_true"
                      )
  parser.add_argument(
                      "-hl", "--hide_lines",
                      help="Hide state and province lines",
                      action="store_true"
                      )
  parser.add_argument(
                      "-sr", "--show_ref",
                      help="Show ref x/y in domain as a red dot",
                      action="store_true"
                      )
  parser.add_argument(
                      "-r", "--rivers",
                      help="Show river features",
                      action="store_true"
                      )
  parser.add_argument(
                      "-i", "--info",
                      help="""
                           Control how much info in domain text
                           bitmask as integer
                           0 = Domain name
                           1 = Domain dx/dy
                           2 = Domain x/y total size
                           4 = Domain ij placement
                           8 = Domain we/sn total size
                           e.g. --info 15 prints everything
                           """,
                      type=int,
                      default=0
                      )
  parser.add_argument(
                      "-f", "--fontsize",
                      help="Font size of text",
                      type=int,
                      default=10
                      )
  parser.add_argument(
                      "-o", "--outline_text",
                      help="Switch the text to white with black border, can be useful if text is not readable",
                      action="store_true"
                      )
  parser.add_argument(
                      "-vf", "--verify_field",
                      help="Field in geogrid output used to verify against",
                      type=str,
                      default="HGT_M"
                      )
  parser.add_argument(
                      "-vc", "--verify_cellsize",
                      help="Size of cell data marker to use in plt.scatter()",
                      type=int,
                      default=12
                      )
  parser.add_argument(
                      "-c", "--cmap",
                      help="cmap used in verify for plt.scatter()",
                      type=str,
                      default="tab20"
                      )
  parser.add_argument(
                      "-p", "--perspective_view",
                      help="Also plot a side-by-side perspective view as if looking straight down at the domain",
                      action="store_true"
                      )
  parser.add_argument(
                      "-pe", "--perspective_extent",
                      help="Extent of perspective view in degrees. If not set, full globe map is output",
                      type=float,
                      default=None
                      )
  parser.add_argument(
                      "--vmin",
                      help="geogrid output min used when plotting",
                      type=float,
                      default=None
                      )
  parser.add_argument(
                      "--vmax",
                      help="geogrid output max used when plotting",
                      type=float,
                      default=None
                      )
  parser.add_argument(
                      "--ignore_verification",
                      help="Continue to plot regardless of domain validation",
                      action="store_true"
                      )
  parser.add_argument(
                      "-s", "--save_file",
                      help="File to save to",
                      default="domains.png"
                      )
  return parser


def verify_domain( max_dom, parent_id, parent_grid_ratio, ij_parent_start, ij_size, xy_start, xy_size ):
  # Check if domains sized correctly
  valid_domains = True
  ij_size_in_parent_cells = [ [0] * max_dom, [0] * max_dom]
  for dom_id in range( max_dom ):
    pg_ratio = parent_grid_ratio[dom_id]
    i = ij_size[0][dom_id]
    j = ij_size[1][dom_id]
    if pg_ratio != 1:
      e_we_off = i % pg_ratio
      e_sn_off = j % pg_ratio
      s = " "
      if e_we_off != 0:
        print( f"Domain {dom_id + 1} invalid: e_we must be one greater than multiple of parent_grid_ratio {pg_ratio}" )
        print( f"{s:<18}Current value {i + 1}, close valid values {i - e_we_off + 1} or {i + pg_ratio - e_we_off + 1}" )
        valid_domains = False
      if e_sn_off != 0:
        print( f"Domain {dom_id + 1} invalid: e_sn must be one greater than multiple of parent_grid_ratio {pg_ratio}" )
        print( f"{s:<18}Current value {j + 1}, close valid values {j - e_sn_off + 1} or {j + pg_ratio - e_sn_off + 1}" )
        valid_domains = False
      ij_size_in_parent_cells[0][dom_id] = int( i / pg_ratio )
      ij_size_in_parent_cells[1][dom_id] = int( j / pg_ratio )

  # Check boundary spacing
  boundary = 5
  for dom_id in range( max_dom ):
    if parent_grid_ratio[dom_id] != 1:
      i_parent_stop = ij_parent_start[0][dom_id] + ij_size_in_parent_cells[0][dom_id]
      j_parent_stop = ij_parent_start[1][dom_id] + ij_size_in_parent_cells[1][dom_id]
      if ij_parent_start[0][dom_id] < boundary:
        print( f"Domain {dom_id + 1} invalid: West border to parent requires space of {boundary} parent cells" )
        valid_domains = False
      if ij_parent_start[1][dom_id] < boundary:
        print( f"Domain {dom_id + 1} invalid: South border to parent requires space of {boundary} parent cells" )
        valid_domains = False
      if i_parent_stop + boundary > ij_size[0][parent_id[dom_id]]:
        print( f"Domain {dom_id + 1} invalid: East border to parent requires space of {boundary} parent cells" )
        valid_domains = False
      if j_parent_stop + boundary > ij_size[1][parent_id[dom_id]]:
        print( f"Domain {dom_id + 1} invalid: North border to parent requires space of {boundary} parent cells" )
        valid_domains = False

  # Check intersections
  for dom_id in range( max_dom ):
    if parent_grid_ratio[dom_id] != 1:
      for dom_id_compare in range( max_dom ):
        if parent_grid_ratio[dom_id_compare] == 1:
          continue
        if dom_id_compare != dom_id and parent_id[dom_id] == parent_id[dom_id_compare]:
          # Compare these two boxes at the same nesting level
          a = [
                xy_start[0][dom_id],
                xy_start[1][dom_id],
                xy_start[0][dom_id] + xy_size[0][dom_id],
                xy_start[1][dom_id] + xy_size[1][dom_id]
              ]
          b = [
                xy_start[0][dom_id_compare],
                xy_start[1][dom_id_compare],
                xy_start[0][dom_id_compare] + xy_size[0][dom_id_compare],
                xy_start[1][dom_id_compare] + xy_size[1][dom_id_compare]
              ]
          we_intersect = a[0] < b[2] and a[2] > b[0]
          sn_intersect = a[1] < b[3] and a[3] > b[1]
          if we_intersect and sn_intersect:
            print( f"Domain {dom_id + 1} invalid: Domain {dom_id + 1} and {dom_id_compare + 1} overlap" )
            valid_domains = False

  return valid_domains


def as_list( value ):
  if isinstance( value, list ):
    return value
  else:
    return [value]


if __name__ == "__main__":

  parser = get_parser()
  options = parser.parse_args()

  read_nml_f = read_nml
  if options.f90nml:
    mod_name = "f90nml"
    spec = importlib.util.find_spec( mod_name )
    if spec is not None:
      f90nml = importlib.util.module_from_spec( spec )
      spec.loader.exec_module( f90nml )
      print( "Using f90nml to read namelist" )
      read_nml_f = f90nml.read
    else:
      raise ModuleNotFoundError( f"No module named {mod_name}", name=mod_name )

  netCDF4 = None
  if options.verify:
    mod_name = "netCDF4"
    spec = importlib.util.find_spec( mod_name )
    if spec is not None:
      netCDF4 = importlib.util.module_from_spec( spec )
      spec.loader.exec_module( netCDF4 )
    else:
      raise ModuleNotFoundError( f"No module named {mod_name}", name=mod_name )

  nml = read_nml_f( options.namelist )

  max_dom = 1
  if "max_dom" in nml["share"]:
    max_dom = int(nml["share"]["max_dom"])
  geogrid = nml["geogrid"]

  projection = None
  ref_lat = float(geogrid["ref_lat"])
  ref_lon = float(geogrid["ref_lon"])

  false_easting  = 0.0
  false_northing = 0.0

  print( "Using map projection : " + geogrid["map_proj"] )
  units = "m"
  if geogrid["map_proj"] == "lambert":
    if "truelat2" in geogrid:
      cutoff_degrees = 15
      avg_lat = (float(geogrid["truelat1"]) + float(geogrid["truelat2"])) / 2
      if avg_lat < 0:
        cutoff = cutoff_degrees
      else:
        cutoff = -cutoff_degrees
      projection = ccrs.LambertConformal(
                                          standard_parallels=(
                                                              float(geogrid["truelat1"]),
                                                              float(geogrid["truelat2"])
                                                              ),
                                          central_latitude=ref_lat,
                                          central_longitude=float(geogrid["stand_lon"]),
                                          cutoff=cutoff
                                          )
  elif geogrid["map_proj"] == "mercator":
    projection = ccrs.Mercator(
                                latitude_true_scale=float(geogrid["truelat1"]),
                                central_longitude=float(geogrid["stand_lon"])
                                )
  elif geogrid["map_proj"] == "polar":
    if float(geogrid["truelat1"]) > 0:
      projection = ccrs.NorthPolarStereo(
                                          central_longitude=float(geogrid["stand_lon"]),
                                          true_scale_latitude=float(geogrid["truelat1"])
                                          )
    else:
      projection = ccrs.SouthPolarStereo(
                                          central_longitude=float(geogrid["stand_lon"]),
                                          true_scale_latitude=float(geogrid["truelat1"])
                                          )
  elif geogrid["map_proj"] == "lat-lon":
    if "pole_lat" in geogrid or "pole_lon" in "geogrid":
      raise Exception( "Rotated lat-lon projection not supported" )
    units = "deg"
    projection = ccrs.PlateCarree(
                                  central_longitude=float(geogrid["stand_lon"])
                                  )

  if projection is None:
    raise Exception( "Unsupported projection type!" )

  perspective_proj = None
  if options.perspective_view:
    perspective_proj = ccrs.NearsidePerspective( central_longitude=ref_lon, central_latitude=ref_lat )

  false_easting, false_northing = projection.transform_point(
                                                              ref_lon,
                                                              ref_lat,
                                                              ccrs.PlateCarree()
                                                              )

  # Find extents of domains which are mandated by ref_lat/lon and ref_x/y
  ref_x = -1
  ref_y = -1
  e_we  = [ int(dom_we) - 1 for dom_we in as_list( geogrid["e_we"] ) ]
  e_sn  = [ int(dom_sn) - 1 for dom_sn in as_list( geogrid["e_sn"] ) ]
  if "ref_x" not in geogrid:
    ref_x = int( e_we[0] / 2.0 )
    ref_y = int( e_sn[0] / 2.0 )
  else:
    ref_x = int(geogrid["ref_x"]) - 1
    ref_y = int(geogrid["ref_y"]) - 1

  dx = float(geogrid["dx"])
  dy = float(geogrid["dy"])

  i_parent_start  = [ int(i) - 1 for i in as_list( geogrid["i_parent_start"] ) ]
  j_parent_start  = [ int(j) - 1 for j in as_list( geogrid["j_parent_start"] ) ]
  parent_grid_ratio = [ int(ratio) for ratio in as_list( geogrid["parent_grid_ratio"] ) ]
  parent_id         = [ int(id) - 1 for id in as_list( geogrid["parent_id"] ) ]

  domain_extent_m = [
                       false_easting - dx * ( ref_x + 0.5 ),
                       false_easting + dx * ( e_we[0] - ( ref_x + 0.5 ) ),
                      false_northing - dy * ( ref_y + 0.5 ),
                      false_northing + dy * ( e_sn[0] - ( ref_y + 0.5 ) )
                    ]

  # First we will find the dx/dy per domain, ratios are stored for future
  # reference as well. This loop (esp while loop) specifically does NOT ASSUME
  # domains are listed in descending order (e.g. 1, 3, 2, 5, 4 is valid)
  dom_dx = [ dx ] * max_dom
  dom_dy = [ dy ] * max_dom
  dom_ratios = [ 1 ] * max_dom

  start_x = [ domain_extent_m[0] ] * max_dom
  start_y = [ domain_extent_m[2] ] * max_dom
  for dom_id in range( max_dom ):
    if parent_grid_ratio[dom_id] != 1:
      ratio  = parent_grid_ratio[dom_id]
      parent = dom_id

      while parent_grid_ratio[parent] != 1:
        parent = parent_id[parent]
        ratio *= parent_grid_ratio[parent]

      dom_dx[dom_id] = dx / ratio
      dom_dy[dom_id] = dy / ratio
      dom_ratios[dom_id] = ratio

  # Now find the SW corner (in projection) of each domain based on the
  # primary domain start location
  start_x = [ domain_extent_m[0] ] * max_dom
  start_y = [ domain_extent_m[2] ] * max_dom
  for dom_id in range( max_dom ):
    if parent_grid_ratio[dom_id] != 1:
      parent = dom_id
      start_x[dom_id] += dom_dx[parent_id[dom_id]] * i_parent_start[dom_id]
      start_y[dom_id] += dom_dy[parent_id[dom_id]] * j_parent_start[dom_id]

      while parent_grid_ratio[parent] != 1:
        parent = parent_id[parent]
        start_x[dom_id] += dom_dx[parent_id[parent]] * i_parent_start[parent]
        start_y[dom_id] += dom_dy[parent_id[parent]] * j_parent_start[parent]

  units_adj = ( "km" if units == "m" else units )
  units_div = ( 1000.0 if units == "m" else 1.0 )
  total_x = [ dom_dx[dom_id] * e_we[dom_id] for dom_id in range( max_dom ) ]
  total_y = [ dom_dy[dom_id] * e_sn[dom_id] for dom_id in range( max_dom ) ]
  print( "Domain ID       " + "".join( [ "{0:<12d}".format( dom_id + 1 ) for dom_id in range( max_dom ) ] ) )
  print( f"     dx ({units:^3}) : " + "".join( [ "{0:<12.2f}".format( dom_dx[dom_id] ) for dom_id in range( max_dom ) ] ) )
  print( f"     dy ({units:^3}) : " + "".join( [ "{0:<12.2f}".format( dom_dy[dom_id] ) for dom_id in range( max_dom ) ] ) )
  print( f"total x ({units_adj:^3}) : " + "".join( [ "{0:<12.2f}".format( total_x[dom_id] / units_div ) for dom_id in range( max_dom ) ] ) )
  print( f"total y ({units_adj:^3}) : " + "".join( [ "{0:<12.2f}".format( total_y[dom_id] / units_div ) for dom_id in range( max_dom ) ] ) )

  valid_domains = verify_domain(
                                max_dom,
                                parent_id,
                                parent_grid_ratio,
                                ( i_parent_start, j_parent_start ),
                                ( e_we, e_sn ),
                                ( start_x, start_y ),
                                ( total_x, total_y )
                                )
  if not valid_domains and not options.ignore_verification:
    raise Exception( "Bad domain configuration" )
  ##############################################################################
  # Plotting
  fig = plt.figure()
  grid_spec = fig.add_gridspec( 1, 2 if options.perspective_view else 1 )

  ax = fig.add_subplot( grid_spec[0], projection=projection )
  all_ax = [ ax ]
  if options.perspective_view:
    ax_perspective = fig.add_subplot( grid_spec[1], projection=perspective_proj )
    if options.perspective_extent is None:
      ax_perspective.set_global()
    else:
      ax_perspective.set_extent(
                                [
                                  ref_lon - options.perspective_extent,
                                  ref_lon + options.perspective_extent,
                                  ref_lat - options.perspective_extent,
                                  ref_lat + options.perspective_extent
                                ],
                                crs=ccrs.PlateCarree()
                                )
    all_ax.append( ax_perspective )

  # Extra border which may be useful in assessing quality of boundary conditions
  extra_we = options.border * dx
  extra_sn = options.border * dy
  ax.set_extent(
                [
                  domain_extent_m[0] - extra_we,
                  domain_extent_m[1] + extra_we,
                  domain_extent_m[2] - extra_sn,
                  domain_extent_m[3] + extra_sn
                ],
                crs=projection
                )

  for ax_proj in all_ax:
    grid = ax_proj.gridlines(
                              crs=ccrs.PlateCarree(),
                              draw_labels=True,
                              x_inline=False,
                              y_inline=False,
                              linewidth=0.33,
                              color='k',
                              alpha=0.5
                              )
    grid.right_labels = grid.top_labels = False

    if not options.hide_borders:
      ax_proj.add_feature( cfeature.BORDERS, edgecolor='darkgray' )
    if not options.hide_lines:
      ax_proj.add_feature( cfeature.STATES, edgecolor='gray' )

    if options.rivers:
      ax_proj.add_feature( cfeature.RIVERS, edgecolor='teal' )

    ax_proj.add_feature( cfeature.LAKES, facecolor='lightcyan', edgecolor='darkblue' )
    ax_proj.add_feature( cfeature.LAND, zorder=1, edgecolor='k' )

    # ref lat lon and thus where ref_x, ref_y are
    if options.show_ref:
      ax_proj.scatter( x=ref_lon, y=ref_lat, color="red", s=2, marker="s", alpha=1, transform=ccrs.PlateCarree() )

  extra_y_offset = 0
  for dom_id in range( max_dom ):
    info = f"Domain {dom_id + 1}"
    if options.info & ( 1 << 0 ):
      info += "\n(dx, dy) : ({dx:.2f}, {dy:.2f}){unit}"
      info  = info.format(
                          dx=dom_dx[dom_id] / units_div,
                          dy=dom_dy[dom_id] / units_div,
                          unit=units_adj
                          )
    if options.info & ( 1 << 1 ):
      info += "\n(x, y) : ({x:.2f}, {y:.2f}){unit}"
      info  = info.format(
                          x=total_x[dom_id] / units_div,
                          y=total_y[dom_id] / units_div,
                          unit=units_adj
                          )
    if options.info & ( 1 << 2 ):
      info += "\n(i, j) : ({i}, {j})".format(
                                              i=i_parent_start[dom_id] + 1,
                                              j=j_parent_start[dom_id] + 1
                                              )
    if options.info & ( 1 << 3 ):
      info += "\n(we, sn) : ({we}, {sn})".format(
                                                  we=e_we[dom_id] + 1,
                                                  sn=e_sn[dom_id] + 1
                                                  )
    text_color = "white" if options.outline_text else "black"
    txt = ax.text(
                  x=start_x[dom_id],
                  y=start_y[dom_id] - dy + extra_y_offset,
                  s=info,
                  va="top",
                  color=text_color,
                  size=options.fontsize,
                  transform=projection
                  )
    if options.outline_text:
      txt.set_path_effects( [PathEffects.withStroke( linewidth=3, foreground="black" ) ] )

    ax.add_patch(
                  mpl.patches.Rectangle(
                                        xy=[start_x[dom_id], start_y[dom_id]],
                                        width=dom_dx[dom_id] * e_we[dom_id],
                                        height=dom_dy[dom_id] * e_sn[dom_id],
                                        linewidth=2,
                                        linestyle="-",
                                        alpha=0.8,
                                        fill=False,
                                        transform=projection,
                                        zorder=2
                                        )
                )
    if options.perspective_view:
      x_len = [ start_x[dom_id] + dom_dx[dom_id] * i for i in range( e_we[dom_id] + 1 ) ]
      y_len = [ start_y[dom_id] + dom_dy[dom_id] * j for j in range( e_sn[dom_id] + 1 ) ]
      x_dup_start = [ start_x[dom_id] ] * ( e_sn[dom_id] + 1 )
      x_dup_stop  = [ start_x[dom_id] + dom_dx[dom_id] * e_we[dom_id] ] * ( e_sn[dom_id] + 1)
      y_dup_start = [ start_y[dom_id] ] * ( e_we[dom_id] + 1 )
      y_dup_stop  = [ start_y[dom_id] + dom_dy[dom_id] * e_sn[dom_id] ] * ( e_we[dom_id] + 1 )

      ax_perspective.plot( x_len, y_dup_start, transform=projection, color="black" )
      ax_perspective.plot( x_len, y_dup_stop,  transform=projection, color="black" )
      ax_perspective.plot( x_dup_start, y_len, transform=projection, color="black" )
      ax_perspective.plot( x_dup_stop,  y_len, transform=projection, color="black" )

  # Verification to geogrid output
  if options.verify:
    output_path = os.path.abspath( options.namelist )
    if "opt_output_from_geogrid_path" in geogrid:
      opt_path = geogrid["opt_output_from_geogrid_path"]
      if "." == opt_path[0]:
        path = os.path.abspath( output_path + "/" + opt_path )
      else:
        output_path = os.path.abspath( opt_path )

    geogrid_nc_template = "geo_em.d0{n}.nc"
    for dom_id in range( max_dom ):
      geo_em0 = netCDF4.Dataset( geogrid_nc_template.format( n=dom_id + 1 ), "r" )
      data = geo_em0.variables[options.verify_field][0]
      xlat = geo_em0.variables["XLAT_M"][0]
      xlon = geo_em0.variables["XLONG_M"][0]
      for ax_proj in all_ax:
        ax_proj.scatter(
                        x=xlon,
                        y=xlat,
                        c=data,
                        cmap=options.cmap,
                        s=options.verify_cellsize / dom_ratios[dom_id],
                        marker="s",
                        alpha=.5,
                        vmin=options.vmin,
                        vmax=options.vmax,
                        transform=ccrs.PlateCarree()
                        )

  non_interactive = [ "agg", "pdf", "ps", "svg", "pgf", "cairo" ]
  can_display = True
  if mpl.get_backend().lower() in non_interactive:
    can_display = False
    print( f"No support for interactive plotting from matplotlib, saving to {options.save_file}")
    plt.savefig( options.save_file )

  if can_display:
    plt.show( )
