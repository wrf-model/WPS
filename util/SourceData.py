import numpy as np
import cartopy

from TileData import TileData

def get_kv_pair( line, delimiter="=", comment="#" ):
  non_comment = line.split( comment, maxsplit=1 )[0]

  if delimiter in non_comment:
    key, _, value = non_comment.partition( delimiter )
    return key.strip(), value.strip()
  else:
    return None, None

class IndexData:

  def __init__( self, path ):
    self.source_path_      = path
    # Default values of index file from :
    # https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/wps.html#index-options
    self.projection_       = "required"
    self.source_type_      = "required"
    self.signed_           = False
    self.units_            = "required"
    self.description_      = "required"
    self.dx_               = -1.0       # required
    self.dy_               = -1.0       # required
    self.known_x_          =  1.0
    self.known_y_          =  1.0
    self.known_lat_        = -1.0       # required
    self.known_lon_        = -1.0       # required
    self.stdlon_           = -2.0       # not required
    self.truelat1_         = -2.0       # not required
    self.truelat2_         = -2.0       # not required
    self.wordsize_         = -1         # required
    self.tile_x_           = -1         # required
    self.tile_y_           = -1         # required
    self.tile_z_           = -2         # not required
    self.tile_z_start_     = -2         # not required
    self.tile_z_end_       = -2         # not required
    self.category_min_     = -2         # not required
    self.category_max_     = -2         # not required
    self.tile_bdr_         =  0
    self.missing_value_    = -2.0       # not required
    self.scale_factor_     =  1.0
    self.row_order_        = "bottom_top"
    self.endian_           = "big"
    self.iswater_          = 16
    self.islake_           = -1         # i.e. no separate inland water category (does not mean required)
    self.isice_            = 24
    self.isurban_          =  1
    self.isoilwater_       = 14
    self.mminlu_           = "USGS"
    self.filename_digits_  =  5

    self.read_file()

  def read_file( self ):
    with open( self.source_path_ + "/index" ) as f:
      for line in f:
        key, value = get_kv_pair( line )
        if key is not None:
          if   key == "projection"      : self.projection_      = value
          elif key == "source_type"     : self.source_type_     = value
          elif key == "signed"          : self.signed_          = ( value == "yes" )
          elif key == "units"           : self.units_           = value
          elif key == "description"     : self.description_     = value
          elif key == "dx"              : self.dx_              = float( value )
          elif key == "dy"              : self.dy_              = float( value )
          elif key == "known_x"         : self.known_x_         = float( value )
          elif key == "known_y"         : self.known_y_         = float( value )
          elif key == "known_lat"       : self.known_lat_       = float( value )
          elif key == "known_lon"       : self.known_lon_       = float( value )
          elif key == "stdlon"          : self.stdlon_          = float( value )
          elif key == "truelat1"        : self.truelat1_        = float( value )
          elif key == "truelat2"        : self.truelat2_        = float( value )
          elif key == "wordsize"        : self.wordsize_        = int( value )
          elif key == "tile_x"          : self.tile_x_          = int( value )
          elif key == "tile_y"          : self.tile_y_          = int( value )
          elif key == "tile_z"          : self.tile_z_          = int( value )
          elif key == "tile_z_start"    : self.tile_z_start_    = int( value )
          elif key == "tile_z_end"      : self.tile_z_end_      = int( value )
          elif key == "category_min"    : self.category_min_    = int( value )
          elif key == "category_max"    : self.category_max_    = int( value )
          elif key == "tile_bdr"        : self.tile_bdr_        = int( value )
          elif key == "missing_value"   : self.missing_value_   = float( value )
          elif key == "scale_factor"    : self.scale_factor_    = float( value )
          elif key == "row_order"       : self.row_order_       = value
          elif key == "endian"          : self.endian_          = value
          elif key == "iswater"         : self.iswater_         = int( value )
          elif key == "islake"          : self.islake_          = int( value )
          elif key == "isice"           : self.isice_           = int( value )
          elif key == "isurban"         : self.isurban_         = int( value )
          elif key == "isoilwater"      : self.isoilwater_      = int( value )
          elif key == "mminlu"          : self.mminlu_          = value
          elif key == "filename_digits" : self.filename_digits_ = int( value )
    
  def get_dtype( self ):
    endian = ">" if self.endian_ == "big" else "<"
    signed = "i" if self.signed_ else "u"
    return f"{endian}{signed}{self.wordsize_}"


class SourceData:
  
  def __init__( self, name, path ):
    self.earth_radius_ = 6371229.0
    self.name_         = name
    self.source_path_  = path
    self.index_        = IndexData( self.source_path_ )

    self.projection_   = None
    self.tile_data_    = None
    self.npts_x_       = int( 360.0 / self.index_.dx_ )
    self.pts_per_deg_  = int( 1.0 / self.index_.dx_ )
    self.subgrid_m_dx_ = 2.0 * np.pi * self.earth_radius_ / self.npts_x_

    self.data_         = None


    # These are all probably wrong
    if self.index_.projection_ == "lambert":
      self.projection_ = cartopy.crs.LambertConformal(
                                                      central_longitude  =self.index_.known_lon_,
                                                      central_latitude   =self.index_.known_lat_,
                                                      standard_parallels =( self.index_.truelat1_, self.index_.truelat2_ )
                                                      )
    elif self.index_.projection_ == "polar_wgs84" or self.index_.projection_ == "polar":
      self.projection_ = cartopy.crs.Stereographic(
                                                    central_longitude   =self.index_.known_lon_,
                                                    central_latitude    =self.index_.known_lat_,
                                                    true_scale_latitude =self.index_.truelat1_
                                                    )
    elif self.index_.projection_ == "albers_nad83":
      self.projection_ = cartopy.crs.AlbersEqualArea(
                                                    central_longitude   =self.index_.known_lon_,
                                                    central_latitude    =self.index_.known_lat_,
                                                    standard_parallels =( self.index_.truelat1_, self.index_.truelat2_ )
                                                    )
    elif self.index_.projection_ == "mercator":
      self.projection_ = cartopy.crs.Mercator(
                                              central_longitude   =self.index_.known_lon_,
                                              # central_latitude    =self.index_.known_lat_,
                                              latitude_true_scale =self.index_.truelat1_
                                              )
    elif self.index_.projection_ == "regular_ll":
      self.projection_ = cartopy.crs.LambertCylindrical( central_longitude   =self.index_.known_lon_ )
      self.tile_data_  = TileData(
                                  int( int( 360.0 / self.index_.dx_ ) / self.index_.tile_x_ ),
                                  int( int( 180.0 / self.index_.dy_ ) / self.index_.tile_y_ ),
                                  self.index_.tile_x_,
                                  self.index_.tile_y_,
                                  load_func=lambda i, j:
                                    self.read_geogrid(
                                                      self.get_tile_name_ij( i, j )[0] )[ # Limit the view to just the tile data for now, remove border
                                                                                          0,
                                                                                          self.index_.tile_bdr_:self.index_.tile_y_+self.index_.tile_bdr_,
                                                                                          self.index_.tile_bdr_:self.index_.tile_x_+self.index_.tile_bdr_
                                                                                          ]
                                  )


  def read_geogrid( self, file ):
    """
    Read in the geogrid raw data using the format provided here:
    https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/wps.html#writing-static-data-to-the-geogrid-binary-format

    Note that data passed out is assumed to be oriented as follows:
      SW
            <- -x               +x -> (increasing longitude)
         ^  +---+---+---+---+---+---+
         |  |   |   |   |   |   |   |
        -y  +---+---+---+---+---+---+
            |   |   |   |   |   |   |
            +---+---+---+---+---+---+
            |   |   |   |   |   |   |
            +---+---+---+---+---+---+
            |   |   |   |   |   |   |
        +y  +---+---+---+---+---+---+
         |  |   |   |   |   |   |   |
         v  +---+---+---+---+---+---+
        (increasing latitude)
                                      NE
    """
    # print( "Reading " + file )
    rawdata = np.fromfile( 
                          file,
                          dtype=self.index_.get_dtype()
                          )
    data = rawdata.astype( np.float32, casting="unsafe" ) * self.index_.scale_factor_
    z_dim = 1
    if self.index_.tile_z_start_ > 0 and self.index_.tile_z_end_ > 0:
      z_dim = self.index_.tile_z_end_ - self.index_.tile_z_start_
    elif self.index_.tile_z_ > 0:
      z_dim = self.index_.tile_z_

    data = data.reshape(
                        (
                          z_dim,
                          self.index_.tile_y_ + 2 * self.index_.tile_bdr_,
                          self.index_.tile_x_ + 2 * self.index_.tile_bdr_
                        )
                       )
    if self.index_.row_order_ == "top_bottom":
      data = np.flip( data, axis=1 )
    
    return data

  def latlon_to_ij( self, lat, lon ):
    # Ignore staggering for now
    i = 0.0
    j = 0.0
    if self.index_.projection_ == "regular_ll":
      delta_lat = lat - self.index_.known_lat_
      delta_lon = lon - self.index_.known_lon_

      i = ( delta_lon / self.index_.dx_ + self.index_.known_x_ ) % int( 360.0 / self.index_.dx_ )
      j = ( delta_lat / self.index_.dy_ + self.index_.known_y_ )

    return i, j

  def ij_to_latlon( self, i, j ):
    lat = 0.0
    lon = 0.0
    if self.index_.projection_ == "regular_ll":
      lon = ( i - self.index_.known_x_ ) * self.index_.dx_ + self.index_.known_lon_
      lat = ( j - self.index_.known_y_ ) * self.index_.dy_ + self.index_.known_lat_

    if lon > 180.0:
      lon -= 360.0
    
    return lat, lon

  def get_tile_extent( self, starti, startj ):
    start_lat, start_lon = self.ij_to_latlon( starti - self.index_.tile_bdr_, startj - self.index_.tile_bdr_ )
    stop_lat,  stop_lon  = self.ij_to_latlon( starti + self.index_.tile_x_ - 1 + self.index_.tile_bdr_, startj + self.index_.tile_y_ - 1  + self.index_.tile_bdr_)
    print( ( starti - self.index_.tile_bdr_, startj - self.index_.tile_bdr_ ) )
    print( ( starti + self.index_.tile_x_ - 1 + self.index_.tile_bdr_, startj + self.index_.tile_y_ - 1  + self.index_.tile_bdr_ ) )
    return ( start_lon, stop_lon, start_lat, stop_lat )

  def get_tile_name_ll( self, lat, lon ):
    i, j = self.latlon_to_ij( lat, lon )
    return self.get_tile_name_ij( i, j )

  def get_tile_name_ij( self, i, j ):
    tile_i = self.index_.tile_x_ * int( int( i ) / self.index_.tile_x_ ) + 1
    tile_j = self.index_.tile_y_ * int( int( j ) / self.index_.tile_y_ ) + 1

    path = f"{self.source_path_}/{{xstart:0{self.index_.filename_digits_}d}}-{{xstop:0{self.index_.filename_digits_}d}}.{{ystart:0{self.index_.filename_digits_}d}}-{{ystop:0{self.index_.filename_digits_}d}}"
    return path.format(
                        xstart=tile_i,
                        xstop =tile_i+self.index_.tile_x_-1,
                        ystart=tile_j,
                        ystop =tile_j+self.index_.tile_y_-1
                        ), tile_i, tile_j


  def get_box( self, lat, lon, size ):
    # X span in points
    nx = 0
    if ( np.cos( np.deg2rad( lat ) ) > ( 2.0 * self.pts_per_deg_ * size * 180.0 ) / ( self.npts_x_ * np.pi * self.earth_radius_ ) ):
      nx = int( np.ceil( ( 180.0 * size * self.pts_per_deg_ ) / ( np.pi * self.earth_radius_ * np.cos( np.deg2rad( lat ) ) ) ) )
    else:
      nx = int( self.npts_x_ / 2 )

    # Y span in points
    ny = int( np.ceil( ( 180.0 * size * self.pts_per_deg_ ) / ( np.pi * self.earth_radius_ ) ) )


    ####################################################################################################################
    ####################################################################################################################
    ####################################################################################################################
    # ## Just load the tile in for now
    true_i, true_j = self.latlon_to_ij( lat, lon )
    # Generate the indices for this box regardless of tile periodicity, let the tile data handle that
    indices = np.indices( ( ny, nx ) )
    indices[0] += int( true_j - ny / 2 )
    indices[1] += int( true_i - nx / 2 )

    box = self.tile_data_.get_box( indices )
    ####################################################################################################################
    ####################################################################################################################
    ####################################################################################################################

    return box
