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
    self.source_path      = path
    # Default values of index file from :
    # https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/wps.html#index-options
    self.projection       = "required"
    self.source_type      = "required"
    self.signed           = False
    self.units            = "required"
    self.description      = "required"
    self.dx               = -1.0       # required
    self.dy               = -1.0       # required
    self.known_x          =  1.0
    self.known_y          =  1.0
    self.known_lat        = -1.0       # required
    self.known_lon        = -1.0       # required
    self.stdlon           = -2.0       # not required
    self.truelat1         = -2.0       # not required
    self.truelat2         = -2.0       # not required
    self.wordsize         = -1         # required
    self.tile_x           = -1         # required
    self.tile_y           = -1         # required
    self.tile_z           = -2         # not required
    self.tile_z_start     = -2         # not required
    self.tile_z_end       = -2         # not required
    self.category_min     = -2         # not required
    self.category_max     = -2         # not required
    self.tile_bdr         =  0
    self.missing_value    = -2.0       # not required
    self.scale_factor     =  1.0
    self.row_order        = "bottom_top"
    self.endian           = "big"
    self.iswater          = 16
    self.islake           = -1         # i.e. no separate inland water category (does not mean required)
    self.isice            = 24
    self.isurban          =  1
    self.isoilwater       = 14
    self.mminlu           = "USGS"
    self.filename_digits  =  5

    self.read_file()

  def read_file( self ):
    with open( self.source_path + "/index" ) as f:
      for line in f:
        key, value = get_kv_pair( line )
        if key is not None:
          if   key == "projection":      self.projection      = value
          elif key == "source_type":     self.source_type     = value
          elif key == "signed":          self.signed          = ( value == "yes" )
          elif key == "units":           self.units           = value
          elif key == "description":     self.description     = value
          elif key == "dx":              self.dx              = float( value )
          elif key == "dy":              self.dy              = float( value )
          elif key == "known_x":         self.known_x         = float( value )
          elif key == "known_y":         self.known_y         = float( value )
          elif key == "known_lat":       self.known_lat       = float( value )
          elif key == "known_lon":       self.known_lon       = float( value )
          elif key == "stdlon":          self.stdlon          = float( value )
          elif key == "truelat1":        self.truelat1        = float( value )
          elif key == "truelat2":        self.truelat2        = float( value )
          elif key == "wordsize":        self.wordsize        = int( value )
          elif key == "tile_x":          self.tile_x          = int( value )
          elif key == "tile_y":          self.tile_y          = int( value )
          elif key == "tile_z":          self.tile_z          = int( value )
          elif key == "tile_z_start":    self.tile_z_start    = int( value )
          elif key == "tile_z_end":      self.tile_z_end      = int( value )
          elif key == "category_min":    self.category_min    = int( value )
          elif key == "category_max":    self.category_max    = int( value )
          elif key == "tile_bdr":        self.tile_bdr        = int( value )
          elif key == "missing_value":   self.missing_value   = float( value )
          elif key == "scale_factor":    self.scale_factor    = float( value )
          elif key == "row_order":       self.row_order       = value
          elif key == "endian":          self.endian          = value
          elif key == "iswater":         self.iswater         = int( value )
          elif key == "islake":          self.islake          = int( value )
          elif key == "isice":           self.isice           = int( value )
          elif key == "isurban":         self.isurban         = int( value )
          elif key == "isoilwater":      self.isoilwater      = int( value )
          elif key == "mminlu":          self.mminlu          = value
          elif key == "filename_digits": self.filename_digits = int( value )

  def get_dtype( self ):
    endian = ">" if self.endian == "big" else "<"
    signed = "i" if self.signed else "u"
    return f"{endian}{signed}{self.wordsize}"


class SourceData:
  def __init__( self, name, path ):
    self._earth_radius = 6370000.0 # Use the same earth radius as used in geogrid
    self._name         = name
    self._source_path  = path
    self._index        = IndexData( self._source_path )

    self._projection   = None
    self._npts_x       = int( 360.0 / self._index.dx )
    self._npts_y       = int( 180.0 / self._index.dy )
    self._pts_per_deg  = int( 1.0 / self._index.dx )
    self._subgrid_m_dx = 2.0 * np.pi * self._earth_radius / self._npts_x

    self._tile_data  = TileData(
                                int( self._npts_x / self._index.tile_x ),
                                int( self._npts_y / self._index.tile_y ),
                                self._index.tile_x,
                                self._index.tile_y,
                                load_func=lambda i, j:
                                  self.read_geogrid(
                                                    # Limit the view to just the tile data for now, remove border
                                                    self.get_tile_name_ij( i, j )[0] )[
                                                                                        0,
                                                                                        self._index.tile_bdr:self._index.tile_y + self._index.tile_bdr,
                                                                                        self._index.tile_bdr:self._index.tile_x + self._index.tile_bdr
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
    rawdata = np.fromfile(
                          file,
                          dtype=self._index.get_dtype()
                          )
    data = rawdata.astype( np.float32, casting="unsafe" ) * self._index.scale_factor
    z_dim = 1
    if self._index.tile_z_start > 0 and self._index.tile_z_end > 0:
      z_dim = self._index.tile_z_end - self._index.tile_z_start
    elif self._index.tile_z > 0:
      z_dim = self._index.tile_z

    data = data.reshape(
                        (
                          z_dim,
                          self._index.tile_y + 2 * self._index.tile_bdr,
                          self._index.tile_x + 2 * self._index.tile_bdr
                        )
                       )
    if self._index.row_order == "top_bottom":
      data = np.flip( data, axis=1 )

    return data

  def latlon_to_ij( self, lat, lon ):
    # Ignore staggering for now
    i = 0.0
    j = 0.0
    if self._index.projection == "regular_ll":
      delta_lat = lat - self._index.known_lat
      delta_lon = lon - self._index.known_lon

      i = ( delta_lon / self._index.dx + self._index.known_x ) % self._npts_x
      j = ( delta_lat / self._index.dy + self._index.known_y )

    return i, j

  def ij_to_latlon( self, i, j ):
    lat = 0.0
    lon = 0.0
    if self._index.projection == "regular_ll":
      lon = ( i - self._index.known_x ) * self._index.dx + self._index.known_lon
      lat = ( j - self._index.known_y ) * self._index.dy + self._index.known_lat

    if lon > 180.0:
      lon -= 360.0
    
    return lat, lon

  def get_tile_extent( self, starti, startj ):
    start_lat, start_lon = self.ij_to_latlon( starti - self._index.tile_bdr, startj - self._index.tile_bdr )
    stop_lat, stop_lon   = self.ij_to_latlon( starti + self._index.tile_x - 1 + self._index.tile_bdr, startj + self._index.tile_y - 1  + self._index.tile_bdr )
    print( ( starti - self._index.tile_bdr, startj - self._index.tile_bdr ) )
    print( ( starti + self._index.tile_x - 1 + self._index.tile_bdr, startj + self._index.tile_y - 1  + self._index.tile_bdr ) )
    return ( start_lon, stop_lon, start_lat, stop_lat )

  def get_tile_name_ll( self, lat, lon ):
    i, j = self.latlon_to_ij( lat, lon )
    return self.get_tile_name_ij( i, j )

  def get_tile_name_ij( self, i, j ):
    tile_i = self._index.tile_x * int( int( i ) / self._index.tile_x ) + 1
    tile_j = self._index.tile_y * int( int( j ) / self._index.tile_y ) + 1

    path  = f"{self._source_path}/"
    path += f"{{xstart:0{self._index.filename_digits}d}}-{{xstop:0{self._index.filename_digits}d}}."
    path += f"{{ystart:0{self._index.filename_digits}d}}-{{ystop:0{self._index.filename_digits}d}}"
    return path.format(
                        xstart=tile_i,
                         xstop=tile_i + self._index.tile_x - 1,
                        ystart=tile_j,
                         ystop=tile_j + self._index.tile_y - 1
                        ), tile_i, tile_j

  def get_box( self, lat, lon, size_x, size_y=None ):
    if size_y is None:
      size_y = size_x

    # X span in points
    nx = 0
    if (
        np.cos( np.deg2rad( lat ) )
        > ( 2.0 * self._pts_per_deg * size_x * 180.0 ) / ( self._npts_x * np.pi * self._earth_radius )
        ):
      nx = int(
                np.ceil(
                        ( 180.0 * size_x * self._pts_per_deg )
                        / ( np.pi * self._earth_radius * np.cos( np.deg2rad( lat ) ) )
                        )
                )
    else:
      nx = int( self._npts_x / 2 )

    # Y span in points
    ny = int( np.ceil( ( 180.0 * size_y * self._pts_per_deg ) / ( np.pi * self._earth_radius ) ) )

    true_i, true_j = self.latlon_to_ij( lat, lon )
    # Generate the indices for this box regardless of tile periodicity, let the tile data handle that
    indices = np.indices( ( ny, nx ) )
    indices[0] += int( true_j - ny / 2 )
    indices[1] += int( true_i - nx / 2 )

    box = self._tile_data.get_box( indices )
    # box is now a 2D box of data spanning size in meters centered on lat/lon
    return box
