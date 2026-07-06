from collections import deque
import numpy as np


class TileData:
  def __init__( self, nx, ny, tile_x, tile_y, load_func ):
    # https://docs.python.org/3/faq/programming.html#how-do-i-create-a-multidimensional-list
    # Beware multiplying lists
    self._tiles = [ [None] * nx for y in range( ny ) ]
    self._nx     = nx
    self._ny     = ny
    self._tile_x = tile_x
    self._tile_y = tile_y
    self._load_func = load_func
    self._tile_cache = deque()
    self._max_cache  = 4
    self._debug      = False

  def print_present_tile_grid( self ):
    for tile_list in self._tiles:
      r = [ 1 if c is not None else 0 for c in tile_list ]
      print( r )

  def load_tile( self, tile_i, tile_j ):
    if len( self._tile_cache ) == self._max_cache:
      # make space for incoming tile
      tile = self._tile_cache.popleft()
      if self._debug:
        print( f"Removing tile {tile} from loaded tiles cache" )

      data = self._tiles[tile[0]][tile[1]]
      self._tiles[tile[0]][tile[1]] = None
      del data

    # Convert tile_i and tile_j to full ij index for the start of the tile
    # This assumes geogrid start of 1,1
    self.add_tile_by_index( tile_i, tile_j, self._load_func( tile_i * self._tile_x, tile_j * self._tile_y ) )

    self._tile_cache.append( ( tile_j, tile_i ) )

  def get_tile_index( self, i, j ):
    tile_i = int( i / self._tile_x )
    tile_j = int( j / self._tile_y )
    return tile_i, tile_j

  def add_tile_by_ij( self, i, j, data ):
    # i and j are raw indices
    tile_i, tile_j = self.get_tile_index( i, j )
    self.add_tile_by_tile_index( tile_i, tile_j, data )

  def add_tile_by_index( self, tile_i, tile_j, data ):
    self._tiles[tile_j][tile_i] = data

  def get_tiles( self, i, j, tile_indices=np.array([[0, 0]]) ):
    tiles = []
    tile_i, tile_j = self.get_tile_index( i, j )

    for ij in tiles:
      tiles.append( self.get_tile( tile_j + ij[1], tile_i + ij[0] ) )

    return tiles

  def get_tile_by_index( self, tile_i, tile_j ):
    """
    Go into the tiles and grab the corresponding tile, periodicity is resolved here
    """
    #  Lat-lon periodicity
    true_tile_i = tile_i % self._nx
    true_tile_j = tile_j
    rotate      = False

    if tile_j < 0 or tile_j > self._ny:
      # Flip at the poles so return a view of the data rotated 180
      true_tile_i = int( tile_i + self._nx / 2 ) % self._nx
      true_tile_j = self._ny - ( tile_j % ( self._ny + 1 ) )
      rotate      = True

    data = self._tiles[ true_tile_j ][ true_tile_i ]
    if data is None:
      if self._debug:
        print( f"Tile at {true_tile_i} {true_tile_j} is not loaded yet. Loading..." )
      self.load_tile( true_tile_i, true_tile_j )
      # Grab the data again
      data = self._tiles[ true_tile_j ][ true_tile_i ]
    else:
      if self._debug:
        print( f"Tile at {true_tile_i} {true_tile_j} is already loaded" )

    if rotate:
      if self._debug:
        print( f"Tile at {tile_i} {tile_j} must be rotated" )
      data = np.rot90( data, 2 )

    return data

  def get_box( self, indices ):
    # First transform indices to regions of distinct tiles by dividing the x and y
    # indices by the respective tile sizes
    as_tile_idx = np.floor(
                          np.einsum(
                                    "ijk, i -> ijk",
                                    indices,
                                    [ 1 / self._tile_y, 1 / self._tile_x ]
                                    )
                          ).astype( int )

    # Create our unique set of tiles needed and load them into an analogous structure
    # Naive creation by checking for uniqueness
    # uniq_tiles      = np.unique( np.unique( as_tile_idx, axis=1 ), axis=2 ).astype( int )

    # We know they will increase monotonically so just generate the range
    uniq_tiles = np.indices(
                              (
                                as_tile_idx[0, -1, 0] - as_tile_idx[0, 0, 0] + 1,
                                as_tile_idx[1, 0, -1] - as_tile_idx[1, 0, 0] + 1
                              )
                            )
    uniq_tiles[0] += as_tile_idx[0, 0, 0]
    uniq_tiles[1] += as_tile_idx[1, 0, 0]

    uniq_tiles_list = list( map( tuple, uniq_tiles.transpose( 1, 2, 0 ).reshape( int( uniq_tiles.size / 2 ), 2 ) ) )

    tiles       = {}
    for tile in uniq_tiles_list:
      tiles[tile] = self.get_tile_by_index( tile[1], tile[0] )

    box = np.zeros( indices[0].shape, dtype=next(iter(tiles.values())).dtype )
    # Now we have our tile set easily accessible based on the 1-to-1 mapping of indices to tile idx

    for tile in uniq_tiles_list:
      if self._debug:
        print( f"Processing tile {tile}")
      # Get the box indices corresponding to this tile
      box_indices = np.logical_and( as_tile_idx[0] == tile[0], as_tile_idx[1] == tile[1] )

      # Bulk assign to box this region of uniq tile by getting the relative index
      # into that tile from original indices
      box[box_indices] = tiles[tile][indices[0, box_indices] % self._tile_y, indices[1, box_indices] % self._tile_x]

    if self._debug:
      self.print_present_tile_grid()

    return box
