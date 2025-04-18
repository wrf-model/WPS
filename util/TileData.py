from collections import deque
import numpy as np

class TileData:

  def __init__( self, nx, ny, tile_x, tile_y, load_func ):
    # https://docs.python.org/3/faq/programming.html#how-do-i-create-a-multidimensional-list
    # Beware multiplying lists
    self.tiles_ = [ [None] * nx for y in range( ny ) ]
    self.nx_     = nx
    self.ny_     = ny
    self.tile_x_ = tile_x
    self.tile_y_ = tile_y
    self.load_func_ = load_func
    self.tile_cache_ = deque()
    self.max_cache_  = 4
    self.debug_      = False
  
  def print_present_tile_grid( self ):
    for l in self.tiles_:
      r = [ 1 if c is not None else 0 for c in l ]
      print( r )

  def load_tile( self, tile_i, tile_j ):
    if len( self.tile_cache_ ) == self.max_cache_:
      # make space for incoming tile
      tile = self.tile_cache_.popleft()
      if self.debug_: print( f"Removing tile {tile} from loaded tiles cache" )

      data = self.tiles_[tile[0]][tile[1]]
      self.tiles_[tile[0]][tile[1]] = None
      del data
      

    # Convert tile_i and tile_j to full ij index for the start of the tile
    # This assumes geogrid start of 1,1
    self.add_tile_by_index( tile_i, tile_j, self.load_func_( tile_i * self.tile_x_, tile_j * self.tile_y_ ) )

    self.tile_cache_.append( ( tile_j, tile_i ) )


  def get_tile_index( self, i, j ):
    tile_i = int( i / self.tile_x_ )
    tile_j = int( j / self.tile_y_ )
    return tile_i, tile_j

  def add_tile_by_ij( self, i, j, data ):
    # i and j are raw indices
    tile_i, tile_j = self.get_tile_index( i, j )
    self.add_tile_by_tile_index( tile_i,tile_j, data )
  
  def add_tile_by_index( self, tile_i, tile_j, data ):
    self.tiles_[tile_j][tile_i] = data

  def get_tiles( self, i, j, tile_indices=np.array([[0,0]]) ):
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
    true_tile_i = tile_i % self.nx_
    true_tile_j = tile_j
    rotate      = False

    if tile_j < 0 or tile_j > self.ny_:
      # Flip at the poles so return a view of the data rotated 180
      true_tile_i = int( tile_i + self.nx_ / 2 ) % self.nx_
      true_tile_j = self.ny_ - ( tile_j % ( self.ny_ + 1 ) )
      rotate      = True

    data = self.tiles_[ true_tile_j ][ true_tile_i ]
    if data is None:
      if self.debug_: print( f"Tile at {true_tile_i} {true_tile_j} is not loaded yet. Loading..." )
      self.load_tile( true_tile_i, true_tile_j )
      # Grab the data again
      data = self.tiles_[ true_tile_j ][ true_tile_i ]
    else:
      if self.debug_: print( f"Tile at {true_tile_i} {true_tile_j} is already loaded" )

  
    if rotate:
      if self.debug_: print( f"Tile at {tile_i} {tile_j} must be rotated" )
      data = np.rot90( data, 2 )

    return data

  def get_box( self, indices ):

    # First transform indices to regions of distinct tiles by dividing the x and y 
    # indices by the respective tile sizes
    as_tile_idx = np.floor( np.einsum( "ijk, i -> ijk", indices, [ 1/self.tile_y_, 1/self.tile_x_ ] ) ).astype( int )

    # Create our unique set of tiles needed and load them into an analogous structure
    # Naive creation by checking for uniqueness
    # uniq_tiles      = np.unique( np.unique( as_tile_idx, axis=1 ), axis=2 ).astype( int )
    # We know they will increase monotonically so just generate the range
    uniq_tiles = np.indices( ( as_tile_idx[0,-1,0] - as_tile_idx[0,0,0] + 1, as_tile_idx[1,0,-1] - as_tile_idx[1,0,0] + 1 ) )
    uniq_tiles[0] += as_tile_idx[0,0,0]
    uniq_tiles[1] += as_tile_idx[1,0,0]

    uniq_tiles_list = list( map( tuple, uniq_tiles.transpose( 1, 2, 0 ).reshape( int( uniq_tiles.size / 2 ), 2 ) ) )

    tiles       = {}
    for tile in uniq_tiles_list:
      tiles[tile] = self.get_tile_by_index( tile[1], tile[0] )
    
    box = np.zeros( indices[0].shape, dtype=next(iter(tiles.values())).dtype )
    # Now we have our tile set easily accessible based on the 1-to-1 mapping of indices to tile idx
    
    for tile in uniq_tiles_list:
      if self.debug_: print( f"Processing tile {tile}")
      # Get the box indices corresponding to this tile
      box_indices = np.logical_and( as_tile_idx[0] == tile[0], as_tile_idx[1] == tile[1] )

      # Bulk assign to box this region of uniq tile by getting the relative index 
      # into that tile from original indices
      box[box_indices] = tiles[tile][indices[0,box_indices] % self.tile_y_, indices[1,box_indices] % self.tile_x_]

    # Naively assign one-by-one
    # for j in range( indices.shape[1] ):
    #   for i in range( indices.shape[2] ):
    #     reli = indices[1,j,i] % self.tile_x_
    #     relj = indices[0,j,i] % self.tile_y_
    #     box[j,i] = tiles[tuple(as_tile_idx[:,j,i])][relj,reli]

    if self.debug_: self.print_present_tile_grid()

    return box
