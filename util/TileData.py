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
  
  def print_present_tile_grid( self ):
    for l in self.tiles_:
      r = [ 1 if c is not None else 0 for c in l ]
      print( r )

  def load_tile( self, tile_i, tile_j ):
    # Convert tile_i and tile_j to full ij index for the start of the tile
    # This assumes geogrid start of 1,1
    self.add_tile_by_index( tile_i, tile_j, self.load_func_( tile_i * self.tile_x_, tile_j * self.tile_y_ ) )


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
      true_tile_i = ( tile_i + self.nx_ / 2 ) % self.nx_
      true_tile_j = self.ny_ - ( tile_j % ( self.ny_ + 1 ) )
      rotate      = True

    data = self.tiles_[ true_tile_j ][ true_tile_i ]
    if data is None:
      print( f"Tile at {true_tile_i} {true_tile_j} is not loaded yet. Loading..." )
      self.load_tile( true_tile_i, true_tile_j )
      # Grab the data again
      data = self.tiles_[ true_tile_j ][ true_tile_i ]
    else:
      print( f"Tile at {true_tile_i} {true_tile_j} is already loaded" )

  
    if rotate:
      data = np.rot90( data, 2 )

    return data

  def get_box( self, indices ):
    print( np.einsum( "ijk, i -> ijk", indices, [ 1/self.tile_y_, 1/self.tile_x_ ] )  )
    # First transform indices to regions of distinct tiles by dividing the x and y 
    # indices by the respective tile sizes
    as_tile_idx = np.floor( np.einsum( "ijk, i -> ijk", indices, [ 1/self.tile_y_, 1/self.tile_x_ ] ) ).astype( int )
    # Create our unique set of tiles needed and load them into an analogous structure
    uniq_tiles      = np.unique( np.unique( as_tile_idx, axis=1 ), axis=2 ).astype( int )
    uniq_tiles_list = list( map( tuple, uniq_tiles.transpose( 1, 2, 0 ).reshape( int( uniq_tiles.size / 2 ), 2 ) ) )
    print( uniq_tiles_list )
    tiles       = {}
    for tile in uniq_tiles_list:
      print( f"Getting data for tile {tile}" )
      tiles[tile] = self.get_tile_by_index( tile[1], tile[0] )
    
    box = np.zeros( indices[0].shape, dtype=next(iter(tiles.values())).dtype )
    # Now we have our tile set easily accessible based on the 1-to-1 mapping of indices to tile idx
    
    for j in range( indices.shape[1] ):
      for i in range( indices.shape[2] ):
        reli = indices[1,j,i] % self.tile_x_
        relj = indices[0,j,i] % self.tile_y_
        # print( f"i : {i} j : {j}")
        # print( f"reli : {reli} relj : {relj}" )
        # print( tiles[tuple(as_tile_idx[:,j,i])] )
        box[j,i] = tiles[tuple(as_tile_idx[:,j,i])][relj,reli]

    self.print_present_tile_grid()
    return box
