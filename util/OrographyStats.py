import numpy as np


class OrographyStats:
  
  def __init__( self, box ):
    self.box_  = box

    self.mean_ = np.mean( self.box_ )                                           #< mean
    self.std_  = np.std ( self.box_, mean=self.mean_ )                          #< var (actulally stddev)

    #< Critical height used in calculation of orographic effective length
    self.hc_   = 1116.2 - 0.878 * self.std_

    # This currently does not use the landuse to average over only land and zero
    # out on mostly water
    if self.std_ < 1.0:                                                         #< con (convexity)
      self.con_ = 0.0
    else:
      var4 = np.sum( [ ( h - self.mean_ )**4 for h in np.nditer( box ) ] )
      self.con_ = var4 / ( self.box_.size * self.std_**4 )
    
    self.oa_ = np.zeros( (4) )                                                  #< oa (orographic asymmetry)
    self.calc_oa()

    self.ol_ = np.zeros( (4) )                                                  # ol (orographic effective length)
    self.calc_ol()

  def __repr__( self ):
    return f"Mean : {self.mean_} Std : {self.std_} CON : {self.con_} OA : {self.oa_} OL : {self.ol_}"

  def calc_oa( self ):
    # Note that right now the assumption is that the box is laid out in col major order
    # with [y,x] indices

    # oa1 is the orographic asymmetry in the West direction
    nu = np.sum( self.box_[:,:int(self.box_.shape[1]/2)] > self.mean_ )
    nd = np.sum( self.box_[:,int(self.box_.shape[1]/2):] > self.mean_ )
    self.oa_[0] = ( nu -nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # oa2 is the orographic asymmetry in the South direction
    nu = np.sum( self.box_[int(self.box_.shape[0]/2):,:] > self.mean_ )
    nd = np.sum( self.box_[:int(self.box_.shape[0]/2),:] > self.mean_ )
    self.oa_[1] = ( nu -nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # Pre-compute the geometric diagonal of the box
    slope = self.box_.shape[1] / self.box_.shape[0]
    j, i  = np.indices( self.box_.shape )
    upstream = np.flip( i <= ( j * slope ), axis=0 )

    # oa3 is the orographic asymmetry in the South-West direction
    nu = np.sum( self.box_[upstream] > self.mean_ )
    nd = np.sum( self.box_[~upstream] > self.mean_ )
    self.oa_[2] = ( nu -nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # oa4 is the orographic asymmetry in the North-West direction
    upstream = np.flip( upstream, axis=0 )
    nu = np.sum( self.box_[upstream] > self.mean_ )
    nd = np.sum( self.box_[~upstream] > self.mean_ )
    self.oa_[3] = ( nu -nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

  def calc_ol( self ):

    # ol1 is the effective orographic length in the West direction
    interior = self.box_[int(np.floor(self.box_.shape[0]*.25)):int(np.ceil(self.box_.shape[0]*.75)),:]
    self.ol_[0] = np.sum( interior > self.hc_ ) / interior.size

    # ol2 is the effective orographic length in the South direction
    interior = self.box_[:,int(np.floor(self.box_.shape[1]*.25)):int(np.ceil(self.box_.shape[1]*.75))]
    self.ol_[1] = np.sum( interior > self.hc_ ) / interior.size

    # The prescribed methodology uses 4 quadrants to get the diagonals and effectively
    # test half of the box, however this does not actually test the interior half
    # of the area of the box in the wind direction...

    # ol3 is the effective orographic length in the South-West direction
    interiorA = self.box_[int(self.box_.shape[0]/2):,int(self.box_.shape[1]/2):] # first half of x first half of y
    interiorB = self.box_[:int(self.box_.shape[0]/2),:int(self.box_.shape[1]/2)] # second half of x second half of y

    self.ol_[2] = ( np.sum( interiorA > self.hc_ ) + np.sum( interiorB > self.hc_ ) ) / ( interiorA.size + interiorB.size )

    # ol4 is the effective orographic length in the North-West direction
    interiorA = self.box_[int(self.box_.shape[0]/2):,:int(self.box_.shape[1]/2)] # first half of x second half of y
    interiorB = self.box_[:int(self.box_.shape[0]/2),int(self.box_.shape[1]/2):] # second half of x first half of y

    self.ol_[3] = ( np.sum( interiorA > self.hc_ ) + np.sum( interiorB > self.hc_ ) ) / ( interiorA.size + interiorB.size )
