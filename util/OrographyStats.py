import numpy as np


class OrographyStats:
  def __init__( self, box ):
    self.box  = box

    self.mean = np.mean( self.box )
    # var (actually stddev)
    self.std  = np.std( self.box )
    self.max = np.max( box )

    # This currently does not use the landuse to average over only land and zero
    # out on mostly water
    # con (convexity)
    if self.std < 1.0:
      self.con = 0.0
    else:
      var4 = np.sum( [ ( h - self.mean )**4 for h in np.nditer( box ) ] )
      self.con = var4 / ( self.box.size * self.std**4 )

    # oa (orographic asymmetry)
    self.oa = np.zeros( (4) )
    self.calc_oa()

    # ol (orographic effective length)
    self.ol = np.zeros( (4) )
    self.calc_ol()

  def __repr__( self ):
    return f"Mean : {self.mean} Std : {self.std} Max: {self.max} CON : {self.con} OA : {self.oa} OL : {self.ol}"

  def calc_oa( self ):
    # Note that right now the assumption is that the box is laid out in col major order
    # with [y,x] indices

    # oa1 is the orographic asymmetry in the West direction
    nu = np.sum( self.box[:, :int((self.box.shape[1] + self.box.shape[1] % 2) / 2)] > self.mean )
    nd = np.sum( self.box[:, int((self.box.shape[1] - self.box.shape[1] % 2) / 2):] > self.mean )
    self.oa[0] = ( nu - nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # oa2 is the orographic asymmetry in the South direction
    nu = np.sum( self.box[:int((self.box.shape[0] + self.box.shape[0] % 2) / 2), :] > self.mean )
    nd = np.sum( self.box[int((self.box.shape[0] - self.box.shape[0] % 2) / 2):, :] > self.mean )
    self.oa[1] = ( nu - nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # Pre-compute the geometric diagonal of the box
    slope = self.box.shape[0] / self.box.shape[1]
    j, i  = np.indices( self.box.shape )
    # Corrected - all indices that lie on diagonal are counted in both upstream and downstream
    vals = ( i + 1 ) * slope - ( self.box.shape[0] - j )
    upstream   = ( vals <= slope )
    downstream = ( vals >= -1.0 )

    # MPAS calcs - slightly accounts for diagonal when i ~= j but increasingly off for larger discrepancies
    # vals = np.rint( ( i + 1 ) * slope ) - ( self.box.shape[0] - j )
    # upstream   = ( vals <= 0 )
    # downstream = ( vals >= 0 )

    # oa3 is the orographic asymmetry in the South-West direction
    nu = np.sum( self.box[upstream] > self.mean )
    nd = np.sum( self.box[downstream] > self.mean )
    self.oa[2] = ( nu - nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

    # oa4 is the orographic asymmetry in the North-West direction
    upstream   = np.flip( upstream, axis=0 )
    downstream = np.flip( downstream, axis=0 )
    nu = np.sum( self.box[upstream] > self.mean )
    nd = np.sum( self.box[downstream] > self.mean )
    self.oa[3] = ( nu - nd ) / ( nu + nd ) if ( ( nu + nd ) > 0 ) else 0.0

  def calc_ol( self ):
    # ol1 is the effective orographic length in the West direction
    interior = self.box[int(np.floor(self.box.shape[0] * .25)):int(np.ceil(self.box.shape[0] * .75)), :]
    self.ol[0] = np.sum( interior > self.mean ) / interior.size

    # ol2 is the effective orographic length in the South direction
    interior = self.box[:, int(np.floor(self.box.shape[1] * .25)):int(np.ceil(self.box.shape[1] * .75))]
    self.ol[1] = np.sum( interior > self.mean ) / interior.size

    # The prescribed methodology uses 4 quadrants to get the diagonals and effectively
    # test half of the box, however this does not actually test the interior half
    # of the area of the box in the wind direction...

    # ol3 is the effective orographic length in the South-West direction
    interiorA = self.box[:int(self.box.shape[0] / 2), :int(self.box.shape[1] / 2)]  # first half of x first half of y
    interiorB = self.box[int(self.box.shape[0] / 2):, int(self.box.shape[1] / 2):]  # second half of x second half of y

    self.ol[2] = ( np.sum( interiorA > self.mean ) + np.sum( interiorB > self.mean ) ) / ( interiorA.size + interiorB.size )

    # ol4 is the effective orographic length in the North-West direction
    interiorA = self.box[int(self.box.shape[0] / 2):, :int(self.box.shape[1] / 2)]  # first half of x second half of y
    interiorB = self.box[:int(self.box.shape[0] / 2), int(self.box.shape[1] / 2):]  # second half of x first half of y

    self.ol[3] = ( np.sum( interiorA > self.mean ) + np.sum( interiorB > self.mean ) ) / ( interiorA.size + interiorB.size )
