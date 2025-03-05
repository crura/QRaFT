
function smooth_polar, img, smooth_xy
  ; x = azimuthal direction, cyclic
  ; y = radial direction
  
  img_ = smooth(img, [1, smooth_xy[1]], /edge_trunc)
  
  for y=0, n_elements(img[0,*])-1 do $
        img_[*,y] = smooth(img_[*,y], smooth_xy[0], /edge_wrap)  
    
  
  return, img_
  
End