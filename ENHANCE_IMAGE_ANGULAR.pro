function ENHANCE_IMAGE_ANGULAR, IMG, XYCenter, R_mask, order=order

  ;+
  ; NAME:
  ;     ENHANCE_IMAGE_ANGULAR
  ;
  ; PURPOSE:
  ;     Enhances quasi-radial structures in a solar coronal image by performing:
  ;       1) radial image detrending
  ;       2) 1st or 2nd order differening in the angular direction.
  ;
  ;     The angular differencing is done by subtraciting a rotated copy of the image.
  ;     In the present version, the rotation agle is preset to +/- 1 degree based on empirical tests.
  ;
  ; CALLING SEQUENCE:
  ;     IMG_enh = ENHANCE_IMAGE_ANGULAR(IMG, XYCenter, R_mask, order=)
  ;
  ; INPUTS:
  ;     IMG       - 2D array representing the studied solar image
  ;     XYCenter  - 2-element array containing X = XYCenter[0] and Y = XYCenter[1] positions
  ;                 of the Sun center in image IMG, in pixel units.
  ;     R_mask    - the smallest radial distance (in pixel units) from the image center to be included
  ;                 in the enhanced image. Pixels located at smaller radial distances are set to 0.
  ;                 Used to remove contaminations caused by the edge of the solar disk of the coronagraph occulting disk.
  ;     order     - order of the angular differencing; allowed values:
  ;                   order = 1  - first-order differences
  ;                   order = 2  - second-order differences
  ;                 Optional; the default value is 1 (1st order differenceing).
  ;
  ; OUTPUTS:
  ;     IMG_enh   - the enhanced version of the image IMG
  ;
  ; RESTRICTIONS:
  ;     none
  ;
  ; EXAMPLE:
  ;     IMG_enh = ENHANCE_IMAGE_ANGULAR(IMG, [513,513], 205)
  ;
  ; MODIFICATION HISTORY:
  ;       V. Uritsky, 2019-2022
  ;-
  ;----------------------------------------------------------------

  if n_elements(order) eq 0 then order=1 ; order of differentiations
  ;if n_elements(R_mask) eq 0 then R_mask = 195 ; radius of coronagraph mask removed

  Nx = n_elements(img[*,0]) & Ny = n_elements(img[0,*])
  ;if n_elements(x0) eq 0 then x0=Nx/2 +1
  ;if n_elements(y0) eq 0 then y0=Ny/2 +1 ; coordinates of the solar disc center in MLSO images (513, 513)
  if n_elements(XYCenter) eq 0 then XYCenter=[Nx/2 + 1, Ny/2 + 1]
  x0 = XYCenter[0] & y0 = XYCenter[1]

  ;detrended image

  img_detr = radial_detrending(img, XYCenter)

  ; first-order angular differencing
  img_diff_p  = (img_detr - rot(img_detr, +0.5,  1, x0, y0, /interp, /pivot))
  img_diff_n  = (img_detr - rot(img_detr,  -0.5,  1, x0, y0, /interp, /pivot))

  img_diff_abs = smooth(abs(img_diff_p-img_diff_n),[1,1]) ; unsiged 1st derivative
 ; img_diff_abs = img_diff_p-img_diff_n   							   ; siged 1st derivative

  img_enh = img_diff_abs

  ;img_diff_abs = img_diff_abs * (img_diff_abs gt 30)

  if order eq 2 then begin

    img_diff2_p  = (img_diff_abs- rot(img_diff_abs, +0.5,  1, x0, y0, /interp, /pivot))
    img_diff2_n  = (img_diff_abs- rot(img_diff_abs, -0.5,  1, x0, y0, /interp, /pivot))

    img_diff2_abs = abs(img_diff2_p-img_diff2_n)   ; unsiged 2nd derivative
    ; img_diff2_abs = img_diff2_p-img_diff2_n 		  ;  siged 2nd derivative

    img_enh = img_diff2_abs

  endif

  for x=0,Nx-1 do for y=0,Ny-1 do begin & r = sqrt((x-x0)^2.0+(y-y0)^2.0) & if r lt (R_mask+1) then img_enh[x,y]=0 & endfor
  ;stop

  return, img_enh

End
