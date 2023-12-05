
; s = '5\' & fname = file_search(dr+s+'*ne.f*') & fname_B1 = file_search(dr+s+'\*By.f*') & fname_B2 = file_search(dr+s+'*Bz.f*')
; qraft_test,1, fname, fname_B1, fname_B2, 110

PRO QRaFT_TEST, key, fname, fname_B1, fname_B2, rho_min
  
  if n_elements(key) eq 0 then key=1
    
  if key eq 1 then begin
    ; PSI ne
    ;fname = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_central_plane\__2017_08_29__COR1__PSI_ne.fits"
    if n_elements(fname) eq 0 then begin
      dir = "/Users/crura/Desktop/Research/github/Test_Suite/Image-Coalignment/QRaFT/3.0_PSI_Tests"
      fname = dir + "/__2017_09_11__COR1__PSI_ne_LOS.fits" 
      fname_B1 = dir + "/__2017_09_11__COR1__PSI_By.fits"  
      fname_B2 = dir + "/__2017_09_11__COR1__PSI_Bz.fits"
      rho_min = 110.0
    endif
    ;rho_min = 90.0
  endif
  
  if key eq 2 then begin
    ; COR1
    if n_elements(fname) eq 0 then begin
      dir = "/Users/crura/Desktop/Research/github/Test_Suite/Image-Coalignment/QRaFT/3.0_PSI_Tests"
      fname = dir + "/COR1/2017_09_11_rep_med.fts" 
      fname_B1 = dir + "/__2017_09_11__COR1__PSI_By.fits"  
      fname_B2 = dir + "/__2017_09_11__COR1__PSI_Bz.fits"
      rho_min = 110.0
    endif
  endif
  
  if key eq 3 then begin
    ; PSI ne
    ;fname = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_central_plane\__2017_08_29__COR1__PSI_ne.fits"
    if n_elements(fname) eq 0 then begin
      dir = "/Users/crura/Desktop/Research/github/Test_Suite/Image-Coalignment/QRaFT/3.0_PSI_Tests"
      fname = dir + "/__2017_09_11__KCor__PSI_ne_LOS.fits"
      fname_B1 = dir + "/__2017_09_11__KCor__PSI_By.fits"
      fname_B2 = dir + "/__2017_09_11__KCor__PSI__PSI_Bz.fits"
      rho_min = 220.0
    endif
    ;rho_min = 90.0
  endif

  ;-------- opening file -------------
    
  IMG_orig = readfits(fname, header, exten_no=exten_no)
  sz = size(IMG_orig, /dim) & Nx = sz[0] & Ny = sz[1]
  IMG_orig = congrid(IMG_orig, 512, 512) & IMG_orig = congrid(IMG_orig, Nx, Ny)

  ;-------- image processing constants ----------------

  ; Tunable parameters:
  ;   
  XYCenter = [Nx, Ny] / 2.0
  d_phi = 2*0.00872665
  d_rho = 2*1.0 ; 2*1.0
  rot_angle = 2.5 
  phi_shift = 2.0 ; 2.0 

  smooth_xy = 12 ;[5,5]
  smooth_phi_rho = [5,8] ;[3,3]
  detr_phi = 10 ;10

  rho_range = [rho_min, min([Nx/2, Ny/2])]/d_rho
  n_rho = 20 ; number of rho_min levels used for tracing 
  
  p_range = [0.90, 0.99]
  n_p = 10  ; number of probability levels

  n_nodes_min = 10

  ;-------- IMAGE PREPROCESSING -------------

  ; -------------------------------------------
  ; 1. Initial processing in rect. coordinates
  ; -------------------------------------------
  ; 
  ; initial smoothing: 
  ;IMG_orig = smooth(IMG_orig, smooth_xy)
  IMG_orig = median(IMG_orig, smooth_xy)
  ; detrending:  
  IMG_orig = radial_detrending(abs(IMG_orig), XYCenter)
  ; removing near-Sun region:
  for i=0,Nx-1 do for k=0, Ny-1 do $
    if (i - XYCenter[0])^2 + (k - XYCenter[1])^2 lt rho_min^2 then IMG_orig[i,k] = 0.0
  ; 2nd phi-detrivatinve in rect. coord or plotting purpose only:  
  IMG_d2_phi_r = abs(rot(IMG_orig,  +rot_angle,  1, XYCenter[0], XYCenter[1], /interp, /pivot) + rot(IMG_orig,  -rot_angle,  1, XYCenter[0], XYCenter[1], /interp, /pivot) - 2*IMG_orig)
  X = findgen(Nx) - XYCenter[0]
  Y = findgen(Ny) - XYCenter[1]
  
  ; -------------------------------------------
  ; 2. Main processing in polar coordinates
  ; -------------------------------------------
  
  ; Rectang. to polar transform
  Rect_to_Polar, img_orig, XYcenter, d_phi, d_rho, IMG_orig_p, Rho_range=Rho_range, X_p=X_p, Y_p=Y_p, Phi_p=Phi_p, Rho_p=Rho_p
  
  ; phi and rho coordinate arrays for polotting: 
  Xp = findgen(n_elements(IMG_orig_p[*,0]))*d_phi
  Yp = findgen(n_elements(IMG_orig_p[0,*]))*d_rho + Rho_range[0]

  
  ; Fill binning gaps (if any) in polar image:
  IMG_orig_p_ = patch_image_holes(IMG_orig_p, count=count) ; help, count
  
  ; Smoothing
  IMG_orig_p_ = smooth(IMG_orig_p_, smooth_phi_rho)
  
  ; 2-step 2nd order differentiation with smoothing after 1st derivative
  
  ;IMG_d1_phi_ = shift(IMG_orig_p_, [phi_shift,0]) - IMG_orig_p_
  ;IMG_d1_phi_ = smooth(IMG_d1_phi_, smooth_phi_rho)
  ;IMG_d2_phi_ = shift(IMG_d1_phi_, [phi_shift,0]) - IMG_d1_phi_
  
  ; 2nd phi-derivative in polar coordinates
  IMG_d2_phi_ = shift(IMG_orig_p_, [phi_shift,0]) + shift(IMG_orig_p_, [-phi_shift,0]) - 2*IMG_orig_p_
  
  
  ; absolute value:
  IMG_d2_phi_enh = abs(IMG_d2_phi_)
    
  ; phi - detrending: 
  avr_d2_phi_ = mean(IMG_d2_phi_enh, dim=2)
  smooth_d2_phi_ = IMG_d2_phi_
  for i=0, n_elements(smooth_d2_phi_[*,0])-1 do smooth_d2_phi_[i,*] = avr_d2_phi_[i]
  smooth_d2_phi_ = smooth(smooth_d2_phi_, [detr_phi,1])    
  IMG_d2_phi_enh = abs(IMG_d2_phi_/smooth_d2_phi_)

  ; -------------------------------------------
  ; 3. Blob detection and interpolation
  ; -------------------------------------------
  ;  
  ; Array of percintile thresholds:
  p_arr = findgen(n_p)*(p_range[1]-p_range[0])/(n_p-1 - 0) + p_range[0]
  ;p_arr = [0.90, 0.92, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99]

  ; Array of min. rho levels:

  rho_min_arr = 1 + fix(n_elements(IMG_d2_phi_enh[0,*])*(4.0/5.0)*findgen(n_rho)/float(n_rho) )  
  ;rho_min_arr = [1, 2, 5, 10, 15, 20, 25]
  ;rho_min_arr = [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]

  ; blob detection
  IMG_lbl_arr = detect_blobs(IMG_d2_phi_enh, p_arr, rho_min_arr, 5, blob_stat=blob_stat, blob_indices=blob_indices)
  
  
    ; TESTING SHAPES OF FEATURES IN RECTANG COORD:
    ;window,1, xsize=1000, ysize=1000 & loadct,0 & erase & image_plot_1, IMG_orig, X, Y
    ; *** IMG_d2_phi_r = abs(rot(IMG_orig,  +rot_angle,  1, XYCenter[0], XYCenter[1], /interp, /pivot) + rot(IMG_orig,  -rot_angle,  1, XYCenter[0], XYCenter[1], /interp, /pivot) - 2*IMG_orig)        
    
    ; Comapting with POSI magnetic fueld line orientation
    
    if key le 1  then begin ; PSI model only
      
      B1 = readfits(fname_B1) & B2 = readfits(fname_B2)
      
      features = blob_stat_to_features(blob_stat, d_phi, d_rho, rho_min, XYCenter)
       
      ; removing short features
      w = where(features[*].n_nodes gt n_nodes_min)
      features = features[w]
        
      angle_err = features_vs_B(features, B1, B2, angle_err_avr=angle_err_avr, angle_err_sd=angle_err_sd, angle_err_signed=angle_err_signed)

      ;-------------------------------------
      ; saving main resutls:
      fname_save = fname+'.sav'      
      ;w = where((angle_err ne 0) and finite(angle_err) )
      ;w_ = where((angle_err_signed ne 0) and finite(angle_err_signed))            
      save, filename = fname_save, features, angle_err, angle_err_signed, IMG_d2_phi_r, blob_stat, blob_indices
      ;-------------------------------------
      
    endif
   
  
  ; ------- GRAPHICS ---------------------------
  ;     
  !P.charsize=1.3
  !P.noerase=1

  ; --------------------------------------------
  ;window, 1, xsize=3000, ysize=1200
  window, 1, xsize=1500, ysize=600
  loadct, 0 & erase
  image_plot_1, IMG_d2_phi_enh, range=[0, adapt_thresh_prob(IMG_d2_phi_enh, p=0.95)]
  setcolors
  for i=0, n_elements(rho_min_arr)-1 do $
    for k = 0, n_elements(p_arr)-1 do begin
    i1=blob_indices[0,k,i] & i2=blob_indices[1,k,i]
    plots, blob_stat.phi_fit[i1:i2,*], blob_stat.rho[i1:i2,*], psym=4, color=2
  endfor
  ; --------------------------------------------
  window, 2, xsize=1200, ysize=800, xpos = 1900
  loadct, 0
  erase
  pos = getpos(2,2,xy=[0.15, 0.06], region=[0.03,0.03,0.9,0.97])
    ;image_plot_1, IMG_d2_phi_r, X, Y, pos=pos[0,0,*], range=[0, adapt_thresh_prob(IMG_d2_phi_r, p=0.97)]
    image_plot_1, IMG_orig, X, Y, pos=pos[0,0,*], range=[0, adapt_thresh_prob(IMG_orig, p=0.95)]
    image_plot_1, IMG_orig_p_, Xp, Yp, pos=pos[0,1,*], $
      title='IMG_orig_p_', xtitle='phi (rad)', ytitle='rho (pix)'
    image_plot_1, abs(IMG_d2_phi_), Xp, Yp, range=[0,1]*adapt_thresh_prob(abs(IMG_d2_phi_), p_arr=0.95), pos=pos[1,0,*], $
      title = 'abs(IMG_d2_phi_)';, xtitle='phi (rad)'
    image_plot_1, IMG_d2_phi_enh, range = [0,1]*adapt_thresh_prob(IMG_d2_phi_enh, p=0.95), Xp, Yp, pos=pos[1,1,*], $
      title = 'IMG_d2_phi_enh', xtitle='phi (rad)', ytitle='rho (pix)'
          
    ;image_plot_1, IMG_d2_phi_enh_lbl, Xp, Yp, pos=pos[1,1,*], ctable=13, $
    ;  title = 'IMG_d2_phi_enh_lbl', xtitle='phi (rad)'

  ; --------------------------------------------
  window, 3, xsize=1200, ysize=800, xpos = 1900
  loadct, 0
  erase
  pos = getpos(3,2,xy=[0.11, 0.05], region=[0.0,0.1,0.85,0.9])
      
    for i=0, 1 do $
      for k=0, 2 do begin
        title = 'IMG_lbl_arr' + '   p='+str(p_arr[i]) + '  rho_min='+str(rho_min_arr[k])
        image_plot_1, IMG_lbl_arr[*,*,i,k], Xp, Yp, pos=pos[k,i,*], ctable=13, title=title
        
      endfor
  ; --------------------------------------------    

  window, 4, xsize=1000, ysize=1000 & loadct,0 & erase
  image_plot_1, IMG_d2_phi_r, X, Y, range=[0, adapt_thresh_prob(IMG_d2_phi_r, p=0.95)]
  setcolors
  for i=0, n_elements(blob_stat.length)-1 do begin & l=blob_stat.length[i] & phi= d_phi*blob_stat.phi_fit[i,0:l-1] & rho= d_rho*blob_stat.rho[i,0:l-1] + rho_min & xx_r = rho*cos(phi) &  yy_r = rho*sin(phi) & plots, [xx_r, yy_r], color=2, thick=2 & endfor


  ; --------------------------------------------

  ; B-field lines vs detected features; statistics of discrepancy angles:
  if key le 1 then begin ; PSI model only
    
    window, 5, xsize=1200, ysize=600 & loadct,0 & erase
    !P.noerase=0
    !P.multi = [0,2,1]
    setcolors
    plot_B_lines, B1, B2, XYCenter=XYCenter, rho_min=rho_min
    oplot_features, features, XYCenter
    plots, [0], [0], psym=4, thick=2
    
    w = where((angle_err ne 0) and finite(angle_err) )
    w_ = where((angle_err_signed ne 0) and finite(angle_err_signed))
    
    hy = histogram(angle_err[w]*180/!Pi, nbins=100, loc=hx)/float(n_elements(w))
    hy_signed = histogram(angle_err_signed[w_]*180/!Pi, nbins=200, loc=hx_signed)/float(n_elements(w_))
    plot, hx, hy, title = 'Mean='+str(mean(angle_err[w]*180/!Pi))+' Median='+str(median(angle_err[w]*180/!Pi)), thick=4, /xstyle, xrange=[-45,45]
    oplot, hx_signed, hy_signed, thick=2
    oplot, [0,0], !y.crange, lines=2
    


    !P.multi = 0
  endif

  ; --------------------------------------------

  !P.noerase=0

  return


  ;------- PREVIOUS TESTS -----------------------  

  ; POLAR COORDS BY TRIANGULATION
  ; (cartesian -> polar coord. transformation using triangulation)
  ; RESULTS WORSE THAN WITH MY DIRECT TRANSFORMAITON "Rect_to_Polar".
  ; Rect_to_Polar_, img_orig, XYcenter, d_phi, d_rho, IMG_orig_p_tr, Phi_p=Phi_tr, Rho_p=Rho_tr
  ; IMG_orig_p_tr = smooth(IMG_orig_p_tr, [3,3])
  ; wset, 2 & erase & image_plot_1, IMG_orig_p_tr, Phi_tr, Rho_tr
  ; IMG_d2_phi_tr = shift( IMG_orig_p_tr, [phi_shift,0]) + shift(IMG_orig_p_tr, [-phi_shift,0]) - 2*IMG_orig_p_tr

  ; FEATURES USIGN OLD METHODS
  ;th_arr = adapt_thresh_prob(q)
  ;i = 8
  ;poly_order = 2
  ;FEATURES = Polyfit_decomp(q, [th_arr[i], 10*max(th_arr)], 5, 3, poly_order, /tr)
  ;FEATURES=Compute_Feature_Coord(FEATURES, XYcenter, X_p, Y_p, Phi_p, Rho_p)
  ;P = {XYCenter:XYCenter, d_phi:d_phi, d_rho:d_rho}
  ;wset, 7 & erase
  ;PLOT_FEATURES, q, FEATURES, P, range=[th_arr[i], max(th_arr)], /polar
  
  ; PLOTTING CONTOURS
  ;window, 3, xsize=1000, ysize=1000 & erase
  ;!P.noerase=1
  ;image_plot_1, IMG_p, Xp, Yp, pos=position, plottitle='IMG_p', range=minmax(thresh_arr)
  ;setcolors
  ;w = where(thresh_arr gt 0)
  ;for i=0, n_elements(w)-1 do $
  ;  contour, IMG_p, Xp, Yp, /overplot, levels = thresh_arr[w[i]],  pos=position, color=i


  
End