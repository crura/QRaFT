;
; V. Uritsky 2021
;

function PSI_convert, Arr, R, T, P, mode ;,fname_fits ; Arr: 3D array in spherical coord.
; mode = 0 : returns LOS-integrated projection
; mode =1 : returns central cross-section perpendicular to LOS

 ;if n_elements(fname) eq 0 then begin

 ;  cd, "c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI"

 ;  fname = "PSI_Data.sav"

 ;endif

 ;restore, fname, /ver

 xyz_step = 0.10 ; ;0.05  Rs
 ;xyz_min = -15.0 ; Rs
 ;xyz_max = +15.0 ; Rs
 xyz_min = -5.0 ; Rs
 xyz_max = +5.0 ; Rs

 ; ------------
 t_tilt = 0.0 ;0.1
 p_tilt = 0.0 ;0.1
 ; ------------

 N = (xyz_max - xyz_min)/xyz_step + 1

 arr_xyz = fltarr(N,N,N)
 n_xyz = lon64arr(N,N,N)

 r_min = 1.1
 r_max = xyz_max
 wr = where((r gt r_min) and (r lt r_max))
 Nr = n_elements(r[wr])
 Nt = n_elements(t)
 Np = n_elements(p)
 r = double(r)
 t = double(t) ;+ t_tilt ; when adding the tilt angles, enforce correct dynamic ranges of both angles !!!
 p = double(p) ;+ p_tilt ; same

 p_xyz = fltarr(N,N,N)
 t_xyz = fltarr(N,N,N)

 ;t_ = shift(t, 100) ;; shift ???

 t_ = t

 ;------------------ main loop --------------------------

 for i=0, Nr-1 do begin

   if i mod 10 eq 0 then begin
     print, (100*i)/Nr,'%'
     wait,0.1
   endif

   for j=0, Nt-1 do $
     for k = 0, Np-1 do begin

 	   x = r[wr[i]]*sin(t_[j])*cos(p[k])
 	   y = r[wr[i]]*sin(t_[j])*sin(p[k])
 	   z = r[wr[i]]*cos(t_[j])

	   x_ = (x - xyz_min)/xyz_step
	   y_ = (y - xyz_min)/xyz_step
	   z_ = (z - xyz_min)/xyz_step

	   t_xyz[x_,y_,z_] = t_[j]
	   p_xyz[x_,y_,z_] = p[k]
	   arr_xyz[x_,y_,z_] = arr_xyz[x_,y_,z_] + arr[i,j,k]
	   n_xyz[x_,y_,z_] = n_xyz[x_,y_,z_] + 1

     endfor ; k
  endfor ; i
  ;------------------------------------------------------

  ; --- computing average values, filling empty pixels with 0s ---
  arr_avr_xyz = arr_xyz/(1.0*n_xyz)
  w = where(~finite(arr_avr_xyz))
  if w[0] ne -1 then arr_avr_xyz[w] = 0.0


  ; --- constructing image plane projection ---
  ; LOS || X axis
  arr_proj_yz = reform(arr_avr_xyz[0,*,*]) & arr_proj_yz[*]=0.0
  for y_=0, N-1 do $
    for z_=0, N-1 do $
      arr_proj_yz[y_,z_] = total(arr_avr_xyz[*,y_,z_])

  arr_cut_yz = reform(arr_avr_xyz[N/2,*,*])

  for y_=0, N-1 do for z_=0,N-1 do $
    if ( (xyz_step*y_+xyz_min)^2 + (xyz_step*z_+xyz_min)^2 )^0.5 lt r_min then begin
      arr_proj_yz[y_,z_]=0.0
      arr_cut_yz[y_,z_]=0.0
	endif

  ; --- plotting ---
  window, 0, xsize=512, ysize=512, title='central cross-section'
  tvscl, smooth(congrid(arr_cut_yz, 512, 512), [5,5])<1E8

  window, 1, xsize=512, ysize=512, title='LOS integral'
  tvscl, smooth(congrid(arr_proj_yz, 512, 512), [5,5])<1E10

  ; --- saving results ---
  ;save, filename="PSI_Data_xyz.sav", $
  ;      arr_xyz, n_xyz, arr_avr_xyz, arr_proj_yz, xyz_step, xyz_min, xyz_max, r_min, r_max, N


  if mode eq 0 then $
    data = smooth(congrid(arr_proj_yz, 512, 512), [5,5]) $
  else if mode eq 1 then $
    data = smooth(congrid(arr_cut_yz, 512, 512), [5,5])

  t_yz = smooth(congrid(reform(t_xyz[N/2,*,*]), 512, 512), [5,5])
  p_yz = smooth(congrid(reform(p_xyz[N/2,*,*]), 512, 512), [5,5])


  ;writefits, fname_fits, img

  ; 'psi_rho_proj_yz.fits'

  return, {data:data, t_yz:t_yz, p_yz:p_yz}

End

;------------------------------------------------------------

function vect_spherical_to_cartesian, Ar, At, Ap, t, p
  ;
  ; Computes Cartesian component of a vector field defined in spherical coordinates.
  ;
  ; Ar, At, Ap : arrays of spherical components of field A (== A_r, A_theta, A_phi)
  ; t, p: arrays of sperical angles (inclunation and azimuth)
  ;
  ; Dimension and size of all 5 arrays must be the same.
  ;
  ; Example:
  ;
  ; restore, "c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_2d_cuts.sav" , /ver
  ; B = vect_spherical_to_cartesian(Br_2d, Bth_2d, Bph_2d, th_2d, ph_2d )
  ; vel, B.y, B.z, nvecs = 5000  ; (plotting vectors at 5000 random locations)
  ;

  Ax = sin(t)*cos(p)*Ar + cos(t)*cos(p)*At - sin(p)*Ap
  Ay = sin(t)*sin(p)*Ar + cos(t)*sin(p)*At + cos(p)*Ap
  Az = cos(t)       *Ar - sin(t)       *At + 0     *Ap

  ; - matrix approach doesn't work, problem with dimensionality of terms
  ; M = [ [sin(t)*cos(p),  cos(t)*cos(p), - sin(p)], $
  ;      [sin(t)*sin(p),  cos(t)*sin(p),   cos(p)], $
  ;      [cos(t),         - sin(t),        0     ]]
  ;A = M#[Ar, At, Ap]

  return, {x:Ax, y:Ay, z:Az} ;, A:A}

End

;------------------------------------------------------------

function features_vs_B, features, B1, B2, angle_err_avr=angle_err_avr, angle_err_sd=angle_err_sd, angle_err_signed=angle_err_signed
  ; (B1,B2: in-plane components of B field, e.g. By, Bz),
  ; angle_err_avr : 1D array of mean angular errors

  N = n_elements(features)
  N_nodes_max = n_elements(features[0].angles_xx_r)
  angle_err = fltarr(N, N_nodes_max)
  angle_err_signed = fltarr(N, N_nodes_max)
  angle_err_signed_test = fltarr(N, N_nodes_max)

  ; ensuring that the  B-vector is always outward
  sz = size(B1, /dim)
  R1 = fltarr(sz[0],sz[1]) & R2 = R1 ; coordinate arrays
  for x=0, sz[0]-1 do $
    for y=0, sz[1]-1 do begin
      R1[x,y] = x - sz[0]/2
      R2[x,y] = y - sz[1]/2
    endfor
  mask = fix( (B1*R1 + B2*R2) gt 0 ) ; 2D binary array filled with 1s (0s) for outward (inward) B field
  w = where(mask le 0)
  if w[0] ne -1 then mask[w] = -1 ; 0-> -1
  B1_ = B1*mask & B2_ = B2*mask ; flipping the field where necessary

  for i=0, N-1 do begin

    xx = features[i].angles_xx_r[0:features[i].n_nodes-1]
    yy = features[i].angles_yy_r[0:features[i].n_nodes-1]

    for k=0, features[i].n_nodes-2 do begin

	  v1 = [features[i].xx_r[k+1]-features[i].xx_r[k], features[i].yy_r[k+1]-features[i].yy_r[k] ]
	  v2 = [B1[xx[k], yy[k]], B2[xx[k], yy[k]]  ]
	  v1_mag = sqrt(total(v1^2))
	  v2_mag = sqrt(total(v2^2))

	  d_angle = acos(total(v1*v2)/(v1_mag*v2_mag))
	  if d_angle gt !Pi/2 then d_angle =  !Pi - d_angle
	  angle_err[i,k] = d_angle

	  v2_ = [B1_[xx[k], yy[k]], B2_[xx[k], yy[k]]  ] ; outward B
	  v2_mag_ = sqrt(total(v2_^2))
	  d_angle_signed = asin( ( v1[0]*v2_[1] - v1[1]*v2_[0]  ) / (v1_mag*v2_mag_)  )
	  d_angle_signed_test = atan( v1[0]*v2_[1] - v1[1]*v2_[0], v1[0]*v2_[0] + v1[1]*v2_[1] );atan(v2_[1], v2_[0]) - atan(v1[1], v1[0])
	  angle_err_signed[i,k] = d_angle_signed
	  angle_err_signed_test[i,k] = d_angle_signed_test

	  ;x = xx[k]
      ;y = yy[k]
      ;B_angle = atan2(B2[x,y], B1[x,y])
      ;F_angle = features[i].angles[k]
      ;angle_err[i,k] = F_angle - B_angle
 	endfor
  endfor

  angle_err_avr = fltarr(N)
  angle_err_sd = fltarr(N)
  for i=0, N-1 do begin
    angle_err_avr[i] = mean(angle_err[i,0:features[i].n_nodes-1])
    angle_err_sd[i] = stddev(angle_err[i,0:features[i].n_nodes-1])
  endfor

  ;stop
  ;B = sqrt(B1^2 + B2^2) ; B magnitude for normalization
  ;w = where(B gt 0) &  mn =min( B[w] ) ; minimum non-0 value of B
  ;w_ = where(B eq 0) &  B[w_]=mn & B1[w_] = mn & B2[w_] = mn ; replace 0s with mn (not need to do that for B1 and B2)
  ;vel, B1/B, B2/B  ; plot normalized B vectors

  return, angle_err;{angle_err, angle_err_signed, angle_err_signed_test}

End

;------------------------------------------------------------

function get_2D_coord ;, Nxy, dx, dy, R_occult

  R_occult = 1.01
  dx = 9.0/256.0 & dy = dx
  Nxy = 256

  X0 = 0  & Y0 = 0; Sun's disk center

  X = findgen(Nxy)*(9.0/Nxy) - 4.5 + dx/2  ; grid node positions, centered
  Y = X

  i_arr = [0.0] & j_arr = i_arr
  N = 0LL

  for i =0, Nxy-1 do for j=0, Nxy-1 do $
    if ((X[i] - X0)^2 + (Y[j] - Y0)^2) gt R_occult^2 then begin
      N = N+1
      i_arr = [i_arr,i]
      j_arr = [j_arr,j]
    endif
  print,N

  return, {i:i_arr[1:*], j:j_arr[1:*], Nxy:Nxy, N:N}

End

;------------------------------------------------------------

function convert_psi_array, arr_1d

 map = get_2D_coord()

 arr_2d = dblarr(map.Nxy, map.Nxy)

 for k = 0LL, map.N-1 do arr_2d[map.i[k], map.j[k]] = arr_1d[k]

 return, transpose(arr_2d)

End

;------------------------------------------------------------

; restore, "c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\psimas_WL_PB_disk.sav"

; dens_2d = convert_psi_array(MODSOLSTRUCT.dens)

; pB = quantmap.data & w = where(pB lt 0) & pB[w]=0

;  window,0, xsize=256, ysize=256 & tvscl, dens_2D<1E6

;  window,1, xsize=256, ysize=256 & tvscl, pB<0.2

;------------------------------------------------------------

PRO coord_test

  restore, "c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\psimas_WL_PB_disk.sav"
  L0 = quantmap.L0[0]/180*!Pi ; defined as a 1-element array
  B0 = quantmap.B0/180*!Pi

  ; spherical coordinates of image plane pixels:
  rpos = gridpramsstruct.rpos
  thpos = gridpramsstruct.thpos
  phpos = gridpramsstruct.phpos

  ; Cartesian coordinates of image plane pixels:
  xpos = rpos*sin(thpos)*cos(phpos)
  ypos = rpos*sin(thpos)*sin(phpos)
  zpos = rpos*cos(thpos)

  ; Cartesian coordinates of pixels of the perpendiculr plane containing LOS:
  xpos_ = rpos*sin(thpos+B0)*cos(L0-!Pi/2)
  ypos_ = rpos*sin(thpos+B0)*sin(L0-!Pi/2)
  zpos_ = zpos


  ; preparing display image plane in 3D

  dxyz = 0.03 ; 3D grid resolution

  ; 3D array indices
  xpos_n = xpos/dxyz+150
  ypos_n = ypos/dxyz+150
  zpos_n = zpos/dxyz+150

  xpos_n_ = xpos_/dxyz+150
  ypos_n_ = ypos_/dxyz+150
  zpos_n_ = zpos_/dxyz+150

  A = fltarr(301,301,301)
  for i=0,255 do for j=0,255 do $
    if max([xpos_n[i,j], ypos_n[i,j], zpos_n[i,j]]) lt 301 $
    and min([xpos_n[i,j], ypos_n[i,j], zpos_n[i,j]]) ge 0 then $
      A[xpos_n[i,j], ypos_n[i,j], zpos_n[i,j]] = 1

  ; preparing 3D array to display the perpendicular image
  A_ = A
  for i=0,255 do for j=0,255 do $
    if max([xpos_n_[i,j], ypos_n_[i,j], zpos_n_[i,j]]) lt 301 $
    and min([xpos_n_[i,j], ypos_n_[i,j], zpos_n_[i,j]]) ge 0 then $
      A_[xpos_n_[i,j], ypos_n_[i,j], zpos_n_[i,j]] = 1


  ; displaying xy-crossections of the 3D volume at al z positions:
  for i=0,299 do begin & tvscl, A[*,*,i]+0.5*A_[*,*,i] & wait, 0.01 & endfor

  stop

End

;=====================

PRO convert_coaligned_PSI, dir

  	 ff = file_search(dir +"\*.sav" )
   	for i=0, n_elements(ff)-1 do begin




	endfor


End
