
PRO plot_B_lines, B1, B2, X=X, Y=Y, XYCenter = XYCenter, rho_min=rho_min 

  Nx = n_elements(B1[*,0]) & Ny = n_elements(B1[0,*])   
  if n_elements(Y) eq 0 then Y = findgen(Ny)-Ny/2
  if n_elements(X) eq 0 then X = findgen(Nx)-Nx/2
  
  if n_elements(XYCenter) eq 0 then XYCenter = [Nx,Ny]/2
  if n_elements(rho_min) eq 0 then rho_min = 110.0

  N_lines = 500  
  cong_size = [50,50]
  d_phi = 2.0


  ;----------------------------
  ; for field line tracing (original resolution, normalized):
  B1n = B1 & B2n = B2 & B1n[*] = 0 & B2n[*] = 0
  B = (B1^2 + B2^2)^0.5
  w = where(B ne 0)
  B1n[w] = B1[w]/B[w]
  B2n[w] = B2[w]/B[w]
   
  data = fltarr(2, Nx, Ny)
  data[0,*,*] = B1n
  data[1,*,*] = B2n
  ;----------------------------
  
  ;----------------------------
  ;; for velovect pl.ot (reduced resoluution, normalized):
  X_ = congrid(X, cong_size[0]) & Y_ = congrid(Y, cong_size[1])
  B1_ = congrid(B1, cong_size[0], cong_size[1])
  B2_ = congrid(B2, cong_size[0], cong_size[1])
  B1_n = B1_ & B2_n = B1_ & B1_n [*] = 0 & B2_n[*] = 0
  B_ = (B1_^2 + B2_^2)^0.5
  w = where(B_ ne 0)
  B1_n[w] = B1_[w]/B_[w]
  B2_n[w] = B2_[w]/B_[w]
  ;----------------------------
  
  velovect, B1_n, B2_n, X_, Y_, /isotr, /nodata
  velovect, B1_n, B2_n, X_, Y_, /isotr, /overplot, thick=1 , color=3

  ;seeds = randomu(s, 2, N_lines)*Nx
  
  phi_arr = findgen(360.0/d_phi)*(d_phi*!Pi/180.0)
  seeds_x = rho_min*cos(phi_arr) + XYCenter[0]
  seeds_y = rho_min*sin(phi_arr) + XYCenter[1]
  seeds = [transpose(seeds_x), transpose(seeds_y)] 

  particle_trace, data, seeds, verts, conn, MAX_ITERATIONS=Nx/2
  sz = SIZE(verts, /dim)
  i=0   
  while (i LT sz[1]) do begin
    nverts = conn[i]
    x_verts = verts[0, conn[i+1:i+nverts]]
    y_verts = verts[1, conn[i+1:i+nverts]]     
      plots, x_verts + min(X), y_verts + min(Y), thick=2
    i += nverts + 1 
  endwhile
  
  particle_trace, -data, seeds, verts, conn, MAX_ITERATIONS=Nx/2 ; repeat with inverted B directions for uniform coverage
  sz = SIZE(verts, /dim)
  i=0
  while (i LT sz[1]) do begin
    nverts = conn[i]
    x_verts = verts[0, conn[i+1:i+nverts]]
    y_verts = verts[1, conn[i+1:i+nverts]]
      plots, x_verts + min(X), y_verts + min(Y), thick=2
    i += nverts + 1
  endwhile

End



