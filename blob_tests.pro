
function detect_blobs, IMG, p_arr, rho_min_arr, min_blob, blob_stat_merged=blob_stat_merged, blob_indices=blob_indices
   
   
   sz = size(IMG,/dim)
   
   IMG_lbl_arr = fltarr(sz[0], sz[1], n_elements(p_arr), n_elements(rho_min_arr))  
   blob_indices = intarr(2,n_elements(p_arr), n_elements(rho_min_arr))
   IMG_trimmed = IMG
   
   blob_ind_1 = 0
   
   for i=0, n_elements(rho_min_arr)-1 do begin
     IMG_trimmed[*, 0:rho_min_arr[i]] = 0
 
     for k = 0, n_elements(p_arr)-1 do begin
     
       status = label_arr_(IMG_trimmed, adapt_thresh_prob(IMG_trimmed, p=p_arr[k]), lbl=lbl, h=h, $
                           blob_stat=blob_stat, IMG_lbl=IMG_lbl)       
       
       IMG_relbl = IMG_lbl
       w = where(IMG_lbl ne -1)
       IMG_relbl[w] = IMG_relbl[w] + blob_ind_1 
       IMG_lbl_arr[*,*,k,i] = IMG_relbl
       
       blob_stat_merged = merge_blob_stat_(blob_stat_merged, blob_stat) 
       
       blob_ind_2 = blob_ind_1 + n_elements(blob_stat.area) - 1       
       blob_indices[*,k,i] = [blob_ind_1, blob_ind_2]
       blob_ind_1 = blob_ind_2 + 1
     
     endfor
     
   endfor
        
    ; window, 0, xsize=3000, ysize=1200
    ; loadct, 0 & erase
    ; image_plot_1, IMG, range=[0, adapt_thresh_prob(IMG, p=0.95)]
    ; setcolors
    ; for i=0, n_elements(rho_min_arr)-1 do $
    ;   for k = 0, n_elements(p_arr)-1 do begin
    ;     i1=blob_indices[0,k,i] & i2=blob_indices[1,k,i] 
    ;     plots, blob_stat_merged.phi_fit[i1:i2,*], blob_stat_merged.rho[i1:i2,*], psym=4, color=2 
    ;   endfor     
    ; stop
    
   return, IMG_lbl_arr
   
End

;=============================

function label_arr_, IMG_p, thresh, lbl=lbl, h=h, blob_stat=blob_stat, $
                     IMG_lbl=IMG_lbl, IMG_p_clr=IMG_p_clr, $
                     width_range=width_range, length_range=length_range
  ; returns array of detected clusters, each labeled with random number in range 0 ... 256 for color plotting
  
  if n_elements(width_range) eq 0 then width_range=[2, 10] 
  if n_elements(length_range) eq 0 then length_range=[10, 30];1E10]
  
  min_length = width_range[0]
  
  ; can add eccentricity
  
  IMG_lbl = long(label_region(IMG_p  gt thresh)) - 1
  
  
  blob_stat = measure_blobs_(IMG_lbl)

  status = validate_blobs_(IMG_lbl, blob_stat, width_range=width_range, length_range=length_range)
  
  rnd = randomu(seed, max(IMG_lbl)+1)
  IMG_p_clr = IMG_p
  for i=0, max(IMG_lbl) do begin
    w = where(IMG_lbl eq i)
    IMG_p_clr[w] = rnd(i)*256
  endfor 
    
  return, status
  
End

;=============================

function measure_blobs_, IMG_lbl, min_length=min_length, poly_order=poly_order
  ; IMG_lbl: labeled image
  
  if n_elements(min_length) eq 0 then min_length = 5 
  if n_elements(poly_order) eq 0 then poly_order = 2;3
  
  
  phi_2D = fix(IMG_lbl) & phi_2D[*]=0 & rho_2D = phi_2D 
  mask = phi_2D
  for i = 0, n_elements(phi_2D[*,0])-1 do phi_2D[i,*] = i
  for k = 0, n_elements(rho_2D[0,*])-1 do rho_2D[*,k] = k

  n_blobs = max(IMG_lbl)+1 ; counting 0th blob
  n_rho = n_elements(IMG_lbl[0,*])
  
  width = fltarr(n_blobs)
  length = fltarr(n_blobs)
  area = fltarr(n_blobs)

  phi_arr = fltarr(n_blobs, n_rho)
  phi_fit_arr = fltarr(n_blobs, n_rho)
  rho_arr = fltarr(n_blobs, n_rho)
  status_arr = intarr(n_blobs)

  for i=0, n_blobs-1 do begin
    w = where(IMG_lbl eq i)
    mask[*] = 0 & mask[w] = 1

    width[i] = max(total(mask, 1))
    length[i] = max(rho_2D[w]) - min(rho_2D[w]) + 1
    area[i] = n_elements(w)

    if length[i] ge min_length then begin
      n = total(mask, 1) & xsum = total(phi_2D*mask, 1) & ww = where(n gt 0)
      phi = xsum[ww]/n[ww]
      rho = min(rho_2D[w]) + findgen(length[i])

      fit = poly_fit(rho, phi, poly_order, status=status)
      phi_fit = fltarr(n_elements(phi)) & for k=0, poly_order do phi_fit = phi_fit + fit[k]*(rho^k)

      phi_arr[i,0:length[i]-1] = phi
      phi_fit_arr[i,0:length[i]-1] = phi_fit
      rho_arr[i,0:length[i]-1] = rho
      status_arr[i] = status

      ; Visualize each blob:
      ; 
      ;erase & plot, rho_2D[w], phi_2D[w], psym=4, xrange = minmax(rho_2D[w])+ [-10,10], $
      ;  yrange = minmax(phi_2D[w])+ [-20,20], xtitle='rho', ytitle='phi'
      ;oplot, rho, phi, thick=1
      ;oplot, rho, phi_fit, thick=3
      ;stop

    endif

  endfor

  blob_stat = {width:width, length:length, area:area, phi:phi_arr, phi_fit:phi_fit_arr, rho:rho_arr, $
    status:status_arr}

  return, blob_stat
  
End

;=============================

function validate_blobs_, IMG_lbl, blob_stat, width_range=width_range, length_range=length_range
  
  IMG_lbl_ = IMG_lbl & IMG_lbl_[*] = 0
  
  ;***i_ = 0L
  w_valid = []
  for i=0, max(IMG_lbl)-1 do begin
    
    w = where(IMG_lbl eq i)
    
    if (blob_stat.width[i]  ge width_range[0]) and $
       (blob_stat.width[i]  le width_range[1]) and $
       (blob_stat.length[i] ge length_range[0]) and $
       (blob_stat.length[i] le length_range[1]) then begin 
              
       ;***IMG_lbl_[w] = i_
       ;***i_ = i_ + 1
       w_valid = [w_valid, i]
                   
     endif 
        
  endfor
  
  if n_elements(w_valid) eq 0 then $ ' no valid features
    return, -1
    
    IMG_lbl_[*] = -1
    for i=0, n_elements(w_valid)-1 do begin
      w = where(IMG_lbl eq w_valid[i])
      IMG_lbl_[w] = i
    endfor
  
    blob_stat_ = {width:blob_stat.width[w_valid], length:blob_stat.length[w_valid], area:blob_stat.area[w_valid], $
                  phi:blob_stat.phi[w_valid,*], phi_fit:blob_stat.phi_fit[w_valid,*], rho:blob_stat.rho[w_valid,*], $
                  status:blob_stat.status[w_valid]}
  
    IMG_lbl = IMG_lbl_
    blob_stat = blob_stat_
  
    return, 1
  
  
End

;=============================

function merge_blob_stat_, bs1, bs2
  
  if n_elements(bs1) eq 0 then return, bs2
  
  width   = [bs1.width, bs2.width]
  length  = [bs1.length, bs2.length]
  area    = [bs1.area, bs2.area]
  phi     = [bs1.phi, bs2.phi]
  phi_fit = [bs1.phi_fit, bs2.phi_fit]
  rho     = [bs1.rho, bs2.rho]
  status  = [bs1.status, bs2.status]
  ; add more fields/tags as needed
  
  blob_stat = {width:width, length:length, area:area, phi:phi, phi_fit:phi_fit, rho:rho, $
    status:status}
    
  return, blob_stat
  
End  

;=============================
PRO plot_blob, IMG_lbl, blob_stat, blob_index

  phi_2D = fix(IMG_lbl) & phi_2D[*]=0 & rho_2D = phi_2D
  for i = 0, n_elements(phi_2D[*,0])-1 do phi_2D[i,*] = i
  for k = 0, n_elements(rho_2D[0,*])-1 do rho_2D[*,k] = k

  w = where(IMG_lbl eq blob_index)

  length = blob_stat.length[blob_index]
  phi = blob_stat.phi[blob_index, 0:length-1]
  rho = blob_stat.rho[blob_index, 0:length-1]
  phi_fit = blob_stat.phi_fit[blob_index, 0:length-1]

  rho_all = rho_2D[w]
  phi_all = phi_2D[w]

  rho_range = minmax(rho_all)+ [-10,10]
  phi_range = minmax(phi_all)+ [-20,20]

  ;X = findgen(n_elements(IMG[*,0])) & Y = findgen(n_elements(IMG[0,*]))
  ;X = X[phi_range[0]:phi_range[1]] & Y = Y[rho_range[0]:rho_range[1]]


  loadct, 0 & erase
  ;image_plot_1, transpose(IMG), Y, X
  setcolors
  plot, rho_all, phi_all, xtitle='rho', ytitle='phi', xrange=rho_range, yrange=phi_range, /nodata
  plots, rho_all, phi_all, psym=4, color=4
  oplot, rho, phi, thick=1, color = 5
  oplot, rho, phi_fit, thick=3, color=2

  return

End

;=============================


pro blob_plot_, IMG_lbl_arr, blob_stat_merged, blob_stat_indices, k, i, blob_ind
  
  ;i = 0 & k = 0 & blob_ind = 10 ;... test
  
  IMG_lbl = reform(IMG_lbl_arr[*,*,k,i])
  IMG_lbl[where (IMG_lbl ne blob_ind)]=0
  ;stop
  loadct, 0 & erase
  image_plot_1, IMG_lbl
  
  length = blob_stat_merged.length[blob_ind]
  phi     = blob_stat_merged.phi[blob_ind, 0:length-1]
  phi_fit = blob_stat_merged.phi_fit[blob_ind, 0:length-1]
  rho     = blob_stat_merged.rho[blob_ind, 0:length-1]
  
  setcolors
  plots, phi_fit, rho, color=2, psym=4
  ;print, phi_fit, rho
  
End
;=============================
