
PRO script1

 features_cut = process_file("c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_RHO_cut.fits", 'PSI')

 features_proj = process_file("c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_RHO_proj.fits", 'PSI')

 restore, "c:\Users\Vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\PSI_2d_cuts.sav"

 B = vect_spherical_to_cartesian(Br_2d, Bth_2d, Bph_2d, th_2d, ph_2d)

 err_cut = features_vs_B(features_cut, B.y, B.z, angle_err_avr = err_avr_cut)
 err_proj = features_vs_B(features_proj, B.y, B.z, angle_err_avr = err_avr_proj)

 w_cut=where(features_cut.L gt 50)
 w_proj=where(features_proj.L gt 50)

 mean_large_cut = mean(err_avr_cut[w_cut])*180/!Pi
 mean_large_proj = mean(err_avr_proj[w_proj])*180/!Pi

 print, 'feature_cut vs B angle, L > 50: ' + string(mean_large_cut, format='(F6.2)') + ' degrees'

 print, 'feature_proj vs B angle, L > 50: '  + string(mean_large_proj, format='(F6.2)') + ' degrees'

 LL_cut = err_cut
  for i=0, n_elements(LL_cut[0,*])-1 do LL_cut[*,i] = features_cut.L
 LL_proj = err_proj
  for i=0, n_elements(LL_proj[0,*])-1 do LL_proj[*,i] = features_proj.L

 LL_cut_1d = [0.0]
 err_cut_1d = [0.0]
 LL_proj_1d = [0.0]
 err_proj_1d = [0.0]
 for i=0, n_elements(features_cut)-1 do begin
   n_nodes = features_cut[i].n_nodes
   LL_cut_1d = [LL_cut_1d, fltarr(n_nodes)+features_cut[i].L]
   err_cut_1d = [err_cut_1d, reform(err_cut[i,0:n_nodes-1])]
 endfor
 for i=0, n_elements(features_proj)-1 do begin
   n_nodes = features_proj[i].n_nodes
   LL_proj_1d = [LL_proj_1d, fltarr(n_nodes)+features_proj[i].L]
   err_proj_1d = [err_proj_1d, reform(err_proj[i,0:n_nodes-1])]
 endfor
LL_cut_1d = LL_cut_1d[0:*]
LL_proj_1d = LL_proj_1d[0:*]
err_cut_1d = err_cut_1d[0:*]
err_proj_1d = err_proj_1d[0:*]


 window, 3 & vel, B.y, B.z, nvecs=10000, title='B in central YZ plane'

 window, 4  & plot, LL_cut_1d, err_cut_1d*180/!Pi, psym=6, title='feature_cut vs B        L>50 mean: ' $
              + string(mean_large_cut, format='(F6.2)') + ' degrees', xtitle='Feature length, pixels', ytitle='Discrepancy angle, degrees'
 window, 5 & plot, LL_proj_1d, err_proj_1d*180/!Pi, psym=6, title='feature_proj vs B        L>50 mean: ' $
             + string(mean_large_proj, format='(F6.2)') + ' degrees', xtitle='Feature length, pixels', ytitle='Discrepancy angle, degrees'


End

;=================================

function compare_angles,  f_corona,  f_By, f_Bz, f_By_LOS, f_Bz_LOS, isplot=isplot, data_source=data_source, thresh_k=thresh_k, manual=manual

; examples:

; dir = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\"
; script2, dir + 'pB.fits', dir + 'By.fits', dir + 'Bz.fits', dir + 'By_LOS.fits', dir + 'Bz_LOS.fits', hist_x=hist_x_pB_, hist_y=hist_y_pB_
; script2, dir + '20170829_200801_kcor_l2_avg.fts', dir + 'By.fits', dir + 'Bz.fits', dir + 'By_LOS.fits', dir + 'Bz_LOS.fits', hist_x=hist_x_MLSO_, hist_y=hist_y_MLSO_


  ;dir = "c:\Users\Vadim\Documents\"

 ; fn_ne = dir + "SCIENCE PROJECTS\N Arge\PSI\FORWARD -  rotated\Integrated_Electron_Density.fits"
 ; fn_ne_center = dir + "SCIENCE PROJECTS\N Arge\PSI\FORWARD -  rotated\Electron_Density_Center.fits"
 ; fn_pB = dir + "SCIENCE PROJECTS\N Arge\PSI\FORWARD -  rotated\Forward_PB_data.fits"

  ;fn_ne_coal = dir + "SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\FORWARD_INTEGRATED_ELECTRON_DENSITY_COALIGN.fits"
  ;fn_pB_coal = dir + "SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\FORWARD_PB_IMAGE_COALIGN.fits"
  ;fn_MLSO_coal = dir +  "SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\20170829_200801_kcor_l2_avg.fts"


  ;fn_B = dir + "SCIENCE PROJECTS\N Arge\PSI\FORWARD -  rotated\Forward_Rotated_B_xyz.sav"
  ;fn_B_LOS = dir + "SCIENCE PROJECTS\N Arge\PSI\FORWARD -  rotated\LOS_Integrated_B_xyz.sav"
  ;fn_B_coal  = dir + "SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\Central_B_Field_MLSO_Coaligned.sav"
  ;fn_B_LOS_coal  = dir + "SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\LOS_B_Field_MLSO_Coaligned.sav"

  ;f_corona = fn_ne_center
  ;f_magnetic = fn_B
  ;f_magnetic_LOS = fn_B_LOS

   ;f_corona = fn_MLSO_coal
   ;f_corona = fn_pB_coal
  ; f_corona = fn_ne_coal
   ;f_magnetic = fn_B_coal
   ;f_magnetic_LOS = fn_B_LOS_coal

  ;features = process_file(fn_ne, 'PSI', thresh_k = 1.0, IMG_enh=IMG_enh, P=P)
  ;features = process_file(fn_pB, 'PSI', thresh_k = 1.0, IMG_enh=IMG_enh, P=P)
  ;features = process_file(fn_ne_center, 'PSI',  thresh_k = 1.0, IMG_enh=IMG_enh, P=P)

  ;restore, f_magnetic
  ;restore, f_magnetic_LOS

  if n_elements(thresh_k) eq 0 then thresh_k=0.5
  if n_elements(data_source) eq 0 then data_source = 'PSI'

  ;---------------------------------
  By_2D = readfits(f_By)
  Bz_2D = readfits(f_Bz)
  LOS_integrated_By_2D = readfits(f_By_LOS)
  LOS_integrated_Bz_2D = readfits(f_Bz_LOS)


  if data_source eq 'MLSO2016' or data_source eq 'PSI_MLSO' then begin
    By = congrid(By_2D,1024,1024)
    Bz = congrid(Bz_2D,1024,1024)
    By_LOS = congrid(LOS_integrated_By_2D,1024,1024)
    Bz_LOS = congrid(LOS_integrated_Bz_2D,1024,1024)

    By_rnd = randomu(seed1, 1024, 1024)
    Bz_rnd = randomu(seed2, 1024, 1024)

  endif else begin
    By = congrid(By_2D,512,512)
    Bz = congrid(Bz_2D,512,512)
    By_LOS = congrid(LOS_integrated_By_2D,512,512)
    Bz_LOS = congrid(LOS_integrated_Bz_2D,512,512)

    By_rnd = randomu(seed1, 512, 512)
    Bz_rnd = randomu(seed2, 512, 512)

  endelse


  ;By = congrid(By_2D,512,512)
  ;Bz = congrid(Bz_2D,512,512)
  ;By_LOS = congrid(LOS_integrated_By_2D,512,512)
  ;Bz_LOS = congrid(LOS_integrated_Bz_2D,512,512)

  if keyword_set(manual) then begin
    features = process_corona(f_corona,data_source, thresh_k=thresh_k, IMG_enh=IMG_enh, P=P, manual=manual)  ; using  'PSI' rescales MLSO image to match the B-field arrays
  endif else begin
    features = process_corona(f_corona,data_source, thresh_k=thresh_k, IMG_enh=IMG_enh, P=P)  ; using  'PSI' rescales MLSO image to match the B-field arrays
  endelse

  ;features = process_corona(f_corona,data_source, thresh_k=thresh_k, IMG_enh=IMG_enh, P=P, /old, /silent)  ; using  'PSI' rescales MLSO image to match the B-field arrays

 err = features_vs_B(features, By, Bz, angle_err_avr = err_avr, angle_err_sd = err_sd, angle_err_signed=err_signed)
 err_LOS = features_vs_B(features, By_LOS, Bz_LOS, angle_err_avr = err_avr_LOS, angle_err_sd = err_sd_LOS, angle_err_signed=err_signed_LOS)

 ; random test:
 ;By_rnd = randomu(seed1, 512, 512)
 ;Bz_rnd = randomu(seed2, 512, 512)
 err_rnd = features_vs_B(features, By_rnd, Bz_rnd, angle_err_avr = err_avr_rnd, angle_err_signed=err_signed_rnd)

 Lmin = 1 ; 30 ; can be adjusted

 w=where(features.L gt Lmin)

 mean_all = mean(err_avr, /NaN)*180/!Pi
 mean_all_LOS = mean(err_avr_LOS, /NaN)*180/!Pi
 mean_large = mean(err_avr[w], /NaN)*180/!Pi
 mean_large_LOS = mean(err_avr_LOS[w], /NaN)*180/!Pi

 hist_y = float(histogram(err[where(err gt 0)]*180/3.14, locations=hist_x, binsize=2.5, min=0, max=50))

 LL_1d = [0.0]
 err_1d = [0.0]
 err_1d_signed = [0.0]
 err_1d_LOS = [0.0]
 err_1d_signed_LOS = [0.0]
 err_1d_rnd   = [0.0]
 err_1d_signed_rnd   = [0.0]

 for i=0, n_elements(features)-1 do begin
   n_nodes = features[i].n_nodes
   LL_1d = [LL_1d, fltarr(n_nodes)+features[i].L]

   err_1d = [err_1d, reform(err[i,0:n_nodes-1])]
   err_1d_signed = [err_1d_signed, reform(err_signed[i,0:n_nodes-1])]

   err_1d_LOS = [err_1d_LOS, reform(err_LOS[i,0:n_nodes-1])]
   err_1d_signed_LOS = [err_1d_signed_LOS, reform(err_signed_LOS[i,0:n_nodes-1])]

   err_1d_rnd = [err_1d_rnd, reform(err_rnd[i,0:n_nodes-1])]
   err_1d_signed_rnd = [err_1d_signed_rnd, reform(err_signed_rnd[i,0:n_nodes-1])]
 endfor

LL_1d = LL_1d[1:*]
err_1d = err_1d[1:*]
err_1d_signed = err_1d_signed[1:*]
err_1d_LOS = err_1d_LOS[1:*]
err_1d_signed_LOS = err_1d_signed_LOS[1:*]
err_1d_rnd = err_1d_rnd[1:*]
err_1d_signed_rnd = err_1d_signed_rnd[1:*]

if keyword_set(isplot) then begin

 print, 'feature vs B angle, all L: ' + string(mean_all, format='(F6.2)') + ' degrees'
 print, 'feature vs B_LOS angle, all L: '  + string(mean_all_LOS, format='(F6.2)') + ' degrees'
 print, 'feature vs B angle, L > Lmin: ' + string(mean_large, format='(F6.2)') + ' degrees'
 print, 'feature vs B_LOS angle, L > Lmin: '  + string(mean_large_LOS, format='(F6.2)') + ' degrees'

 print, 'Random test: ', mean(err_avr_rnd[w], /NaN)*180/!Pi

 window, 3, xsize=500, ysize=500 & vel, By, Bz, nvecs=10000, title='B in central YZ plane'
 ;window, 4, xsize=500, ysize=500 & vel, By_LOS, Bz_LOS, nvecs=10000, title='B, LOS-integrated'

 setcolors
 !P.charsize=1.5

 window, 5  & plot, LL_1d, err_1d*180/!Pi, psym=6, title='Features vs B, central plane   L>Lmin mean: ' $
              + string(mean_large, format='(F6.2)') + ' degrees', xtitle='Feature length, pixels', $
              ytitle='Discrepancy angle, degrees', symsize=1.5
 			errplot, features.L, (err_avr-err_sd)*180/!Pi, (err_avr+err_sd)*180/!Pi, color=2, thick=2
 			oplot, !x.crange, [45,45], lines=2

; window, 6 & plot, LL_1d, err_1d_LOS*180/!Pi, psym=6, title='Features vs B, LOS-integrated    L>Lmin mean: ' $
;             + string(mean_large_LOS, format='(F6.2)') + ' degrees', xtitle='Feature length, pixels', $
;             ytitle='Discrepancy angle, degrees', symsize=1.5

; 			errplot, features.L, (err_avr_LOS-err_sd_LOS)*180/!Pi, (err_avr_LOS+err_sd_LOS)*180/!Pi, color=2, thick=2
; 			oplot, !x.crange, [45,45], lines=2

;window, 7 & plot, features.L, err_avr*180/!Pi, psym=6, xtitle='Feature length, pixels', $
;			 ytitle='Discrepancy angle, degrees'
;			oplot, features.L, err_avr_LOS*180/!Pi, psym=6, thick=3


   ;window, 8
   ;PLOT_FEATURES, IMG_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds), title=''
 endif

  return, {err:err_1d,  err_signed:err_1d_signed, err_LOS:err_1d_LOS, err_signed_LOS:err_1d_signed_LOS, err_rnd: err_1d_rnd, err_signed_rnd: err_1d_signed_rnd, $
  				L:LL_1d,  f_corona:f_corona,   f_By: f_By, f_Bz:f_Bz, f_By_LOS:f_By_LOS, f_Bz_LOS: f_Bz_LOS} ; hist_x=hist_x, hist_y=hist_y

End


PRO script3, input_directory,  output_directory,  err_arr_MLSO,  err_arr_LOS_MLSO, err_arr_FORWARD,  err_arr_LOS_FORWARD,  err_arr_rnd, L_MLSO, L_FORWARD, manual=manual
  ;dirs = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\slices_2\" + ['1\','2\','3\','4\','5\','6\']
  ;dirs = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\slices_2\" + ['4\']
  dirs = input_directory
  out_dir = output_directory

  err_arr_MLSO_ = [0.0]
  err_arr_LOS_MLSO_ = [0.0]
  err_arr_FORWARD_ = [0.0]
  err_arr_LOS_FORWARD_ = [0.0]
  err_arr_rnd_ = [0.0]
  L_MLSO_ = [0.0]
  L_FORWARD_ = [0.0]


  for i=0, n_elements(dirs)-1 do begin
     print, dirs[i]
     f_MLSO = file_search(dirs[i]+'*kcor*')
     f_pB = file_search(dirs[i]+'*KCor__PSI_pB.fits')
     f_ne = file_search(dirs[i]+'*KCor__PSI_ne.fits')
     f_ne_LOS = file_search(dirs[i]+'*KCor__PSI_ne_LOS.fits')

     f_By =  file_search(dirs[i]+'*KCor__PSI_By.fits')
     f_Bz =  file_search(dirs[i]+'*KCor__PSI_Bz.fits')
     f_By_LOS =  file_search(dirs[i]+'*KCor__PSI_By_LOS.fits')
     f_Bz_LOS =  file_search(dirs[i]+'*KCor__PSI_Bz_LOS.fits')

     for i=0, n_elements(f_pB)-1 do begin
     f_err_sav = out_dir + repstr(file_basename(f_pb[i]), '_pB.fits', '.sav'); + '/'  + date_str+ '_' + detector_str +  '_errors.sav'

      res_MLSO = compare_angles( f_MLSO[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='MLSO2016', thresh_k = 1.5)  ;hist_x=hist_x, hist_y=hist_y
      res_FORWARD = compare_angles( f_pB[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='PSI_MLSO', thresh_k = 1.1647, manual=manual)  ;hist_x=hist_x, hist_y=hist_y



      ;err_arr_MLSO = [err_arr_MLSO, res_MLSO.err]
      ;err_arr_LOS_MLSO = [err_arr_LOS_MLSO, res_MLSO.err_LOS]
      ;err_arr_FORWARD = [err_arr_FORWARD, res_FORWARD.err]
      ;err_arr_LOS_FORWARD = [err_arr_LOS_FORWARD, res_FORWARD.err_LOS]
      ;err_arr_rnd = [err_arr_rnd,  res_MLSO.err_rnd]
      err_arr_MLSO = res_MLSO.err
      err_signed_arr_MLSO = res_MLSO.err_signed

      err_arr_LOS_MLSO = res_MLSO.err_LOS
      err_signed_arr_LOS_MLSO = res_MLSO.err_signed_LOS

      err_arr_FORWARD = res_FORWARD.err
      err_signed_arr_FORWARD = res_FORWARD.err_signed

      err_arr_LOS_FORWARD = res_FORWARD.err_LOS
      err_signed_arr_LOS_FORWARD = res_FORWARD.err_signed_LOS

      err_arr_rnd =res_MLSO.err_rnd
      err_signed_arr_rnd =res_MLSO.err_signed_rnd

      L_MLSO = res_MLSO.L
      L_FORWARD = res_FORWARD.L

	  save, err_arr_MLSO,  err_signed_arr_MLSO, err_arr_LOS_MLSO, err_signed_arr_LOS_MLSO, err_arr_FORWARD,  err_signed_arr_FORWARD, $
	    err_arr_LOS_FORWARD,  err_signed_arr_LOS_FORWARD,  err_arr_rnd,  err_signed_arr_rnd,  L_MLSO, L_FORWARD, filename = f_err_sav

	  err_arr_MLSO_ = [err_arr_MLSO_, err_arr_MLSO]
	  err_arr_LOS_MLSO_ = [err_arr_LOS_MLSO_,err_arr_LOS_MLSO]
	  err_arr_FORWARD_ = [err_arr_FORWARD_, err_arr_FORWARD]
	  err_arr_LOS_FORWARD_ = [err_arr_LOS_FORWARD_, err_arr_LOS_FORWARD]
	  err_arr_rnd_ = [err_arr_rnd_,  err_arr_rnd]
	  L_MLSO_ = [L_MLSO_, L_MLSO]
	  L_FORWARD_ = [L_FORWARD_, L_FORWARD]

	  endfor


  endfor

  err_arr_MLSO = err_arr_MLSO[1:*]
  err_arr_LOS_MLSO = err_arr_LOS_MLSO[1:*]
  err_arr_FORWARD =err_arr_FORWARD[1:*]
  err_arr_LOS_FORWARD =err_arr_LOS_FORWARD[1:*]
  err_arr_rnd = err_arr_rnd[1:*]
  L_MLSO = L_MLSO[1:*]
  L_FORWARD = L_FORWARD[1:*]


End


PRO script3_optimize, input_directory,  output_directory, thresh_k_optimize,  err_arr_MLSO,  err_arr_LOS_MLSO, err_arr_FORWARD,  err_arr_LOS_FORWARD,  err_arr_rnd, L_MLSO, L_FORWARD, manual=manual
  ;  Suppress error messages:
  !quiet = 1 & !except = 0
  ;dirs = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\slices_2\" + ['1\','2\','3\','4\','5\','6\']
  ;dirs = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\MLSO_PSI_FORWARD_coaligned\slices_2\" + ['4\']
  dirs = input_directory
  out_dir = output_directory

  err_arr_MLSO_ = [0.0]
  err_arr_LOS_MLSO_ = [0.0]
  err_arr_FORWARD_ = [0.0]
  err_arr_LOS_FORWARD_ = [0.0]
  err_arr_rnd_ = [0.0]
  L_MLSO_ = [0.0]
  L_FORWARD_ = [0.0]

  print, 'thresh_k optimize: ', thresh_k_optimize


  for i=0, n_elements(dirs)-1 do begin
    print, dirs[i]
    f_MLSO = file_search(dirs[i]+'*kcor*')
    f_pB = file_search(dirs[i]+'*KCor__PSI_pB.fits')
    f_ne = file_search(dirs[i]+'*KCor__PSI_ne.fits')
    f_ne_LOS = file_search(dirs[i]+'*KCor__PSI_ne_LOS.fits')

    f_By =  file_search(dirs[i]+'*KCor__PSI_By.fits')
    f_Bz =  file_search(dirs[i]+'*KCor__PSI_Bz.fits')
    f_By_LOS =  file_search(dirs[i]+'*KCor__PSI_By_LOS.fits')
    f_Bz_LOS =  file_search(dirs[i]+'*KCor__PSI_Bz_LOS.fits')

    for i=0, n_elements(f_pB)-1 do begin
      f_err_sav = out_dir + repstr(file_basename(f_pb[i]), '_pB.fits', '.sav'); + '/'  + date_str+ '_' + detector_str +  '_errors.sav'

      res_MLSO = compare_angles( f_MLSO[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='MLSO2016', thresh_k = thresh_k_optimize)  ;hist_x=hist_x, hist_y=hist_y
      res_FORWARD = compare_angles( f_pB[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='PSI_MLSO', thresh_k = 1.1647)  ;hist_x=hist_x, hist_y=hist_y



      ;err_arr_MLSO = [err_arr_MLSO, res_MLSO.err]
      ;err_arr_LOS_MLSO = [err_arr_LOS_MLSO, res_MLSO.err_LOS]
      ;err_arr_FORWARD = [err_arr_FORWARD, res_FORWARD.err]
      ;err_arr_LOS_FORWARD = [err_arr_LOS_FORWARD, res_FORWARD.err_LOS]
      ;err_arr_rnd = [err_arr_rnd,  res_MLSO.err_rnd]
      err_arr_MLSO = res_MLSO.err
      err_signed_arr_MLSO = res_MLSO.err_signed

      err_arr_LOS_MLSO = res_MLSO.err_LOS
      err_signed_arr_LOS_MLSO = res_MLSO.err_signed_LOS

      err_arr_FORWARD = res_FORWARD.err
      err_signed_arr_FORWARD = res_FORWARD.err_signed

      err_arr_LOS_FORWARD = res_FORWARD.err_LOS
      err_signed_arr_LOS_FORWARD = res_FORWARD.err_signed_LOS

      err_arr_rnd =res_MLSO.err_rnd
      err_signed_arr_rnd =res_MLSO.err_signed_rnd

      L_MLSO = res_MLSO.L
      L_FORWARD = res_FORWARD.L

      save, err_arr_MLSO,  err_signed_arr_MLSO, err_arr_LOS_MLSO, err_signed_arr_LOS_MLSO, err_arr_FORWARD,  err_signed_arr_FORWARD, $
        err_arr_LOS_FORWARD,  err_signed_arr_LOS_FORWARD,  err_arr_rnd,  err_signed_arr_rnd,  L_MLSO, L_FORWARD, filename = f_err_sav

      err_arr_MLSO_ = [err_arr_MLSO_, err_arr_MLSO]
      err_arr_LOS_MLSO_ = [err_arr_LOS_MLSO_,err_arr_LOS_MLSO]
      err_arr_FORWARD_ = [err_arr_FORWARD_, err_arr_FORWARD]
      err_arr_LOS_FORWARD_ = [err_arr_LOS_FORWARD_, err_arr_LOS_FORWARD]
      err_arr_rnd_ = [err_arr_rnd_,  err_arr_rnd]
      L_MLSO_ = [L_MLSO_, L_MLSO]
      L_FORWARD_ = [L_FORWARD_, L_FORWARD]

    endfor


  endfor

  err_arr_MLSO = err_arr_MLSO[1:*]
  err_arr_LOS_MLSO = err_arr_LOS_MLSO[1:*]
  err_arr_FORWARD =err_arr_FORWARD[1:*]
  err_arr_LOS_FORWARD =err_arr_LOS_FORWARD[1:*]
  err_arr_rnd = err_arr_rnd[1:*]
  L_MLSO = L_MLSO[1:*]
  L_FORWARD = L_FORWARD[1:*]


End
;----------------------------------------
PRO script4, input_directory, output_directory,  err_arr_COR1,  err_arr_LOS_COR1, err_arr_FORWARD,  err_arr_LOS_FORWARD,  err_arr_rnd, L_COR1, L_FORWARD, manual=manual

 ;dirs = "c:\Users\vadim\Documents\SCIENCE PROJECTS\N Arge\PSI\COR1_PSI_FORWARD_coaligned\slices_1\" + ['1\','2\','3\','4\','5\','6\']
  dirs = input_directory;"/Users/crura/Desktop/Research/github/Test_Suite/Image-Coalignment/QRaFT/COR1_PSI_FORWARD_slices_1/"
  out_dir = output_directory

  err_arr_COR1_ = [0.0]
  err_arr_LOS_COR1_ = [0.0]
  err_arr_FORWARD_ = [0.0]
  err_arr_LOS_FORWARD_ = [0.0]
  err_arr_rnd_ = [0.0]
  L_COR1_ = [0.0]
  L_FORWARD_ = [0.0]

  for i=0, n_elements(dirs)-1 do begin
     print, dirs[i]
     f_COR1 = file_search(dirs[i]+'*rep_med*')
     f_pB = file_search(dirs[i]+'*COR1__PSI_pB.fits')
     f_ne = file_search(dirs[i]+'*COR1__PSI_ne.fits')
     f_ne_LOS = file_search(dirs[i]+'*COR1__PSI_ne_LOS.fits')

     f_By =  file_search(dirs[i]+'*COR1__PSI_By.fits')
     f_Bz =  file_search(dirs[i]+'*COR1__PSI_Bz.fits')
     f_By_LOS =  file_search(dirs[i]+'*COR1__PSI_By_LOS.fits')
     f_Bz_LOS =  file_search(dirs[i]+'*COR1__PSI_Bz_LOS.fits')

	 for i=0, n_elements(f_pB)-1 do begin
	 date_str =  strmid(file_basename(f_pB[i]),2,10)
	 detector_str = strmid(file_basename(f_pB[i]),14,19)
     f_err_sav = out_dir + repstr(file_basename(f_pb[i]), '_pB.fits', '.sav'); + '/'  + date_str+ '_' + detector_str +  '_errors.sav'

      res_COR1 = compare_angles( f_COR1[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='COR1', thresh_k = 1.5, manual=manual)  ;hist_x=hist_x, hist_y=hist_y
      res_FORWARD = compare_angles( f_pB[i],  f_By[i], f_Bz[i], f_By_LOS[i], f_Bz_LOS[i], data_source='PSI', thresh_k = 1.1647, manual=manual)  ;hist_x=hist_x, hist_y=hist_y

      err_arr_COR1 = res_COR1.err
      err_signed_arr_COR1 = res_COR1.err_signed

      err_arr_LOS_COR1 = res_COR1.err_LOS
      err_signed_arr_LOS_COR1 = res_COR1.err_signed_LOS

      err_arr_FORWARD = res_FORWARD.err
      err_signed_arr_FORWARD = res_FORWARD.err_signed

      err_arr_LOS_FORWARD = res_FORWARD.err_LOS
      err_signed_arr_LOS_FORWARD = res_FORWARD.err_signed_LOS

      err_arr_rnd =res_COR1.err_rnd
      err_signed_arr_rnd =res_COR1.err_signed_rnd

	  L_COR1 = res_COR1.L
	  L_FORWARD = res_FORWARD.L

      save, err_arr_COR1,  err_signed_arr_COR1, err_arr_LOS_COR1, err_signed_arr_LOS_COR1, err_arr_FORWARD,  err_signed_arr_FORWARD, $
      			err_arr_LOS_FORWARD,  err_signed_arr_LOS_FORWARD,  err_arr_rnd,  err_signed_arr_rnd,  L_COR1, L_FORWARD, filename = f_err_sav

      err_arr_COR1_ = [err_arr_COR1_, err_arr_COR1]
      err_arr_LOS_COR1_ = [err_arr_LOS_COR1_,err_arr_LOS_COR1]
      err_arr_FORWARD_ = [err_arr_FORWARD_, err_arr_FORWARD]
      err_arr_LOS_FORWARD_ = [err_arr_LOS_FORWARD_, err_arr_LOS_FORWARD]
      err_arr_rnd_ = [err_arr_rnd_,  err_arr_rnd]
	  L_COR1_ = [L_COR1_, L_COR1]
	  L_FORWARD_ = [L_FORWARD_, L_FORWARD]
	  endfor

  endfor

  err_arr_COR1 = err_arr_COR1_[1:*]
  err_arr_LOS_COR1 = err_arr_LOS_COR1_[1:*]
  err_arr_FORWARD =err_arr_FORWARD_[1:*]
  err_arr_LOS_FORWARD =err_arr_LOS_FORWARD_[1:*]
  err_arr_rnd = err_arr_rnd_[1:*]
  L_COR1 = L_COR1_[1:*]
  L_FORWARD = L_FORWARD_[1:*]


End

;=========================================================================

PRO plot_err_hist,  err_arr_MLSO,  err_arr_LOS_MLSO, err_arr_FORWARD,  err_arr_LOS_FORWARD,  err_arr_rnd, L_MLSO, L_FORWARD
   Lmin =40

  	hist_y_MLSO = float(histogram(err_arr_MLSO[where(L_MLSO gt Lmin)]*180/3.14, locations=hist_x_MLSO, binsize=2.5, min=0, max=50))
	hist_y_FORWARD = float(histogram(err_arr_FORWARD[where(L_FORWARD gt Lmin)]*180/3.14, locations=hist_x_FORWARD, binsize=2.5, min=0, max=50))

  	hist_y_LOS_MLSO = float(histogram(err_arr_LOS_MLSO[where(L_MLSO gt Lmin)]*180/3.14, locations=hist_x_LOS_MLSO, binsize=2.5, min=0, max=50))
	hist_y_LOS_FORWARD = float(histogram(err_arr_LOS_FORWARD[where(L_FORWARD gt Lmin)]*180/3.14, locations=hist_x_LOS_FORWARD, binsize=2.5, min=0, max=50))

	hist_y_rnd = float(histogram(err_arr_rnd[where(err_arr_rnd gt 0)]*180/3.14, locations=hist_x_rnd, binsize=2.5, min=0, max=50))

	window, 0, xsize= 600, ysize=1000, title='CENTRAL PLANE'  & erase

	plot, hist_x_FORWARD, hist_y_FORWARD /total(hist_y_FORWARD) ,  xtitle='Misalignment angle [degrees]', ytitle='Normalized occurrence frequency', /nodata
	oplot, hist_x_FORWARD, hist_y_FORWARD /total(hist_y_FORWARD) , psym=10, thick=2, color=2
 	oplot, hist_x_MLSO, hist_y_MLSO/total(hist_y_MLSO)  , psym=10, color=4, thick=2
	oplot, hist_x_rnd, hist_y_rnd /total(hist_y_rnd) , psym=10, lines=2,  thick=2

	xyouts, 0.33,  0.9,  'FORWARD vs PSI MAS: ' +string(median(err_arr_FORWARD[where(L_FORWARD gt Lmin)]*180/3.14), format='(F4.1)')+'!Uo!N',  /normal, color=2
	xyouts, 0.33, 0.87,  'Solar image vs PSI MAS: ' +string(median(err_arr_MLSO[where((L_MLSO gt Lmin) and finite(err_arr_MLSO))]*180/3.14), format='(F4.1)')+'!Uo!N',  /normal, color=4
	xyouts, 0.33, 0.84,  'Random test: ' +string( median(   err_arr_rnd[where(err_arr_rnd ne 0)   ] *180.0/3.14 ), format='(F4.1)')+'!Uo!N',  /normal


	window, 1, xsize= 600, ysize=1000, title='LOS - integrated' & erase
	plot, hist_x_LOS_FORWARD, hist_y_LOS_FORWARD /total(hist_y_LOS_FORWARD) , psym=10, thick=2, xtitle='Misalignment angle [degrees]', ytitle='Normalized occurrence frequency'
 	oplot, hist_x_LOS_MLSO, hist_y_LOS_MLSO/total(hist_y_LOS_MLSO)  , psym=10
	oplot, hist_x_rnd, hist_y_rnd /total(hist_y_rnd) , psym=10, lines=2

	xyouts, 20,  0.2,  'mean FORW=' +string(mean(err_arr_LOS_FORWARD[where(L_FORWARD gt Lmin)]*180/3.14), format='(F5.1)'),  /data
	xyouts, 20, 0.18,  'mean MLSO=' +string(mean(err_arr_LOS_MLSO[where(L_MLSO gt Lmin)]*180/3.14), format='(F5.1)'),  /data

End


;  plot,hist_x_pB, hist_y_pB/total(hist_y_pB), xtitle='Discrepancy [degrees]', ytitle='Occurrence rate', psym=10, color=0
;  oplot, hist_x_pB, hist_y_pB/total(hist_y_pB),  psym=10, color=2, thick=2
;  oplot, hist_x_MLSO, hist_y_MLSO/total(hist_y_MLSO),  psym=10, color=4, thick=3, lines=2
