function ADAPT_THRESH_PROB, IMG, p_arr = p_arr, sign=sign



  if n_elements(p_arr) eq 0 then p_arr = [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99]
  if n_elements(sign) eq 0 then sign = 'p'
  
  Nbins = 1E7  
  
  w_p = where(IMG gt 0)
  w_n = where(IMG lt 0)

  if w_p[0] ne -1 then begin
    h_p = histogram(IMG[w_p], nbins=nbins, loc = v_p)/float(n_elements(w_p)) 
    hc_p = total(h_p, /cumul)
    thresh_p_arr = v_p[value_locate(hc_p, p_arr)]
  endif else thresh_p_arr = fltarr(n_elements(p_arr))
   
  if w_n[0] ne -1 then begin
    h_n = histogram(abs(IMG[w_n]), nbins=nbins, loc = v_n)/float(n_elements(w_n))
    hc_n = total(h_n, /cumul)
    thresh_n_arr = -v_n[value_locate(hc_n, p_arr)]
  endif else thresh_n_arr = fltarr(n_elements(p_arr))

  
  ; forming 2D threshold array for compliance with process_corona.pro  
  ;thresh_arr = fltarr(2, 2*n_elements(p_arr))
  
  ;thresh_arr[1, 0:n_elements(p_arr)-1] = reverse(thresh_n_arr)
  ;thresh_arr[1, n_elements(p_arr):*] = thresh_p_arr
  ;thresh_arr[0, *] = min(img)
  
  if sign eq 'p' then thresh_arr=thresh_p_arr 
  if sign eq 'n' then thresh_arr=thresh_n_arr
  if sign eq 'np' then thresh_arr=[thresh_n_arr, thresh_p_arr]
  return, thresh_arr

End
