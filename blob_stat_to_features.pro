
function blob_stat_to_features, blob_stat, d_phi, d_rho, rho_min, XYCenter

  n_blobs = n_elements(blob_stat.area) & n_nodes_max = n_elements(blob_stat.phi[0,*])
  xx_r = fltarr(n_blobs, n_nodes_max) & xx_r[*]=0 & yy_r=xx_r & n_nodes = intarr(n_blobs)
  for i=0, n_blobs-1 do begin
    L = blob_stat.length[i]
    phi= d_phi*blob_stat.phi_fit[i,0:L-1]
    rho= d_rho*blob_stat.rho[i,0:L-1] + rho_min
    xx_r[i,0:L-1] = rho*cos(phi)
    yy_r[i,0:L-1] = rho*sin(phi)
    n_nodes[i]=L
  endfor

  features= make_array(n_blobs, value = {xx_r: fltarr(n_nodes_max), yy_r: fltarr(n_nodes_max), $
                       angles_xx_r: fltarr(n_nodes_max), angles_yy_r: fltarr(n_nodes_max), n_nodes:0} )
  for i=0, n_blobs-1 do begin
    features[i].n_nodes = n_nodes[i]
    features[i].xx_r = xx_r[i,*] + XYCenter[0]
    features[i].yy_r = yy_r[i,*] + XYCenter[1]
    features[i].angles_xx_r = xx_r[i,*] + XYCenter[0]
    features[i].angles_yy_r = yy_r[i,*] + XYCenter[1]
  endfor

  return, features 
End