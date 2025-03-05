
function blob_stat_to_features, blob_stat, d_phi, d_rho, rho_min, XYCenter, IMG

  n_blobs = n_elements(blob_stat.area) & n_nodes_max = n_elements(blob_stat.phi[0,*])
  n_nodes = intarr(n_blobs)
  xx_r = dblarr(n_blobs, n_nodes_max) & xx_r[*]=0 
  yy_r=xx_r & angles_xx_r = xx_r & angles_yy_r = yy_r & angles = xx_r 
  L = dblarr(n_blobs)
  
  for i=0, n_blobs-1 do begin
    N = blob_stat.length[i]
    n_nodes[i]=N
    phi= d_phi*blob_stat.phi_fit[i,0:N-1]
    rho= d_rho*blob_stat.rho[i,0:N-1] + rho_min
    xx_r[i,0:N-1] = rho*cos(phi) + XYCenter[0]
    yy_r[i,0:N-1] = rho*sin(phi) + XYCenter[0]

    L[i] = ( (xx_r[i,N-1]-xx_r[i,0])^2 + (yy_r[i,N-1]-yy_r[i,0])^2 )^0.5
            
    angles_xx_r[i,*] = (xx_r[i,*] + shift(xx_r[i,*], -1))/2.0 
    angles_yy_r[i,*] = (yy_r[i,*] + shift(yy_r[i,*], -1))/2.0    
    
    angles[i,*] = atan2(shift(yy_r[i,*],-1) - yy_r[i,*], shift(xx_r[i,*],-1) - xx_r[i,*] )
    ;angles[k] = atan2(yy_r[k+1]-yy_r[k], xx_r[k+1]-xx_r[k])
    
    angles_xx_r[i, N-1:-1] = 0 
    angles_yy_r[i, N-1:-1] = 0
    angles[i, N-1:-1] = 0
        
  endfor

  features= make_array(n_blobs, value = {xx_r: dblarr(n_nodes_max), yy_r: dblarr(n_nodes_max), $
                       angles_xx_r: dblarr(n_nodes_max), angles_yy_r: dblarr(n_nodes_max), angles_p:dblarr(n_nodes_max), $
                       intensity:0.0, n_nodes:0, L:0.0} )
  for i=0, n_blobs-1 do begin
    features[i].n_nodes = n_nodes[i]
    features[i].xx_r = xx_r[i,*] ;+ XYCenter[0]
    features[i].yy_r = yy_r[i,*] ;+ XYCenter[1]        
    features[i].L = L[i]
    
    features[i].intensity = median( IMG[features[i].xx_r[0:n_nodes[i]-1], features[i].yy_r[0:n_nodes[i]-1]] )
    
    features[i].angles_xx_r = angles_xx_r[i,*]
    features[i].angles_yy_r = angles_yy_r[i,*]
    features[i].angles_p = angles[i,*]
    
  endfor

  return, features 
End