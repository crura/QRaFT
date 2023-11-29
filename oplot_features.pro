
pro oplot_features, features, XYCenter
  setcolors
  for i=0, n_elements(features)-1 do begin
    L=features[i].n_nodes
    xx = features[i].xx_r[0:L-1] - XYCenter[0]
    yy = features[i].yy_r[0:L-1] - XYCenter[1]
    plots, xx, yy, color=2, thick=2
  endfor
  
End