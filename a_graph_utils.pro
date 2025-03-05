

;====================================================================================================================
PRO flipcolors

  ; flips display colors to plot black lines on white background

  bk = !P.BACKGROUND & !P.background=!P.color & !P.color = bk

  ;additional settings

  device, decom=0
  ;!X.margin=[15,5]
  ;!Y.margin=[4,2]
  ;!P.charsize=2
  ;!X.ticklen=0.04
  ;!Y.ticklen=0.01

End
;====================================================================================================================


PRO setgraph
  loadct,0, /silent
  setcolors
  !P.background = 1
  !P.color = 0
  ; device,decompose=0
  !P.charsize=1.2
  !P.thick=1;.5
  clean
End

;====================================================================================================================
PRO OpenPS, fname, x_size, y_size
  ; example :
  ; openps, fname, 20.,30.
  setkeyword, fname, 'c:\q.eps'
  setkeyword, x_size, 20.0
  setkeyword, y_size, 30.0
  SET_PLOT, 'PS'
  DEVICE, /ENCAPSULATED, FILENAME = fname, /color, bits_per_pixel=8, xsize=x_size,  ysize=y_size, /TIMES
  !P.font=2 ; necessary for using postscript font "TIMES"; otherwise set to default !P.font = -1
End
;====================================================================================================================
PRO ClosePS
  Device, /close
  SET_PLOT, 'WIN'
  !P.font=-1 ; restoring the default WIN font
End
;====================================================================================================================
function value_locate_, array, values
; improved value_locate which doesn't require that the input aarray be monotonically increasing

  w = sort(array)
  ind = value_locate(array[w], values)
  return, w[ind]
End

;====================================================================================================================
function Str, a, format
  return, strtrim(string(a, format=format), 2)
End

;====================================================================================================================
function GetPos, n_rows, n_cols, region=region, xyspace=xyspace

  pos = fltarr(n_rows,n_cols,4)
  if not keyword_set(region) then region = [0.05, 0.1, 0.98, 0.9] ;[0.1,0.1, 0.99, 0.90]
  if not keyword_set(xyspace) then xyspace = [0.08, 0.06] ; [0.04,0.06]

  xsize = (region[2]-region[0])/(1.0*n_cols)
  ysize = (region[3]-region[1])/(1.0*n_rows)
  for x=0,n_cols-1 do $
    for y=0,n_rows-1 do begin

      x1 = region[0] + x*xsize
      x2 = region[0] + (x+1)*xsize
      y1 = region[3] - (y+1)*ysize
      y2 = region[3] -  y*ysize
      pos[y,x,*] = [x1,y1,x2,y2] + [xyspace,0,0]

    endfor
  return, pos
End
;====================================================================================================================
function GetPositives, A

  return, A[where(A gt 0)]
End

;====================================================================================================================

PRO GetPositives_2arr, A1,A2, A1_, A2_
  setkeyword, A2, A1
  w = where((A1 gt 0) and (A2 gt 0))

  A1_ = A1[w]
  A2_ = A2[w]
End

;====================================================================================================================
PRO SetKeyword, keyword, value
  ;if not keyword_set(keyword) then keyword=value
  if n_elements(keyword) eq 0 then keyword=value
End
;====================================================================================================================
PRO SetColors

    TVLCT, [0,255,255, 0,0,150, 255, 160], [0,255,0,255,0,150, 230, 100], [0,255,0,0,255,150, 0, 0]
    ;- for Postscript output Color=
    ; 0 - black, 1 - white, 2 - red, 3 - green, 4 - blue, 5 - grey, 6 - yellow, 7 - brown
    ;
End
;====================================================================================================================
PRO SetBW

  ; ENSURES DEVICE-INDEPENDENT PLOTTING WITH BLACK AXES ON WHITE BACKGROUND
  ; swaps BLACK and WHITE color indices depending on the device
  if !D.NAME eq 'PS' then clr_indices = [0, 255] else clr_indices = [255, 0]

  ;device, decomposed=0

  tvlct, 0,   0,    0,    clr_indices[0]  ; black color, indexed by 0 (255) in PS (WIN) device
  tvlct, 255, 255,  255,  clr_indices[1]  ; white color, indexed by 255 (0) in PS (WIN) device

  ; Based on the example from  http://www.idlcoyote.com/tips/ps_colorswitch.html :
  ; if !D.NAME eq 'PS' then tvlct, 255, 255, 255, 255 else tvlct, 255, 255, 255, 0
  ; IF !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0 ELSE TVLCT, 0, 0, 0, 255

End

;====================================================================================================================

PRO Clr_OPLOT, X, Y, C, ctable=ctable, thick=thick
  ; example:
  ;  X = findgen(100) & Y = findgen(100)  & C = findgen(100)
  ; setcolors & plot, X,Y, /nodata & clr_oplot, X,Y, (256./100.)*C, thick=2, ctable=13

  if keyword_set(ctable) then loadct, ctable, /silent
  for i=0, n_elements(X)-2 do plots, X[i:i+1], Y[i:i+1], color=C[i:i+1], thick=thick, noclip=0

End
;====================================================================================================================
function minmax, x
  return, [min(x) , max(x)]
End
;====================================================================================================================
function atan2, yy, xx ; returns angles in the  0... 2Pi range
 N = n_elements(xx)
 angles = fltarr(N)
 for i=0, N-1 do $
   angles[i] =  atan(yy[i],xx[i])+(yy[i] lt 0. ? 2*!PI : 0.)
 if N eq 1 then angles=angles[0]
 return, angles
end
;====================================================================================================================

PRO Clean
  ; you can also use  ERASE
  A = fltarr(3000,3000)
  A[0,0]=-1
  tvscl,A

End
;====================================================================================================================
PRO PlotFrame, ylog=ylog
  ; plots a frame around the current plotting area using the current color

  xx = [ !x.crange[0], !x.crange[0], !x.crange[1], !x.crange[1], !x.crange[0] ]
  yy = [ !y.crange[0], !y.crange[1], !y.crange[1], !y.crange[0], !y.crange[0] ]
  if keyword_set(ylog) then yy = 10.0^yy
  plots, xx,yy
End
;====================================================================================================================

Function GetCorners
  ;print, 'Click at the bottom left corner: '
  CURSOR,X1,Y1,/DOWN,/DEVICE
  ;print, 'Click at the top right corner'
  CURSOR,X2,Y2,/DOWN,/DEVICE
  a=[x1,y1,x2,y2]
  ;print,a
  return, a
End
;====================================================================================================================
Function GetCorners1,position,Nx,Ny
  ;print, 'Click at the bottom left corner: '

  x1=position[0] &  y1=position[1] &  x2=position[2] & y2=position[3]
  ax = 1/(x2-x1) & bx = -x1/(x2-x1)
  ay = 1/(y2-y1) & by = -y1/(y2-y1)

  CURSOR,X1,Y1,/DOWN,/NORMAL
  ;print, 'Click at the top right corner'
  CURSOR,X2,Y2,/DOWN,/NORMAL

  x1 = ax*x1 + bx
  x2 = ax*x2 + bx
  y1 = ay*y1 + by
  y2 = ay*y2 + by

  a=[Nx*x1,Ny*y1,Nx*x2,Ny*y2]
  ;print,a
  return, a
End
;====================================================================================================================
PRO DefUserSym, fill=fill
   ; fill = 0  - empty
   ; fill = 1  - solid
  ;The following code illustrates the use of USERSYM to define a new symbol—a filled circle:

  ; Make a vector of 16 points, A[i] = 2pi/16:
  A = FINDGEN(17) * (!PI*2/16.)
  ; Define the symbol to be a unit circle with 16 points,
  ; and set the filled flag:
  USERSYM, COS(A), SIN(A), fill=fill;/FILL

end

;====================================================================================================================
PRO Click, Q
;+
;SAMPLE_CLICK,Q uses the mouse cursor to show values of an array Q that
;has been displayed in a window. Left click in the image to print the
;coordinates and the value. Right-click to end the procedure. It is assumed
;that the window is sized to fit the array image.
;-
!mouse.button=1
WHILE(!MOUSE.BUTTON NE 4) DO BEGIN
  CURSOR,X,Y,/DEVICE,/DOWN
  IF !MOUSE.BUTTON EQ 1 THEN $
  PRINT,FORMAT='("POSITION=[",I3,",",I3,"], VALUE=",E12.3)',X,Y,Q[X,Y]
ENDWHILE
END
;====================================================================================================================
PRO Click0, Q

  !mouse.button=1
  WHILE(!MOUSE.BUTTON NE 4) DO BEGIN
    CURSOR,X,Y,/Data,/DOWN
    IF !MOUSE.BUTTON EQ 1 THEN $
      ;PRINT,'X = ', string(X, format = '(D30.20)'), '  Y = ', string(Y, format = '(D30.20)','  Value = ', string(Q[X,Y], format = '(D30.20)'))
      PRINT,FORMAT='("POSITION=[",I3,",",I3,"], VALUE=",F8.4)',X,Y,Q[X,Y]
  ENDWHILE
End
;====================================================================================================================
function Click1, N_clicks

  !mouse.button=1

  Xarr = dblarr(N_clicks)
  count=0
  while (count lt N_clicks) and (!MOUSE.BUTTON NE 4) do begin
    cursor,X,Y,/data,/down
    ;oplot, [x,x], [-1000,1000], lines=3, thick=0.5

    if !mouse.button eq 1 then begin
      Xarr[count] = X
     ; oplot, [x,x], [-1000,1000], lines=2
      count=count+1
    endif

  endwhile

  if (!MOUSE.BUTTON EQ 4) then Xarr = [-1]

  return, Xarr

End
;====================================================================================================================
function Click1_, N_clicks

  !mouse.button=1

  Xarr = dblarr(N_clicks)
  count=0
  while (count lt N_clicks) and (!MOUSE.BUTTON NE 4) do begin
    cursor,X,Y, /device,/down

    xy_data = convert_coord([X,Y],/device, /to_data)
    x = xy_data[0] & y = xy_data[1]
    ;oplot, [x,x], [-1000,1000], lines=3, thick=0.5


    if !mouse.button eq 1 then begin
      Xarr[count] = X
      ; oplot, [x,x], [-1000,1000], lines=2
      count=count+1
    endif

  endwhile

  if (!MOUSE.BUTTON EQ 4) then Xarr = [-1]

  return, Xarr

End
;====================================================================================================================
PRO Click2, N_clicks, Xarr, Yarr

  !mouse.button=1

  Xarr = dblarr(N_clicks)
  Yarr = dblarr(N_clicks)
  count=0
  while (count lt N_clicks) and (!MOUSE.BUTTON NE 4) do begin
    cursor,X,Y,/data,/down
    ;oplot, [x,x], [-1000,1000], lines=3, thick=0.5

    ;if !mouse.button eq 1 then begin
      Xarr[count] = X
      Yarr[count] = Y

      count=count+1
    ;endif

  endwhile

  if (!MOUSE.BUTTON EQ 4) then begin & Xarr = [-1] & Yarr = [-1] & endif

End
;====================================================================================================================
Function DrawLine, A, x1,y1,x2,y2
  mn = min(A)
  sz = size(A)
  A1=A
  slope = (y2-y1)/(1.0*x2-x1)
  shift = y1 - slope*x1
  for x=x1,x2 do begin
    y = slope*x + shift
    if (x lt sz[1]) and (y lt sz[2]) then $
      A1[x,y] = mn
  endfor
  return, A1
End
;====================================================================================================================
Function DrawRect, A, x1,y1,x2,y2, min=min, max=max

  if keyword_set(min) then v = min(A)
  if keyword_set(max) then v = max(A)

  setkeyword, v, mean(A) ; otherwise

  A1=A
  A1[x1:x2,y1]=v
  A1[x1:x2,y2]=v
  A1[x1,y1:y2]=v
  A1[x2,y1:y2]=v
  return,A1
End
;====================================================================================================================
PRO DrawRect1, x1,y1,x2,y2, color=color
  ; in data coordinate system
  X = [x1,x1,x2,x2,x1]
  Y = [y1,y2,y2,y1,y1]
  PLOTS, X,Y,color=color

End
;====================================================================================================================
;IDL EXAMPLE: labeling plots using the cursor to position the text:
; Text is the string to be written on the screen:
PRO LABEL, TEXT
   ; Ask the user to mark the position:
   PRINT, 'Use the mouse to mark the text position:'
   ; Get the cursor position after pressing any button:
   CURSOR, X, Y, /NORMAL, /DOWN
   ; Write the text at the specified position.
   ; The NOCLIP keyword is used to ensure that
   ; the text will appear even if it is outside
   ; the plotting region.
   XYOUTS, X, Y, TEXT, /NORMAL, /NOCLIP
END
;====================================================================================================================
Function TimeTicks, ut1, ut2, ticks_per_hour, key, ticks_positions
  ; key = 0 => hrmn;  key = 1 => hr:mn;  key = 2 => hrmnsc;  key = 3 => hr:mn:sc

  n_ticks = (ticks_per_hour*1.0) ; to ensure correct rounding
  xticnum=fix((ut2-ut1)*n_ticks + 0.1) ;+.1)
  xtickstr=strarr(xticnum+1)
  ticks_positions=fltarr(xticnum+1)
  for i=0, xticnum do begin
    hr=fix(ut1+i/n_ticks)
    mn=round((ut1+i/n_ticks-hr*1.)*60.)
      sc=round((ut1+i/n_ticks-hr)*3600. - mn*60.)
    if sc lt 0 then begin & mn = mn-1 & sc=abs(sc) & endif
   ;print, hr,mn,sc
    hrmn=hr*100+mn
      hrmnsc=long64(hr*10000.+mn*100+sc)
    ticks_positions[i] = hr+ (mn/60.0) + (sc/3600.0)
    case key of
      0: xtickstr[i]=string(format='(I4.4)',hrmn)
      1: xtickstr[i]=string(format='(I2.2)',hr)+':'+string(format='(I2.2)',mn)
      2: xtickstr[i]=string(format='(I6.6)',hrmnsc)
      3: xtickstr[i]=string(format='(I2.2)',hr)+':'+string(format='(I2.2)',mn)+':'+string(format='(I2.2)',sc)
    endcase
  endfor

  return, xtickstr
  ; xticnum = n_elements(xtickstr) - 1

  ; NECESSARY KEYWORDS:
  ; plot, ...., xrange=[ut1,ut2] {required range}, \xstyle {==xstyle=1, exact axis limits}, xticks=xticnum, xtickn=xtickstr

  ; EXAMPLE :
  ;plot, ut2, P_th, /xstyle, /ystyle, xrange=[ut1,ut2],
  ;yrange=[.05,1.],pos=[0.,.7,1.,.8], thick=2,$
  ;      yticks=1,ytickv=[.1,1.],yminor=9, xticks=xticnum, xminor=5,
  ;xticklen=.04, ytitle='Pressure (nPa)', charsize=1.4,xtickn=xtickstr

End
;====================================================================================================================
PRO Label_Time, time_format ;, q = q

  if time_format eq 'hh:mm' then    q = LABEL_DATE(DATE_FORMAT=['%H:%I']) else $
  if time_format eq 'mm:ss' then    q = LABEL_DATE(DATE_FORMAT=['%I:%S']) else $
  if time_format eq 'hh:mm:ss' then q = LABEL_DATE(DATE_FORMAT=['%H:%I:%S'])
  if time_format eq 'd/m/y'    then q = LABEL_DATE(DATE_FORMAT=['%D/%N/%Y'])  ; N = month number
  if time_format eq 'm/d/y'    then q = LABEL_DATE(DATE_FORMAT=['%N/%D/%Y'])


End
;====================================================================================================================

;
;
;IDL EXAMPLE of a simple drawing program. Straight lines are connected to positions marked with the left or middle mouse buttons
;until the right button is pressed.
; another option (??): use DEFROI, but DEFROI does not function correctly when used with draw widgets. See CW_DEFROI.
PRO EX_DRAW, Xarr, Yarr, color=color
   ; Start with a blank screen:
   ;ERASE
   ; Get the initial point in normalized coordinates:
   CURSOR, X, Y, /NORMAL, /DOWN
   Xarr=X & Yarr=Y
   ; Repeat until right button is pressed. Get the second point.
   ; Draw the line. Make the current second point be the new first.
   WHILE (!MOUSE.button NE 4) DO BEGIN
      CURSOR, X1, Y1, /NORM, /DOWN
      PLOTS,[X,X1], [Y,Y1], /NORMAL, color=color , psym=5 ;, symsize=2
      ;print, x,x1,'     ', y,y1
      X = X1 & Y = Y1
      Xarr=[Xarr,X1] & Yarr=[Yarr,Y1]
   ENDWHILE
END
;====================================================================================================================
PRO SELECT_REGION
  ; opening the file (from IDL examples)
  file = FILEPATH('hurric.dat', SUBDIRECTORY = ['examples', 'data'])
  Nx=440  &   Ny=340
  image = READ_BINARY(file, DATA_DIMS = [Nx,Ny])

  DEVICE, DECOMPOSED = 0  & LOADCT, 0, /silent
  WINDOW, 0, XSIZE = Nx, YSIZE = Ny
  TVSCL, image

  ; selecting the region of interest; X and Y contain coordinates of points limiting the contour

  setcolors
  Ex_DRAW,X,Y,color=4 ; "1" for black, "0" for white bounding lines

  X=[X,X[0]] & Y=[Y,Y[0]]  ; to connect the first and the last points
  SPLINE_P, X, Y, XR, YR, INTERVAL=0.05  ;,TAN0=[0,1], TAN1=[0,1]
    ;TAN0, TAN1- tangents of the first and the last points
    ; TAN0=[0,1], TAN1=[0,1]  - for left or right points
    ; TAN0=[1,0], TAN1=[1,0]  - for bottom and top points
  PLOTS, Xr,Yr, linestyle=2, thick=2, color=2, /normal

  w = Polyfillv(Xr*Nx,Yr*Ny, Nx, Ny)  ; because the coordinates are obtained in /normal mode (for drawing purposes)

  image1 = image
  image1[*,*]=0
  image1[w]=image[w]
  WINDOW, 1, XSIZE = Nx, YSIZE = Ny
  ERASE
  TVSCL, image1

  ;XY= DEFROI(440, 340)
  ; defines arbitrarily shaped region of interest (no spline interpolation)
  ; there is source file

End
;====================================================================================================================
Function EmptyLabels

  empty_labels=replicate(' ',60)
  return, empty_labels

End
;====================================================================================================================
function plusminus
  return, string(177b)
end

;====================================================================================================================
function GetLogFactor, n_per_decade
; computes the ratio of adjacent ticks on the log axis for a given number
; points per decade

  return, 10.0^(1.0/(1.0*n_per_decade))

End
;====================================================================================================================

PRO Subtitle, plot_position, color, title
    setcolors
    xyouts, reform(plot_position[0])+0.012, plot_position[3]-0.018, title, color=color, /normal, charsize=!P.charsize*0.9
end
;====================================================================================================================
PRO LetterLabel, plot_position, color, title
  SetColors
  A = fltarr(100,100) ; black background

  if color ne 1 then A[0,99]=-1  ; white bacground

  tvscl,A, plot_position[0]+0.001,  plot_position[3]-0.021, xsize=0.04, ysize=0.02, /normal

  xyouts, plot_position[0]+0.007, plot_position[3]-0.016, title, color=color, /normal
end
;====================================================================================================================
pro LetterLabel1, str, size, vshift
  setkeyword, size, !P.charsize
  setkeyword, vshift, -0.015;0.01
  x0 = !x.window[0] + 0.005
  y0 = !y.window[1] + vshift
  xyouts,x0,y0, str, charsize=size, /normal
End
;====================================================================================================================
pro LetterLabel2, str, size=size, xyshift=xyshift, color=color
  setkeyword, size, !P.charsize
  setkeyword, xyshift, [0.01, 0.01]
  setkeyword, color, 0
  x0 = !x.window[0] + xyshift[0]
  y0 = !y.window[1] + xyshift[1]
  setcolors
  xyouts,x0,y0, str, charsize=size, /normal, color=color
End
;====================================================================================================================
PRO LineLegend, x,y,caption, charsize=charsize, linestyle=linestyle, symcolor=symcolor, color=color, psym=psym, thick=thick, dy=dy, fy = fy,symsize=symsize
  setcolors
  setkeyword, fy, 1.0  ; coefficient to adjust caption y-position
  oplot, x,y,linestyle=linestyle, color=color, thick=thick
  if n_elements(psym) gt 0 then oplot,x,y,psym=psym, color=symcolor, thick=thick, symsize=symsize

  y_text=y[1]
  if keyword_set(fy) then y_text=fy*y[1]
  if keyword_set(dy) then y_text=y[1]-dy
  xyouts, x[1]+ 0.2*(x[1]-x[0]), y_text, caption, charsize=charsize  ; /data by default ?
end
;====================================================================================================================
PRO LineLegend1, xpos, ypos, caption, charsize=charsize, linestyle=linestyle, symcolor=symcolor, color=color, psym=psym, thick=thick, dy=dy, fy = fy,symsize=symsize
  setcolors
  setkeyword, fy, 1.0  ; coefficient to adjust caption y-position
  plots, xpos, ypos, linestyle=linestyle, color=color, thick=thick, /normal
  if n_elements(psym) gt 0 then plots,xpos,ypos,psym=psym, color=symcolor, thick=thick, symsize=symsize, /normal

  y_text=ypos[1]
  if keyword_set(fy) then y_text=fy*ypos[1]
  if keyword_set(dy) then y_text=ypos[1]-dy
  xyouts, xpos[1]+ 0.2*(xpos[1]-xpos[0]), y_text, caption, charsize=charsize, /normal

end
;====================================================================================================================
function scistr, x, mant_format=mant_format
  setkeyword, mant_format, '(F5.1)'

  n = floor(alog10(abs(x)))
  a = abs(x)/10.0^n

  ;print, a,n
  n_s = 'x10!U'+ str(n)+'!N'
  ;a_s = string(signum(x)*a, format=mant_format)

  a_s = string((2*(x gt 0) -1)*a, format=mant_format)

  return, a_s+n_s

End

;====================================================================================================================
PRO DataArrow, x1,y1,x2,y2, v, T, Lambda, color=color
  setcolors

  arrow, x1,y1, x2,y2, thick=1.8,  /solid, /data, color=color

  xyouts, x1-0.04, y1-3, 'v = '+v+' km/s', /data, color=color
  xyouts, x1-0.04, y1-6, 'T = '+T+' s', /data, color=color
  xyouts, x1-0.04, y1-9, '!9l!X = '+ Lambda+' km', /data, color=color


End
;====================================================================================================================
PRO UTline, UT

 oplot, [ut, ut], [-1E5, 1E5]

End

;====================================================================================================================

function string_, v , format=format

  s = strtrim( string(v, format=format), 2)
  return, s
end
;===================================================================================================================
;  Conversion routines :
;====================================================================================================================

PRO Print_progress, n_current, n_total, n_increment
  ;setkeyword, n_increment, 10

  if n_current mod n_increment eq 0 then $

    print, string( 100*(n_current*1.0)/(n_total*1.0), format='(F5.2)'),' %' else $

  if n_current eq n_total then print, '100 % -- DONE.'


End
;====================================================================================================================

function IntToFname, i, length=length

  setkeyword, length, 6
  s = strcompress(i, /r)
  ;repeat s = '0'+s until strlen(s) ge length
  while not  (strlen(s) ge length) do s = '0'+s
  return, s

End

;====================================================================================================================

function Epoch_to_UT, Epoch_arr, y,m,d

  cdf_epoch, Epoch_arr[0], y,m,d, hh,mm,ss,ms,/br
  cdf_epoch, epoch_0, y,m,d, /comp
  return, (Epoch_arr - epoch_0)/(1000.*3600.) ; hours after midnight

End

;====================================================================================================================
function Str_to_str, s
  ; input format: 20-Aug-2010 18:00:07.840
  ; output format: 2010/08/20 18:00:07.840
  mm_arr = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  dd = strmid(s,0,2)
  yy = strmid(s,7,4)
  mm_ = strmid(s,3,3)
  mm = inttofname(1 + where(strmatch(mm_arr, mm_) eq 1), length=2)
  tt = strmid(s,12,12)
  return, yy + '/' + mm + '/' + dd + ' ' + tt

End
;====================================================================================================================
function Flip_date, s
  ; 15-03-2009/08:30:00.036  ->  2009-03-15/08:30:00.036

  d_s = (strmid(s,0,2))
  m_s = (strmid(s,3,2))
  y_S = (strmid(s,6,4))

  t_s = strmid(s,10,strlen(s)-10)

  return, y_s + '-' + m_s + '-' + d_s + t_s

End
;====================================================================================================================

function Str_to_Epoch, s
; Input format examples:
; 2009-03-15/08:30:00.036 ,  2009/03/15 08:30:00.036, 2009 03 15 08 30 00.036   etc
; separator can be any symbol or space
; adding leading zeros before 1-digit numbers is _necessary_
    y = fix(strmid(s,0,4))
    m = fix(strmid(s,5,2))
    d = fix(strmid(s,8,2))
    h = fix(strmid(s,11,2))
    mn= fix(strmid(s,14,2))
    sec = fix(strmid(s,17,2))
    ms = 1000*float('0'+strmid(s,19,4))
    CDF_EPOCH, epoch, y, m, d, h, mn, sec, ms, /COMPUTE_EPOCH
  return, epoch

End
;====================================================================================================================
;
; USEFUL FOR FUTURE:
;
; FAST reading array of strings of dates and times in the format '11-04-2013' and '23:00:27.749'
;
; fmt='(C(CDI2, 1X, CMOI2, 1X, CYI4, 1X, CHI2, 1X, CMI2, 1X, CSF7.4))'
;
; S_jul = DBLARR(N_ELEMENTS(S_date_s))
;
; READS, S_date_s+' '+S_time_s, S_jul, FORMAT=fmt
;
; S_jul contains Julian dates preserving milliseconds
;
;====================================================================================================================
function Str_arr_to_Epoch, s_arr, flip_date=flip_date
  ;flip_date=1  : for reading date/time strings like 15-03-2009/08:30:00.036
  ; otherwise 2009-03-15/08:30:00.036 expected as usual
  ;epoch = []

  for i=0LL, n_elements(s_arr)-1 do begin
    ;s = data_mfi.field1[i]+'/'+data_mfi.field2[i]

    if keyword_set(flip_date) then s=flip_date(s_arr[i]) else s=s_arr[i]

    epoch = add_elem(epoch, str_to_epoch(s))
    ;if i mod 10000 eq 0 then print, i/10000.0

  endfor

  return, epoch

End

;====================================================================================================================

function Str_to_UT, s
; Input format examples:
; 2009-03-15/08:30:00.036 ,  2009/03/15 08:30:00.036, 2009 03 15 08 30 00.036   etc
; separator can be any symbol or space
; adding leading zeros before 1-digit numbers is _necessary_

  h = fix(strmid(s,11,2))
  mn= fix(strmid(s,14,2))
  sec = fix(strmid(s,17,2))
  ms = 1000.0*float('0'+strmid(s,19,4))
  UT = h + mn/60.0 + sec/3600.0 + ms/(1000.0*3600.0)
  return, UT

End
;====================================================================================================================

function Str_to_DOY, s
; Input format examples:
; 2009-03-15/08:30:00.036 ,  2009/03/15 08:30:00.036, 2009 03 15 08 30 00.036
; OR just 2009-03-15, 2009/03/15  etc
; separator can be any symbol or space; only the first 10 characters with date info are used
; adding leading zeros before 1-digit numbers is _necessary_

    y = fix(strmid(s,0,4))
    m = fix(strmid(s,5,2))
    d = fix(strmid(s,8,2))
    days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    if y mod 4 eq 0 then days_in_month[1] = days_in_month[1] +1 ; leap year

    sum_of_days = fltarr(12) & for k=0,11 do sum_of_days[k]=total(days_in_month[0:k-1]) &  sum_of_days[0]=0

    DOY = fix( sum_of_days[m-1] + d )

    ;DOY[i] = sum_of_days[m-1] + (d-1) + hh/24.0 + mm/(24.0*60) + ss/(24.0*3600) + ms/(24.0*3600000)

    ;stop
  return, DOY

End

;====================================================================================================================

function DOY_to_Epoch, year, DOY, hh, mm, ss, ms

  cdf_epoch, epoch_0, year, 1, 1, 0, 0, 0, 0, /compute

  ms_day = 24.0*3600.0*1000.0
  ms_hh = 3600.0*1000.0
  ms_mm = 60.0*1000.0
  ms_ss = 1000.0

  epoch = epoch_0 + (DOY-1)*ms_day + hh*ms_hh + mm*ms_mm + ss*ms_ss + ms

  return, Epoch

End

;====================================================================================================================

function DOY_to_Epoch_, year, DOY
  ; same as DOY_to_Epoch, but DOY is a real number encoding  DOY, hh, mm, ss, and ms.

  cdf_epoch, epoch_0, year, 1, 1, 0, 0, 0, 0, /compute

  ms_day = 24.0*3600.0*1000.0

  epoch = epoch_0 + DOY*ms_day

  return, Epoch

End;====================================================================================================================

function Epoch_To_Str, epoch, MS = ms

  ss = strarr(n_elements(epoch))
  for i=0LL, n_elements(epoch)-1 do begin
  CDF_EPOCH, epoch[i], y, m, d, h, mn, sec, msec, /BREAK
  s = string(y, format='(I4.4)') +'/'+ string(m, format='(I2.2)') +'/'+ string(d, format='(I2.2)') +'T'+$
          string(h, format='(I2.2)')+':'+string(mn, format='(I2.2)')+':'+string(sec, format='(I2.2)')
  if n_elements(MS) ne 0 then s = s +'.' + string(msec, format='(I3.3)')
  ss[i] = s
  endfor
  if n_elements(ss) eq 1 then ss = ss[0]

  return, ss
End

;====================================================================================================================

function UT_To_Str, UT

  hh = floor(ut)
  mm = floor( (ut-hh)*60 )
  ss = floor( (ut - hh - mm/60.0)*3600.0  )
  return, string(hh, format='(I2.2)')+':'+string(mm, format='(I2.2)')+':'+string(ss, format='(I2.2)')


End

;====================================================================================================================
function UT_to_Epoch, UT, y, m, d

  setkeyword, d, 1
  setkeyword, m, 1
  setkeyword, y, 2000

  Epoch = dblarr(n_elements(UT))

  for i=0LL, n_elements(epoch)-1 do begin
    hh = floor(UT[i])
    mm = floor( (ut[i] - hh)*60.0D )
    ss = floor( (ut[i] - hh - mm/60.0D)*3600.0D  ) & if ss eq -1 then ss=0
    ms = floor( (ut[i] - hh - mm/60.0D - ss/3600.0D)*3600000.0D  ) & if ms eq -1 then ms=0

    cdf_epoch, epoch_, y, m, d, hh, mm, ss, ms, /compute

    epoch[i] = epoch_

  endfor
  if n_elements(epoch) eq 1 then epoch= epoch[0]

  return, epoch


End
;====================================================================================================================

function Epoch_to_DOY, epoch
; conversts epoch to fractional DOY

  DOY = dblarr(n_elements(epoch))


  for i=0LL, n_elements(epoch)-1 do begin

    cdf_epoch, epoch[i], y, m, d, hh, mm, ss, ms, /break


    if y mod 4 ne 0 then days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31] else $
                         days_in_month = [31,29,31,30,31,30,31,31,30,31,30,31]       ; leap year

    sum_of_days = fltarr(12) & for k=0,11 do sum_of_days[k]=total(days_in_month[0:k-1]) &  sum_of_days[0]=0

    DOY[i] = sum_of_days[m-1] + (d-1) + hh/24.0 + mm/(24.0*60) + ss/(24.0*3600) + ms/(24.0*3600000)
    ;DOY[i] = total(days_in_month[00:m-2])+ d + hh/24.0 + mm/(24.0*60) + ss/(24.0*3600) + ms/(24.0*3600000)

  endfor
  if n_elements(DOY) eq 1 then DOY = DOY[0]

  return, DOY

End

;====================================================================================================================

function Decday_to_DOY, decday_arr
  ; correction to the DOY time counts if a new year time is crossed
  DOY_Arr = decday_arr

  i_ = where( (DOY_Arr  - shift(DOY_Arr ,-1)) gt 300 )

  if (n_elements(i_) eq 1) and (i_[0] ne -1) then begin
    DOY_Arr[i_+1:*] = DOY_Arr[i_+1:*] + 365
    ;print, i_
    ;stop
  endif

  return, DOY_arr

End

;====================================================================================================================


function Epoch_to_Julian, epoch

  julian = dblarr(n_elements(epoch))
  for i=0LL, n_elements(epoch)-1 do begin
    cdf_epoch, epoch[i], y, m, d, hh, mm, ss, ms, /break
    julian[i] = julday(m, d, y, hh, mm, ss+ ms/1000.0 )
  endfor
  if n_elements(julian) eq 1 then julian = julian[0]

  return, julian

End

;====================================================================================================================

function Str_arr_to_Julian, s_arr

  return, epoch_to_Julian(str_arr_to_epoch(s_arr))

End
;====================================================================================================================

function Julian_to_Epoch, julian

  epoch = dblarr(n_elements(julian))

  for i=0LL, n_elements(epoch)-1 do begin

    caldat, julian[i], m,d,y, hh, mm, ss_ms
    ss = floor(ss_ms)
    ms = 1000.0*(ss_ms - ss)
    cdf_epoch, epoch_, y, m, d, hh, mm, ss, ms, /compute
    epoch[i] = epoch_
  endfor
  if n_elements(epoch) eq 1 then epoch = epoch[0]

  return, epoch

End
;====================================================================================================================

function txtfile_length, fname
  i=0
  openr,1,fname
  while not eof(1) do begin
    readf,1,s & i=i+1
  endwhile
  close,1
  return, i
End
;====================================================================================================================
