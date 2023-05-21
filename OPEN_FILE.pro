
FUNCTION OPEN_FILE, fname,  P, exten_no=exten_no, header=header

  ;+
  ; NAME:
  ;     OPEN_FILE
  ;
  ; PURPOSE:
  ;     Opens a file with a solar image to be processed.
  ;     File type depends on the image type.
  ;
  ; CALLING SEQUENCE:
  ;     IMG = OPEN_FILE(fname, P, header=)
  ;
  ; INPUTS:
  ;     fname   - string variable containing the name of the file to be opened
  ;		EXTEN_NO - integer keyword used by OPEN_FILE to open some FITS files
  ;     P       - structure of processing parameters, retrieved from file using P=READ_PARAMETERS()
  ;               The type of the image file is specified by P.data_source.
  ;
  ; OUTPUTS:
  ;     IMG     - 2D array containing the opened image
  ;     header  - a string array of FITS header lines if the file of FITS type,
  ;               an empty string otherwise.
  ;
  ; RESTRICTIONS:
  ;     none
  ;
  ; EXAMPLE:
  ;     IMG = OPEN_FILE('20160501_165853_kcor_l1.fts', P, header=header)
  ;
  ;     (P is a structure of processing parameters created before calling OPEN_FILE
  ;     using P=READ_PARAMETERS(). See PROCESS_FILE for more details.)
  ;
  ; MODIFICATION HISTORY:
  ;       V. Uritsky, 2019-2022
  ;-
  ;----------------------------------------------------------------

  case P.data_source of

    'MLSO2014': begin
       img = readfits(fname, header, exten_no=exten_no, /silent)
    end
    'MLSO2016': begin
      img = readfits(fname, header, exten_no=exten_no, /silent)
      img = congrid(img,1024,1024)
      ;w = where((img lt 0) or ~finite(img)) & if w[0] ne -1 then img[w]=0.0
      ;img = rebin(rebin(img,512,512), 1024,1024) ; despiking COR1
      img= float(remove_occult(img,P))
    end
    'COR1': begin
       img = readfits(fname, header, exten_no=exten_no, /silent)
       w = where((img lt 0) or ~finite(img)) & if w[0] ne -1 then img[w]=0.0
       img = rebin(rebin(img,256,256), 512,512) ; despiking COR1
       img=float(remove_occult(img,P))
    end
    'COR1_RAW': begin
       img = readfits(fname, header, exten_no=exten_no, /silent)
       w = where((img lt 0) or ~finite(img)) & if w[0] ne -1 then img[w]=0.0
       img = rebin(rebin(img,256,256), 512,512) ; despiking COR1
       img = scale_array(img, [0,255]) ; enforcing dynamic range of raw COR1 image
       ;img = img/(max(img)/0.1) ; making COR1 dynamic range close to MLSO2016

       img=float(remove_occult(img,P))
    end
    'PSI': begin
       img = readfits(fname, header,exten_no=exten_no,  /silent)
       img = congrid(img,512,512)
       w = where((img lt 0) or ~finite(img)) & if w[0] ne -1 then img[w]=0.0
       img=float(remove_occult(img,P))

    end
    'PSI_MLSO': begin
      img = readfits(fname, header, exten_no=exten_no, /silent)
      w = where((img lt 0) or ~finite(img)) & if w[0] ne -1 then img[w]=0.0
      img=remove_occult(img,P)

    end

    'ECL1': begin
       img = read_png(fname)
       ;read_png, fname, img
       img = float(reform(img[P.png_channel,*,*]))
      ; removing pixels inside the solar disk
      for x=0, n_elements(img[*,0])-1 do for y=0, n_elements(img[0,*])-1 do $
        if (x-P.XYCenter[0])^2+(y-P.XYCenter[1])^2 le P.Rs^2 then img[x,y]=0
      ; rotating the image to achieve standard orientation
      img = rot(img, P.rot_angle,  1, P.XYCenter[0], P.XYCenter[1], /interp, /pivot, missing=0)
      ; removing edges affected by the rotation:
      img = img[P.XYcenter[0]-P.d_roi:P.XYcenter[0]+P.d_roi, P.XYcenter[1]-P.d_roi:P.XYcenter[1]+P.d_roi]
      ; correction of center coordinates after removing edges:
      P.XYCenter = P.d_roi+1
      header = ''
    end

  endcase

  return, img
END
