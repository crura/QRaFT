 FUNCTION PROCESS_CORONA, fname, data_source, FIT_MODE, exten_no=exten_no, adaptive = adaptive, thresh_k=thresh_k, trend_k= trend_k, $
				save=save, old_win=old_win, manual_filter=manual_filter, silent=silent, $
				IMG_orig=IMG_orig, IMG_enh=IMG_enh, IMG_p_enh=IMG_p_enh, P=P

  ;+
  ; NAME:
  ;     PROCESS_CORONA
  ;
  ; PURPOSE:
  ;     The main (top-level) procedure enabling an automatic or semi-automatic detection of
  ;     quasi-linear field-aligned coronal features in a coronagraph image.
  ;
  ;     The detection involves a sequence of adaptive processing steps using a set of
  ;     parameters optimized for a specifoic image type.
  ;
  ;     The processing steps are implemented by the lower-level package modules
  ;     and include the following manipulations:
  ;
  ;       1) Angular edge-enhancement of the studied coronal image,
  ;       2) Reduction of its noise level based on intensity and pixel clustering criteria,
  ;       3) Detection and analytical approximation of the field-aligned coronal features,
  ;       4) Validation of the obtained set of features using automatic and manual filtering.
  ;
  ;     The detection results can be (a) saved to a file, (b) plotted on the computer screen,
  ;     and/or (c) returned to the next-higher program level as runtime data structures and arrays.
  ;
  ;     Batch-mode processing of large sets of images is supported in a fully automatic mode.
  ;
  ; CALLING SEQUENCE:
  ;     FEATURES = PROCESS_CORONA(fname, data_source, FIT_MODE, /adaptive, thresh_k=, trend_k=, /save, /old_win, /manual_filter,  IMG_orig=, IMG_enh=, IMG_p_enh=, P=,)
  ;
  ; INPUTS:
  ;     fname         - the name of the file to be opened and processed
  ;     data_source   - string constant defining the type of the image. Allowed values: 'MLSO2014', 'MLSO2016', 'ECL1', 'COR1', 'COR1_RAW', 'PSI'  (case sensitive).
  ;                     Used to identify the name of the file containing image-specific processing parameters.
  ;     FIT_MODE      - string constant (case-insensitive) defining the feature interpolation method
  ;                     used by funciton DETECT_FEATURES().
  ;
  ;                     Allowed FIT_MODE values (case-insensitive):
  ;
  ;                     FIT_MODE = 'ELP'  - features are interpolated by ellipses
  ;                     FIT_MODE = 'POLY1'  - features are interpolated by 1st degree polynomials (straight lines) in polar coordinates
  ;                     FIT_MODE = 'POLY2'  - features are interpolated by 2nd degree polynomials in polar coordinates
  ;                     FIT_MODE = 'POLY3'  - features are interpolated by 3rd degree polynomials in polar coordinates
  ;						FIT_MODE not specified - polynomial interpolation with an optimized  order (1, 2, or 3) selected depending on the length of each feature
  ;
  ;		EXTEN_NO - integer keyword used by OPEN_FILE to open some FITS files
  ;		ADAPTIVE - integer keyword controlling adaptive thresholfing. Default value = 1 (adaptive mode is on)
  ; 	THRESH_K - number of standard deviations about the median used for computing adaptive threshold. Default value 1.0.
  ;		TREND_K  - coefficient applied to the minimum value of intensity along the feature computed using averaged radial trend. Default value 1.0.
  ;							Note: THRESH_K and TREND_K are ignored if ADAPTIVE = 0.
  ;
  ;     save          - boolean keyword indicating whether the detected image features will be saved to a file. The keyword is NOT set
  ;                     by default (=features are not saved).
  ;     old_win       - boolean keyword. Set this keyword to use the current preexisting plotting window
  ;                     when plotting in the DEVICE context. Called by the procedure PLOT_FEATURES.
  ;                     Optional; the default value is 0 (plotting in a new window).
  ;     manual_filter - boolean keyword indicating whether a manual feature filtering (validation) is performed by the function  FILTER_FEATURES_MANUAL()
  ;                     applied after the automatic filtering done by by FILTER_FEATURES(). The keyword is NOT set
  ;                     by default (features are not validated manualy, only automatically).
  ;		silent   - integer keyword turning on/off silent mode of Detect_features() calling Polyfit_decomp(). If silent=1, no messages are displayed. Default value 0.
  ;
  ; OUTPUTS:
  ;     FEATURES      - returne function value; represents an array of IDL structures
  ;				containing the location, orientation, shape and other parameters of the detected features.
  ;                     Created by funciton DETECT_FEATURES() and  validated by FILTER_FEATURES() and FILTER_FEATURES_MANUAL() (if /manual_filter is set).
  ;     IMG_orig      - 2D data array containing the original (unprocessed) image obtainined from the file "fname"
  ;     IMG_enh       - 2D data array containing edge-enhanced image in the rectangular (Cartesean) coordinates
  ;     IMG_p_enh     - 2D data array containing edge-enhanced image in the plane polar coordinates
  ;     P             - IDL structure containg the set of adjustable processing parameters used for detecting the features,
  ;                     retrieved from file using P=READ_PARAMETERS()
  ;
  ;     In addition to these numerical outputs, the features are saved to an IDL save file if the keyword /save is set (see above).
  ;
  ; RESTRICTIONS:
  ;     none
  ;
  ; EXAMPLES (use provided image files):
  ;
  ;		 f1 = '20140518_224612_kcor_l1.fts'
  ;		 f2 = '20160501_165853_kcor_l1.fts'
  ;		 f3 = 'cor1_512x512_w50w5_20140501133500.fits'
  ;
  ;     features = PROCESS_CORONA(f1, 'MLSO2014', 'POLY1', /MANUAL_FILTER, /save)
  ;
  ;       (opens a sample 2014 MLSO image with original calibration;
  ;			uses uses adaptive thresholding and 1st order polynomial fitting,
  ;        applies automatic and manual feature filtering;
  ;        saves detected features to an IDL save file with same file basename and extension "*.sav")
  ;
  ;     features = PROCESS_CORONA(f2, 'MLSO2016', 'POLY2')
  ;
  ;       (opens a sample 2016 MLSO image with updated calibration;
  ;			uses adaptive thresholding and 2nd order polynomial fitting,
  ;        applies automatic filtering only;
  ;        does not save the detected features)
  ;
  ;		 features = PROCESS_CORONA(f3, 'COR1', /silent, thresh_k=0.5, trend_k=2.0)
  ;       (opens a sample STEREO COR1  image;
  ;			uses aadaptinve thresholding with k=0.5, adaptive feature filtering with k=2.0, and adaptive  polynomial fitting,
  ;        applies automatic filtering only;
  ;        does not save the detected features,
  ;		   runs in silent mode. )

  ;
  ; 	ADDITIONAL EXAMPLES (images not provided):
  ;
  ;  	ff = file_search("..\MLSO\Apr-Jun 2014\*.fts") & ff = ff[ sort(ff) ]
  ;
  ;		PROCESS & SHOW for Manual filtering, without  saving:

  ; 		features = PROCESS_CORONA(ff[0], 'MLSO2016', 'POLY1', IMG_orig=IMG_orig, IMG_enh=IMG_enh, IMG_p_enh=IMG_p_enh, P=P, old_win=old_win,v/MANUAL_FILTER)
  ;
  ; 	PROCESS, SAVE a set of MLSO FITS files, without manual filtering:
  ;
  ; 		for i=0, n_elements(ff)-1 do features_temp  = PROCESS_CORONA(ff[i], 'MLSO2016', 'POLY1', IMG_orig=IMG_orig, IMG_enh=IMG_enh, IMG_p_enh=IMG_p_enh, P=P, /save, old_win=old_win)
  ;
  ;
  ;    BATCH-PROCESSING MULTIPLE STEREO COR1 IMAGES:
  ;
  ;	 		fnames = file_search( "..\COR1\TESTS_20140501\*.fts" )
  ;    		for i=0, n_elements(fnames)-1 do $
  ;    		features_temp = PROCESS_CORONA(fnames[i], 'COR1', 'POLY1', /manual, /save, /old)
  ;
  ;     DISPLAYING A PROCESSED STEREO COR1 IMAGE (ENHANCED CORONA + DETECTED FEATURES):
  ;
  ; 		restore, "..\COR1\TESTS_20140501\20140501_133500_0P4c1A.fts.sav"
  ;    		PLOT_FEATURES, IMG_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds)
  ;
  ;
  ;
  ; MODIFICATION HISTORY:
  ;       V. Uritsky, 2019-2022
  ;-
  ;----------------------------------------------------------------



  ; Edit this line to direct the code to the folder containing the parameter file "fname_par" (see below)
  ;cd, "c:\Users\vadim\Documents\IDL\QRaFT"

  if keyword_set(silent) then begin
  ;  Suppress error messages:
    !quiet = 1 & !except = 0
  endif

   ; Set default values of keywords controling adaptive thresholding:
   setkeyword, adaptive, 1
   setkeyword, thresh_k,1.0
   setkeyword, trend_k,1.0

   ;print, 'File name: '+ file_basename(fname)

   ; Deffining file with initializatsion parameters:
   if data_source eq 'MLSO2014' then fname_par = "MLSO_2014_parameters.txt"  ; MLSO K-cor images, original calibration
   if data_source eq 'MLSO2016' then fname_par = "MLSO_2016_parameters.txt"  ; MLSO K-cor images, updated calibration
   if data_source eq 'COR1' 	then fname_par = "COR1_parameters.txt"          		 ; STEREO COR1 images (filtered)
   if data_source eq 'COR1_RAW' then fname_par = "COR1_parameters_raw.txt"	 ; STEREO COR1 images (no filters applied)
   if data_source eq 'PSI' 	then fname_par = "PSI_parameters.txt"             					;  density slices from PSI MAS simulation runs
   if data_source eq 'ECL1' 	then fname_par = "ECL1_parameters.txt"          		   ; Ground-nased eclipse images (TBD depending on image sourse)
   if data_source eq 'PSI_MLSO' then fname_par = "PSI_MLSO_Parameters.txt"

   P = READ_PARAMETERS(fname_par)

   IMG_orig = OPEN_FILE(fname, P, exten_no=exten_no, header=header)


   IMG_enh = ENHANCE_IMAGE_ANGULAR(IMG_orig, P.XYCenter, P.Rs)

   IMG_p_enh = ENHANCE_IMAGE_POLAR(IMG_orig, P, X_p=X_p, Y_p=Y_p, Phi_p=Phi_p, Rho_p=Rho_p, img_p_filt=img_p_filt)

   if ADAPTIVE eq 1 then begin
    ; Adaptive thresholds for featyure detection and validation:
     P.blob_detect_thresh_pairs = 2
     P.blob_detect_thresholds = ADAPT_THRESH(IMG_p_enh, k=thresh_k)
     P.Flux_rho_trend_range   = ADAPT_TREND_RANGE(IMG_orig, P.XYCenter, P.d_phi, P.d_rho, k=trend_k)

   endif

   for k=0, n_elements(P.blob_detect_thresholds[0,*])-1 do begin

     if n_elements(FIT_MODE) ne 0 then begin
	   ; -------- USING PRESCRIBED POLYFIT ODER ------------
       FEATURES_ = DETECT_FEATURES(IMG_p_enh, P, X_p, Y_p, Phi_p, Rho_p, [P.blob_detect_thresholds[0,k], $
                   P.blob_detect_thresholds[1,k]], FIT_MODE, silent=silent)
	   if k eq 0 then FEATURES = [ FEATURES_] else $
        FEATURES = [FEATURES, FEATURES_]
	 endif else begin

	   ; -------- OPTIMIZATION OF POLYFIT ODER ------------

	   FEATURES_poly1 = DETECT_FEATURES(IMG_p_enh, P, X_p, Y_p, Phi_p, Rho_p, [P.blob_detect_thresholds[0,k], $
	 							P.blob_detect_thresholds[1,k]], 'POLY1',  silent=silent)
	   FEATURES_poly2 = DETECT_FEATURES(IMG_p_enh, P, X_p, Y_p, Phi_p, Rho_p, [P.blob_detect_thresholds[0,k], $
	   							P.blob_detect_thresholds[1,k]], 'POLY2',  silent=silent)
	   FEATURES_poly3 = DETECT_FEATURES(IMG_p_enh, P, X_p, Y_p, Phi_p, Rho_p, [P.blob_detect_thresholds[0,k], $
								P.blob_detect_thresholds[1,k]], 'POLY3',  silent=silent)

 	   ind_arr = [-1]
 	   for i=0, n_elements(FEATURES_poly1)-1 do begin

 	   ; Polynmial order optimizedbased on chi-squared statistics normalized by feature length (NOT in use in current version):
 	   ;  chisq_min = min(reform([FEATURES_poly1[i].chisq/FEATURES_poly1[i].L, $
 	   ;                          FEATURES_poly2[i].chisq/FEATURES_poly2[i].L, $
 	   ;                          FEATURES_poly3[i].chisq]/FEATURES_poly3[i].L), ind)
 	   ;  ind_arr = [ind_arr,ind+1]
 	   ;  case ind of
 	   ;    0: FEATURE_ = FEATURES_poly1[i]
 	   ;    1: FEATURE_ = FEATURES_poly2[i]
 	   ;    2: FEATURE_ = FEATURES_poly3[i]
 	   ;  endcase

 	   ; Polynmial order optimized by feature length:
 	   	 L = FEATURES_poly3[i].L

 	     if L lt 40 then $
 	       FEATURE_ = FEATURES_poly1[i] else $
 	     if ((L ge 40) and (L lt 60)) then $
 	       FEATURE_ = FEATURES_poly2[i] else $
 	       FEATURE_ = FEATURES_poly3[i]

	     if n_elements(FEATURES) eq 0 then FEATURES = [FEATURE_] else $
	     FEATURES = [FEATURES, FEATURE_]

	   endfor

     endelse

   endfor

   FEATURES = FILTER_FEATURES(FEATURES, IMG_enh, P, w=w)
   if w[0] eq -1 then  RETURN, -1

   if (not keyword_set(old_win)) then $
     window, xsize=1000, ysize=850, /free, title=''
   erase

   !P.noerase=1
   !P.charsize=1.2

    if (n_elements(FEATURES) gt 0) then begin

	 PLOT_FEATURES, IMG_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds), title=file_basename(fname), old=old_win

 	 if keyword_set(manual_filter) then begin
       ;print, 'Manual removal of false features. Left-click on features to be removed, right-click to finish.'
       FEATURES = FILTER_FEATURES_manual(FEATURES, IMG_enh, P)
	 endif

     ;PLOT_FEATURES, IMG_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds), title=file_basename(fname), /old

   endif

 	 dpos = [0.06, 0.06, -0.08, -0.06]
   	 erase

       PLOT_FEATURES, IMG_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds), title='Enhanced', $
                      position=[0.02,0.53, 0.46, 1.0]+dpos, /old
       PLOT_FEATURES, IMG_orig, FEATURES, P, range=minmax(P.blob_detect_thresholds), title='Original', $
                      position=[0.52,0.53, 0.96, 1.0]+dpos, /old
       PLOT_FEATURES, IMG_p_enh, FEATURES, P, range=minmax(P.blob_detect_thresholds)/2.0, title=file_basename(fname), /polar, $
       				  position=[0.02,0.0,0.95, 0.5]+dpos, /old

 	   !P.noerase=0
 	   !P.charsize=1.00


   if keyword_set(save) then $
     save, filename=fname +'.sav', FEATURES, IMG_orig, IMG_enh, IMG_p_enh, P, header, fname


    !quiet = 0 & !except =1

   RETURN, FEATURES

 END
