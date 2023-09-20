function ADAPT_THRESH, IMG_p_enh, k=k

  ;+
  ; NAME:
  ;     ADAPT_THRESH
  ;
  ; PURPOSE:
  ;     Calculates adaptive threshold values of an image array using the provided normalized threshold level
  ;
  ; CALLING SEQUENCE:
  ;    thresholds = ADAPT_THRESH(Image_array, k=)
  ;
  ; INPUTS:
  ;     IMG_p_enh 	 -   2D array representing the studied solar image. For best results, the image must be enhanced and detrended, with pixel values approximately centered around 0.
  ;		k						-   number of standard deviations about the median used for computing adaptive threshold. Default value 1.0.
  ;
  ; OUTPUTS:
  ;     2 x 2 array containing two pairs of thresholds arranged as follows (the format used by feature detection routines):
  ;		[ 	[lower_negative_threshold, upper_negative threshold],
  ;  		[lower_positive_threshold, upper_positive_threshold]   ]
  ;
  ;		At present, upper_negative threshold =lower_positive_threshold=0.0 (could be adjusted to remove noisy pixels from consideration)
  ;
  ; RESTRICTIONS:
  ;     none
  ;
  ; EXAMPLE:
  ;     thresholds = ADAPT_THRESH(IMG_p_enh, k=0.5)
  ;
  ; MODIFICATION HISTORY:
  ;       V. Uritsky, 2019-2022
  ;-
  ;----------------------------------------------------------------


  if n_elements(k) eq 0 then k=2.0

  w_p = where(IMG_p_enh gt 0)
  w_n = where(IMG_p_enh lt 0)

  thresh_p = median(IMG_p_enh[w_p]) + k*stddev(IMG_p_enh[w_p])
  thresh_n = median(IMG_p_enh[w_n]) - k*stddev(IMG_p_enh[w_n])


  return, [[thresh_n, 0.0],[0.0, thresh_p]]

End
