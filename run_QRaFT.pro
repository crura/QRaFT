function run_QRaFT

  files = file_search('/Users/crura/Desktop/Research/idlroutines/STEREO_Data_Processing/Naty_Images/2012b/*.fits')
  for i=0, n_elements(files)-1 do begin
  Features = process_corona(files[i], 'COR1', exten_no=1, thresh_k=2, /manual, /save, /old)
  endfor
  return, files
End


function get_input_files, input_directory
    directory = input_directory

  ; Create an empty list to store the file paths
  file_paths = []

  ; Get a list of all the files in the directory
  files = FILE_SEARCH(directory)

  ; Loop through each file in the list
  for i = 0, N_ELEMENTS(files) - 1 do begin
      ; Get the full path to the file
      file_path = files[i]

      ; Check if the path is a file (not a directory)
      if FILE_TEST(file_path, /regular) then begin
          ; Append the path to the list of file paths
          file_paths = [file_paths, file_path]
      endif
  endfor
return, files
end
