# --- Set your path here ---
target_directory$ = "/Users/nwang40/Documents/Gitrepo/Spanish-DL/stimuli/d-t/dip_tip/!final VOT continuum_selected_70dBnormed/"
target_intensity_db = 70

# Get list of wav files
Create Strings as file list: "file_list", target_directory$ + "*.wav"
number_of_files = Get number of strings

for file_index from 1 to number_of_files
    selectObject: "Strings file_list"
    current_filename$ = Get string: file_index

    Read from file: target_directory$ + current_filename$
    sound_name$ = selected$("Sound")

    Scale intensity: target_intensity_db

    Save as WAV file: target_directory$ + current_filename$

    Remove
endfor

selectObject: "Strings file_list"
Remove

writeInfoLine: "Done. Overwrote ", number_of_files, " files, scaled to ", target_intensity_db, " dB."