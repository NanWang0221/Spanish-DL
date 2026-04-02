# # ## ### ##### ########  #############  ##################### 
## This script automates the creation of series of prevoicing durations by set increments. 
## The original prevoiced sample from which the prevoicing durations are to be 
## manipulated must first be extracted and loaded into the objects window of Praat.
# # ## ### ##### ########  #############  #####################
#
# # ## ### #####
## Maryann Tan
## March 2021
# # ## ### #####

# Name the directory you want the files saved
dir$ = "/Users/willchang/Dropbox/Research/UC Irvine/Research/research projects/Xin/NIH accent adaptation/stimuli recording/Spanish speakers samples/speech recording - RC/pre-voicing segment"
prevoicing = Read from file: dir$ + "/" + "RC_d_pre-voicing.wav"


nDurations = 20
vot_increment = 5
selectObject: prevoicing
duration_prevoice = Get total duration
duration_prevoice_ms = duration_prevoice * 1000
output_length = vot_increment 

for i from 1 to nDurations
appendInfoLine: output_length
duration_factor = output_length/duration_prevoice_ms
appendInfoLine: duration_factor
selectObject: prevoicing
Lengthen (overlap-add): 60, 300, 'duration_factor'
name$ = selected$("Sound")
Rename: name$ - "0" + "'output_length'" + "ms"
name$ = selected$("Sound")
Save as WAV file: dir$ + "/" + name$ + ".wav"
output_length = output_length + vot_increment
endfor


