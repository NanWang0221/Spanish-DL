# This script does the following:
#	Measures the VOT of the automatically/manually aligned boundaries
#	Gets the label of the stop, word, and vowel
#	Gets the following vowel duration
#	Gets the pitch listing for the first 50ms of the vowel at 5ms intervals
# Input: lab CVC corpus
# July 19, 2016 Eleanor Chodroff

dir$ = "/Volumes/MIXER6_cogsci/engCVC/wav_48kHz"
dir2$ = "/Volumes/MIXER6_cogsci/engCVC"
outputfile$ = "/Volumes/MIXER6_cogsci/engCVC/cueAnalysis_engCVC.txt"

Create Strings as file list... files 'dir$'/*.wav
Sort
nFiles = Get number of strings

Create Strings as file list... textgrids 'dir2$'/*_autovot.TextGrid
Sort
#pause 'nFiles'
deleteFile(outputfile$)

pitch$ = "TRUE"

for i from 1 to nFiles
	select Strings files
	filename$ = Get string... i
	#pause 'filename$'
	basename$ = filename$ - ".wav"
	Read from file... 'dir$'/'basename$'.wav
	select Strings textgrids
	textgridname$ = Get string... i
	textgrid$ = textgridname$ - ".TextGrid"
	Read from file... 'dir2$'/'textgrid$'.TextGrid
	select Sound 'basename$'
	if pitch$ = "TRUE"
	#	To Pitch... 0.0 60 650
	#	To Pitch (cc)... 0.0 75 15 no 0.03 0.45 0.01 0.35 0.14 600
	#	To Pitch... 0.0 75 500
		To Pitch... 0.0 75 600
	endif
	select TextGrid 'textgrid$'
	phonetier = Get number of intervals... 1
	wordtier = Get number of intervals... 2
	# phonetier will always end with 'AGAIN'
	for j from 1 to phonetier-3
		select TextGrid 'textgrid$'
		phone1$ = Get label of interval... 1 j
		phone2$ = Get label of interval... 1 j+1
		phone3$ = Get label of interval... 1 j+2
		if index_regex(phone1$, "^[PTKBDG]") & index_regex(phone2$, "^[LAEIOU]") & index_regex(phone3$, "^[TAEIOU]")
			start = Get start point... 1 j
			end = Get end point... 1 j
			vot = end - start
			son_start = Get start point... 1 j+1
			son_end = Get end point... 1 j+1
			son_dur = son_end - son_start
			fileappend 'outputfile$' 'basename$''tab$''phone1$''tab$''start''tab$''end''tab$''j''tab$''vot''tab$''phone2$''tab$''son_dur'
			call getStop 'basename$' 'son_start' 'phonetier' 'phone1$'
			#pause
			fileappend 'outputfile$' 'newline$'
		endif
	endfor
	#pause
	select Sound 'basename$'
	if pitch$ = "TRUE"
		plus Pitch 'basename$'
	endif
	plus TextGrid 'textgrid$'
	Remove
endfor

procedure getStop basename$ son_start phonetier phone1$
	select TextGrid 'textgrid$'
	int_w = Get interval at time... 2 son_start
	word$ = Get label of interval... 2 int_w
	w_start = Get start point... 2 int_w
	w_end = Get end point... 2 int_w
	w_dur = w_end - w_start
	fileappend 'outputfile$' 'tab$''word$''tab$''w_dur'
	#pause
	if pitch$ = "TRUE"
		call getPitch 'basename$' 'son_start'
	endif
endproc

procedure getPitch basename$ son_start
	select Pitch 'basename$'
	for p from 1 to 10
		pitch = Get value at time... son_start+p*0.005 Hertz Linear
		fileappend 'outputfile$' 'tab$''pitch'
	endfor
endproc