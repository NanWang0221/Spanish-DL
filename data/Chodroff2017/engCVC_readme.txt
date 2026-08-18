A few notes on the cueAnalysis_engCVC.txt dataset.

The columns are: file, stop, stop start time, stop end time, interval number in TextGrid, VOT, following sonorant, following sonorant duration, word, word duration, f0_1, f0_2, f0_3, f0_4, f0_5, f0_6, f0_7, f0_8, f0_9, f0_10.
 
	1	It includes CLVC and CVC words. The CLVC words are ones like “KLOT” or “PLOT”. The following sonorant will indicate when the second segment is an “L”. You should probably remove these rows from your calculations.
	2	F0 is extracted using the To Pitch… function in Praat with a floor of 75 Hz and a ceiling of 600 Hz. F0 is extracted every 5 ms with f0_1 extracted from 5ms after the start of the vowel.
	3	Sonorant duration will be vowel duration once you remove the instances of “L”.

The participants are JHU students from 2013. Gender information is included in engCVC_speakerList. Further details are described in Chodroff & Wilson (2017) and Chodroff & Wilson (2018).

engcvc.R, eng_cvc.RData, mixer6_vot.R, and mixer6_vot.RData were the data and scripts used for the Chodroff & Wilson (2017) paper. 