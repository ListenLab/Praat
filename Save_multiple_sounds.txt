# Start with an input form
form Enter directory to save your sounds
     sentence directory C:\Type Directory Here
endform

# Ask the user to select the sounds they wane to save
pause select all sounds you want to save
numberOfSelectedSounds = numberOfSelected ("Sound")

# Assign an object number to each sound
for thisSelectedSound to numberOfSelectedSounds
	sound'thisSelectedSound' = selected("Sound",thisSelectedSound)
endfor

# Loop through the sounds
for thisSound from 1 to numberOfSelectedSounds
    select sound'thisSound'
	name$ = selected$("Sound")

	# Old style of Praat coding, but it still works
	do ("Save as WAV file...", directory$ + "\" + name$ + ".wav")

endfor

#re-select the sounds
select sound1
for thisSound from 2 to numberOfSelectedSounds
    plus sound'thisSound'
endfor