library(dplyr)
library(ggplot2)
library(reshape2)

# Note: this sript assumes that you have saved a Praat Spectrogram 
# using the function "Save to text file. "
# It will not work if you save to a 'short text file',
# and will not work if you save to a 'binary file' 

# Here's the main function: 
convert_spectrogram_to_df <- function(spect_name){
  # e.g. spect_name = "Time.Spectrogram"
  
  # Read the text file
  spect <- readLines(spect_name) %>% 
    colsplit(., pattern = " = ", names = c("coordinate","Value")) 
  
  # Skip the first three rows
  spect <- spect[4:nrow(spect),] %>%
    mutate(Value = as.numeric(Value))
  
  # extract info from the file
  Freq_floor <- spect[spect$coordinate=="ymin", "Value"]
  Freq_max <- spect[spect$coordinate=="ymax", "Value"]
  Time_start <- spect[spect$coordinate=="xmin", "Value"]
  Time_end <- spect[spect$coordinate=="xmax", "Value"]
  num_times <- spect[spect$coordinate=="nx", "Value"]
  time_bin_size <- spect[spect$coordinate=="dx", "Value"]
  num_freq_bins <- spect[spect$coordinate=="ny", "Value"]
  freq_bin_size <- spect[spect$coordinate=="dy", "Value"]
  
  # Get the actual intensity values here
  spect_vals <- spect$coordinate[12:length(spect$coordinate)] %>% 
    # thsoe values are loaded into the variable 'meas'
    data.frame(meas = .) %>%
    # convert to character
    mutate(meas = as.character(meas)) %>%
    # look for :  - if yes, then mark it as a frequency step
    mutate(is.Freqstep = ifelse(grepl(x = meas, pattern=":"), 1, 0)) %>%
    # tally the number of frequency steps
    mutate(Freqstep = cumsum(is.Freqstep)) %>%
    # calculate the time step
    mutate(Timestep = {meas %>% 
        colsplit(., pattern = "\\]|\\]", names = c("junk","Timebin")) %>%
        mutate(Timebin = gsub(replacement = "", x = Timebin, pattern="\\[|\\]")) %>%
        # encode the time bin as an index
        mutate(Timebin = as.numeric(Timebin)) %>%
        `[[`(., "Timebin")}) %>%
    #-------------------------------------------------------#
    # declare the actual time value
    # by multiplying the index by the size of the time bin
    mutate(Time = (Timestep - 1) * time_bin_size) %>%
    # get the value for the *next* timebin
    # for drawing tiles
    mutate(Time_next = (Timestep) * time_bin_size) %>%
    #-------------------------------------------------------#
    # do the same for the frequency value
    mutate(Frequency = (Freqstep - 1) * freq_bin_size) %>%
    # get teh value for the *next* frequency bin
    mutate(Frequency_next = (Freqstep) * freq_bin_size) 
  
  # This is the reference value for 0 dB
  reference_SP <- 0.00002
  
  # Build the output data frame
  spectrogram <- cbind(spect[12:nrow(spect),], spect_vals) %>%
    mutate(filename = spect_name) %>%
    select(filename, Timestep, Freqstep, 
           Time, Time_next, 
           Frequency, Frequency_next, 
           Pressure = Value) %>%
    # Convert sound pressure to dB scale
    mutate(Level = 20 * log10(Pressure / reference_SP))
  
  # Get rid of NA values (don't know why this is necessary, but it is)
  spectrogram <- spectrogram[!is.na(spectrogram$Time),]
  
  rm(spect, spect_vals)
  return(spectrogram)
}

constrain_dynamic_range <- function(spectrogram_object, 
                                    column = "Level", 
                                    upper_limit = 120,
                                    dynamic_range = 60){
  # Here's how to constrain dynamic range
  # with reference to the highest value
  # (the way you'd do in Praat)
  
  # First make a copy of the Level variable,
  # so that you don't lose information
  intensity_values <- 
    spectrogram_object[column]

  if(is.null(upper_limit)){
    upper_limit <- max(intensity_values, na.rm=TRUE)
  }
  
  # truncate the upper limit
  intensity_values[intensity_values > upper_limit] <- upper_limit
  
  
  # Remove anything that's more than [DR] below the uppr limit
  intensity_values[
    intensity_values < (upper_limit - dynamic_range)] <- NA
  
  spectrogram_object[paste0(column, "_dr")] <- intensity_values
  return(spectrogram_object)
}

pre_emphasize <- function(spectrogram_object, column = "Level", starting_freq = 50){
  
  # calculate the number of octaves away 
  # from the starting frequency for pre-emphasis
  spectrogram_object$octaves_from_ref <- 
    log2(spectrogram_object$Frequency / starting_freq)
  
  # for each octave, add 6 dB. 
  # use the column declared in the input arguments
  spectrogram_object[paste0(column, "_preemp")] <- 
    spectrogram_object[column] + (6 * spectrogram_object$octaves_from_ref)
  
  return(spectrogram_object)
}
