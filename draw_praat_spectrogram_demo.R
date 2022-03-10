library(ggplot2)
library(dplyr)

rm(list = ls())
# read the Praat spectrogram functions
setwd("L:\\Tools\\R\\Read_praat_spectrograms")
source("praat_spectrogram_functions.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# here's where to find the files
spectrogram_file_path <- "C:\\Users\\Matt\\Documents\\R\\Read_praat_spectrograms"
setwd(spectrogram_file_path)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
my_spect_file <- "lake.Spectrogram"

df_spectrogram <- my_spect_file %>%
  convert_spectrogram_to_df() %>%
  pre_emphasize() %>% # creates Column `Level_preemp`
  constrain_dynamic_range(column = "Level_preemp",
                          dynamic_range = 120)

summary(df_spectrogram$Level)
summary(df_spectrogram$Level_preemp)
summary(df_spectrogram$Level_preemp_dr)






names(df_spectrogram)

px_spectrogram <- ggplot(df_spectrogram)+
  aes(x = Time, y=Frequency, fill=Level_preemp_dr)+
  geom_tile()+
  scale_fill_gradient(high="black", low="white", na.value = "white")+
  coord_cartesian(ylim=c(0, 5000), 
                  xlim = c(0, 0.56),
                  expand = FALSE)+
  xlab("Time (s)")+
  scale_y_continuous("Frequency (Hz)", breaks=seq(0, 5000, 1000))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_spectrogram

#





#======================================#
# Plot each segment in a different color
onset_l <- 0.0324
onset_ei <- 0.1254
onset_k <- 0.306

df_spectrogram$segment <- 
  ifelse(df_spectrogram$Time > onset_k, "k",
         ifelse(df_spectrogram$Time > onset_ei, "eI",
                ifelse(df_spectrogram$Time > onset_l, "l", NA)))

df_spectrogram$segment <- 
  factor(df_spectrogram$segment, 
         levels = c("l","eI","k"))


segment_colors <- c("black",
                    "#8C3B3B",
                    "#1D2F50")

px_spectrogram_segments <- 
  df_spectrogram %>%
  dplyr::filter(!is.na(Level_preemp_dr)) %>%
  ggplot()+
  aes(x = Time, y=Frequency, fill=segment,
      alpha = Level_preemp_dr)+
  geom_tile()+
  scale_fill_manual(values = segment_colors)+
  # scale_alpha_continuous()
  coord_cartesian(ylim=c(0, 5000), 
                  xlim = c(0, 0.56),
                  expand = FALSE)+
  xlab("Time (s)")+
  scale_y_continuous("Frequency (Hz)", breaks=seq(0, 5000, 1000))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_spectrogram_segments

#==========================================#
# Now with log-scaled Y axis
octave_breaks <- c(125, 250, 500, 1000, 2000, 4000)

px_spectrogram_log_y <- ggplot(df_spectrogram)+
  aes(xmin = Time, xmax = Time_next,
      ymin=Frequency, ymax = Frequency_next,
      fill=Level_preemp_dr)+
  geom_rect()+
  scale_fill_gradient(high="black", low="white", na.value = "white")+
  coord_cartesian(ylim=c(50, 8000), expand = FALSE)+
  xlab("Time (s)")+
  scale_y_log10("Frequency (Hz)", 
                breaks=octave_breaks)+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_spectrogram_log_y

#=============================================#
# Plot narrowband style
my_spect_file_nb <- "lake_narrowband.Spectrogram"

df_spectrogram_nb <- my_spect_file_nb %>%
  convert_spectrogram_to_df() %>%
  pre_emphasize() %>%
  constrain_dynamic_range(column = "Level_preemp",
                          upper_limit = NULL, 
                          dynamic_range = 95)

px_spectrogram_log_y_nb <- ggplot(df_spectrogram_nb)+
  aes(xmin = Time, xmax = Time_next,
      ymin=Frequency, ymax = Frequency_next,
      fill=Level_preemp_dr)+
  geom_rect()+
  scale_fill_gradient(high="black", low="white", na.value = "white")+
  coord_cartesian(ylim=c(50, 8000), expand = FALSE)+
  xlab("Time (s)")+
  scale_y_log10("Frequency (Hz)", 
                breaks=octave_breaks)+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_spectrogram_log_y_nb






# Save some images
ggsave(px_spectrogram,
       file = "px_spectrogram.png",
       height = 3.1, width = 5.5,
       dpi = 300)

ggsave(px_spectrogram_segments,
       file = "px_spectrogram_segments.png",
       height = 3.1, width = 5.5,
       dpi = 300)

ggsave(px_spectrogram_log_y,
       file = "px_spectrogram_log_y.png",
       height = 3.1, width = 5.5,
       dpi = 300)

ggsave(px_spectrogram_log_y_nb,
       file = "px_spectrogram_log_y_nb.png",
       height = 3.1, width = 5.5,
       dpi = 300)


ggsave(px_spectrogram,
       file = "px_spectrogram.pdf",
       height = 3.1, width = 5.5,
       device = cairo_pdf)








#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# plot "Mongoose"

df_mongoose <- "mongoose_12.Spectrogram" %>%
  convert_spectrogram_to_df() %>%
  pre_emphasize() %>%
  constrain_dynamic_range(column = "Level_preemp", 
                          dynamic_range = 60)

#-----------------------------------------#
# Plot each segment in a different color
onset_m <- 0.0517
onset_a <- 0.13
onset_ng <- 0.2535
onset_g <- 0.3575
onset_u <- 0.4016
onset_s <- 0.579
endpoint <- 0.883

segment_times <-
  c(
    onset_m, onset_a, onset_ng, 
    onset_g, onset_u, onset_s,
    endpoint)

#-----------------------------------------#
# Key in the index of the segment
# at each timepoint
df_mongoose$segment <- 
  findInterval(df_mongoose$Time, segment_times)


px_mongoose_segments <- 
  df_mongoose %>%
  dplyr::filter(!is.na(Level_preemp_dr)) %>%
  dplyr::filter(segment >= 1) %>%
  ggplot()+
  aes(x = Time, y=Frequency, fill=as.factor(segment),
      alpha = Level_preemp_dr)+
  geom_tile()+
  coord_cartesian(ylim=c(0, 6000), 
                  xlim = c(0, 0.89),
                  expand = FALSE)+
  xlab("Time (s)")+
  scale_y_continuous("Frequency (Hz)", 
                     breaks=seq(0, 6000, 1000))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_mongoose_segments


# Use a Brewer palette
px_mongoose_segments_d2 <- 
  px_mongoose_segments +
  scale_fill_brewer(palette = "Dark2")
px_mongoose_segments_d2


custom_mongoose_colors <- 
  c("#e41a1c",
    "#377eb8",
    "#4daf4a",
    "#984ea3",
    "#ff7f00",
    "#2E2E2E")

px_mongoose_segments_custom <- 
  px_mongoose_segments +
  scale_fill_manual(values = custom_mongoose_colors)
px_mongoose_segments_custom

ggsave(px_mongoose_segments_custom,
       file = "px_mongoose_segments.png",
       height = 3, width = 6, dpi = 300)






#-----------------------------------#
px_mongoose_segments_bw <- 
  df_mongoose %>%
  dplyr::filter(!is.na(Level_preemp_dr)) %>%
  dplyr::filter(segment >= 1) %>%
  ggplot()+
  aes(x = Time, y=Frequency,
      alpha = Level_preemp_dr)+
  geom_tile(fill = "#2E2E2E")+
  coord_cartesian(ylim=c(0, 6000), 
                  xlim = c(0, 0.89),
                  expand = FALSE)+
  xlab("Time (s)")+
  scale_y_continuous("Frequency (Hz)", 
                     breaks=seq(0, 6000, 1000))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
px_mongoose_segments_bw

ggsave(px_mongoose_segments_bw,
       file = "px_mongoose_bw.png",
       height = 3, width = 6, dpi = 300)
