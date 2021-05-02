
library(ggplot2)
library(behaviouR)
library(tuneR)
library(seewave)
library(warbleR)
library(dplyr)
library(vegan)
library(stringr)
source("nes8010.R")


songthrush_songs <- query_xc(qword = 'Turdus philomelos  type:song len:5-25', download = FALSE)

songthrush_alarm <- query_xc(qword = 'Turdus philomelos  type:alarm len:5-25', download = FALSE)



map_xc(songthrush_songs, leaflet.map = TRUE)

# # Create subfolders in your RStudio Project for song calls and alarm calls
 dir.create(file.path("songthrush_songs"))
 dir.create(file.path("songthrush_alarm"))
#
# # Download the .MP3 files into two separate sub-folders
 query_xc(X = songthrush_songs, path="songthrush_songs")
 query_xc(X = songthrush_alarm, path="songthrush_alarm")


# Rename blackbird files ----
# library(stringr) # part of tidyverse
# 
 old_files <- list.files("songthrush_songs", full.names=TRUE)
 new_files <- NULL
 for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
 }
 file.rename(old_files, new_files)

old_files <- list.files("songthrush_alarm", full.names=TRUE)
 new_files <- NULL
 for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
 }
 file.rename(old_files, new_files)

# Put blackbird files into appropriate folders ----
dir.create(file.path("songthrush_audio"))
 file.copy(from=paste0("songthrush_songs/",list.files("songthrush_songs")),
           to="songthrush_audio")
 file.copy(from=paste0("songthrush_alarm/",list.files("songthrush_alarm")),
           to="songthrush_audio")

# Convert to .WAV format and delete old mp3
 mp32wav(path="songthrush_audio", dest.path="songthrush_audio")
 unwanted_mp3 <- dir(path="songthrush_audio", pattern="*.mp3")
 file.remove(paste0("songthrush_audio/", unwanted_mp3))


songthrush_wav <- readWave("songthrush_audio/Turdusphilomelos-song_243908.wav")
songthrush_wav

# Oscillogram and spectrogram for blackbird song example ####
# Oscillogram
oscillo(songthrush_wav)
oscillo(songthrush_wav, from = 0.59, to = 0.60)
# Spectrogram for blackbird
SpectrogramSingle(sound.file = "songthrush_audio/Turdusphilomelos-song_243908.wav",
                  Colors = "Colors")

# Feature extraction for blackbirds via MFCC and PCA ####
songthrush_mfcc <- MFCCFunction(input.dir = "songthrush_audio",
                               max.freq=7000)
dim(songthrush_mfcc)
songthrush_pca <- ordi_pca(songthrush_mfcc[, -1], scale=TRUE)
summary(songthrush_pca)$cont[[1]][1:3,1:4]

# Plot PCA scores
songthrush_sco <- ordi_scores(songthrush_pca, display="sites")
songthrush_sco <- mutate(songthrush_sco, group_code = songthrush_mfcc$Class)

ggplot(songthrush_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 








blackcap_songs <- query_xc(qword = 'Sylvia atricapilla  type:song len:5-25', download = FALSE)

blackcap_alarm <- query_xc(qword = 'Sylvia atricapilla  type:alarm len:5-25', download = FALSE)