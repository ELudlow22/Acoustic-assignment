
library(ggplot2)
library(behaviouR)
library(tuneR)
library(seewave)
library(warbleR)
library(dplyr)
library(vegan)
library(stringr)
source("nes8010.R")





tawny_male <- query_xc(qword = 'Strix aluco type:male len:5-25', download = FALSE)
tawny_female <- query_xc(qword = 'Strix aluco type:female len:5-25', download = FALSE)

map_xc(tawny_male, leaflet.map = TRUE)

# # Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("tawny_male"))
dir.create(file.path("tawny_female"))
#
# # Download the .MP3 files into two separate sub-folders
query_xc(X = tawny_male, path="tawny_male")
query_xc(X = tawny_female, path="tawny_female")



#
 old_files <- list.files("tawny_male", full.names=TRUE)
 new_files <- NULL
 for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name <- str_c(c(curr_file[[1]][1:2], "-male_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
 }
 file.rename(old_files, new_files)

 old_files <- list.files("tawny_female", full.names=TRUE)
 new_files <- NULL
 for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name <- str_c(c(curr_file[[1]][1:2], "-female_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
 }
 file.rename(old_files, new_files)

# Put blackbird files into appropriate folders ----
dir.create(file.path("tawny_audio"))
 file.copy(from=paste0("tawny_male/",list.files("tawny_male")),
           to="tawny_audio")
 file.copy(from=paste0("tawny_female/",list.files("tawny_female")),
           to="tawny_audio")

# Convert to .WAV format and delete old mp3
 mp32wav(path="tawny_audio", dest.path="tawny_audio")
 unwanted_mp3 <- dir(path="tawny_audio", pattern="*.mp3")
 file.remove(paste0("tawny_audio/", unwanted_mp3))




tawny_male_wav <- readWave("tawny_audio/Strixaluco-male_506715.wav")
tawny_male_wav

oscillo(tawny_male_wav)

#Change this at a later point to make sure it lines up with female
oscillo(tawny_male_wav, from = 0.59, to = 0.60)


SpectrogramSingle(sound.file = "tawny_audio/Strixaluco-male_506715.wav",
                  Colors = "Colors")






tawny_female_wav <- readWave("tawny_audio2/Strixaluco-female_343923.wav")
tawny_female_wav

oscillo(tawny_female_wav)

oscillo(tawny_female_wav, from = 0.59, to = 0.60)


SpectrogramSingle(sound.file = "tawny_audio/Strixaluco-female_343923.wav",
                  Colors = "Colors")




# Feature extraction for blackbirds via MFCC and PCA ####
tawny_mfcc <- MFCCFunction(input.dir = "tawny_audio",
                               max.freq=7000)
dim(tawny_mfcc)
tawny_pca <- ordi_pca(tawny_mfcc[, -1], scale=TRUE)
summary(tawny_pca)$cont[[1]][1:3,1:4]

# Plot PCA scores
tawny_sco <- ordi_scores(tawny_pca, display="sites")
tawny_sco <- mutate(tawny_sco, group_code = tawny_mfcc$Class)

ggplot(tawny_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 


