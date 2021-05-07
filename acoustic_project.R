
library(ggplot2)
library(behaviouR)
library(tuneR)
library(seewave)
library(warbleR)
library(dplyr)
library(vegan)
library(stringr)
source("nes8010.R")



# Specify the type of data you wish to obtain. Here, tawny owl (Strix aluco) song call data is being obtained for both male and female, with length of audio clip being 5-25 seconds
tawny_male <- query_xc(qword = 'Strix aluco type:male len:5-25', download = FALSE)
tawny_fem  <- query_xc(qword = 'Strix aluco type:female len:5-25', download = FALSE)

# Filter out any records that have the word 'female' in as this word encompasses 'male' therefore would lead to a duplication of results
tawny_male1 <- tawny_male %>% 
   filter(!(grepl("female", Vocalization_type)))

# View where the data for the records have been obtained and who by
map_xc(tawny_male1, leaflet.map = TRUE)

# Create subfolders in your RStudio Project for male and female song calls
dir.create(file.path("tawny_male1"))
dir.create(file.path("tawny_fem"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = tawny_male1, path="tawny_male1")
query_xc(X = tawny_fem, path="tawny_fem")


old_files <- list.files("tawny_male1", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-male1_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

old_files <- list.files("tawny_fem", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-fem_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# Put the tawny owl audio clips into appropriate folders
dir.create(file.path("tawny_audio"))
file.copy(from=paste0("tawny_male1/",list.files("tawny_male1")),
          to="tawny_audio")
file.copy(from=paste0("tawny_fem/",list.files("tawny_fem")),
          to="tawny_audio")

# Convert to .WAV format and delete old mp3
mp32wav(path="tawny_audio", dest.path="tawny_audio")
unwanted_mp3 <- dir(path="tawny_audio", pattern="*.mp3")
file.remove(paste0("tawny_audio/", unwanted_mp3))

#After listening to the audio clips, read in one that is suitable, displaying the call loudly and clearly
tawny_male1_wav <- readWave("tawny_audio/Strixaluco-male1_506715.wav")
tawny_male1_wav

# Use the oscillo function to display an oscillogram for the above audio clip
oscillo(tawny_male1_wav)

# Change this at a later point to make sure it lines up with female
oscillo(tawny_male1_wav, from = 0.59, to = 0.60)

# A spectrogram can also be displayed for the same file
SpectrogramSingle(sound.file = "tawny_audio/Strixaluco-male1_506715.wav",
                  Colors = "Colors")

# Complete the same instructions for a female audio clip to show a contrast in the sounds
tawny_fem_wav <- readWave("tawny_audio/Strixaluco-fem_343923.wav")
tawny_fem_wav

oscillo(tawny_fem_wav)

oscillo(tawny_fem_wav, from = 0.59, to = 0.60)

SpectrogramSingle(sound.file = "tawny_audio/Strixaluco-fem_343923.wav",
                  Colors = "Colors")

# Feature extraction for tawny owls via MFCC and PCA
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




# Complete the same indices but for song thrushes but comparing song and alarm calls rather than differences between genders

#Specify the type of data you wish to obtain. Here, song thrush (Turdus philomelos) song call data is being obtained for both song and alarm calls, with length of audio clip being 5-25 seconds
wren_songs       <- query_xc(qword = 'Troglodytes troglodytes cnt:"united kingdom" type:song len:5-25', download = FALSE)
woodpigeon_songs <- query_xc(qword = 'Columba palumbus cnt:"united kingdom" type:song len:5-25', download = FALSE)

# Map for the song thrush data points
map_xc(wren_songs, leaflet.map = TRUE)

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("wren_songs"))
dir.create(file.path("woodpigeon_songs"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = wren_songs, path="wren_songs")
query_xc(X = woodpigeon_songs, path="woodpigeon_songs")

# Rename wren and wood pigeon files
old_files <- list.files("wren_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

old_files <- list.files("woodpigeon_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# Put song thrush files into appropriate folders 
dir.create(file.path("bird_audio"))
file.copy(from=paste0("wren_songs/",list.files("wren_songs")),
          to="bird_audio")
file.copy(from=paste0("woodpigeon_songs/",list.files("woodpigeon_songs")),
          to="bird_audio")

# Convert to .WAV format and delete old mp3
mp32wav(path="bird_audio", dest.path="bird_audio")
unwanted_mp3 <- dir(path="bird_audio", pattern="*.mp3")
file.remove(paste0("bird_audio/", unwanted_mp3))


wren_wav <- readWave("bird_audio/Troglodytestroglodytes-song_447467.wav")
wren_wav

# Oscillogram and spectrogram for song thrush song example
# Oscillogram
oscillo(wren_wav)
oscillo(wren_wav, from = 0.59, to = 0.60)
# Spectrogram for song thrush
SpectrogramSingle(sound.file = "bird_audio/Troglodytestroglodytes-song_447467.wav",
                  Colors = "Colors")


woodpigeon_wav <- readWave("bird_audio/Columbapalumbus-song_235149.wav")
woodpigeon_wav

# Oscillogram and spectrogram for song thrush song example
# Oscillogram
oscillo(woodpigeon_wav)
oscillo(woodpigeon_wav, from = 0.59, to = 0.60)
# Spectrogram for song thrush
SpectrogramSingle(sound.file = "bird_audio/Columbapalumbus-song_235149.wav",
                  Colors = "Colors")

# Feature extraction for song thrushes via MFCC and PCA ####
bird_mfcc <- MFCCFunction(input.dir = "bird_audio",
                                max.freq=7000)
dim(bird_mfcc)
bird_pca <- ordi_pca(bird_mfcc[, -1], scale=TRUE)
summary(bird_pca)$cont[[1]][1:3,1:4]

# Plot PCA scores
bird_sco <- ordi_scores(bird_pca, display="sites")
bird_sco <- mutate(bird_sco, group_code = bird_mfcc$Class)

ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
   geom_point() 



#Specify the type of data you wish to obtain. Here, song thrush (Turdus philomelos) song call data is being obtained for both song and alarm calls, with length of audio clip being 5-25 seconds
robin_songs <- query_xc(qword = 'Erithacus rubecula  type:song len:5-25', download = FALSE)
robin_alarm <- query_xc(qword = 'Erithacus rubecula  type:alarm len:5-25', download = FALSE)

#Map for the song thrush data points
map_xc(robin_songs, leaflet.map = TRUE)

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("robin_songs"))
dir.create(file.path("robin_alarm"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = robin_songs, path="robin_songs")
query_xc(X = robin_alarm, path="robin_alarm")

# Rename robin files 
old_files <- list.files("robin_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

old_files <- list.files("robin_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
   curr_file <- str_split(old_files[file], "-")
   new_name  <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
   new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# Put robin files into appropriate folders 
dir.create(file.path("robin_audio"))
file.copy(from=paste0("robin_songs/",list.files("robin_songs")),
          to="robin_audio")
file.copy(from=paste0("robin_alarm/",list.files("robin_alarm")),
          to="robin_audio")

# Convert to .WAV format and delete old mp3
mp32wav(path="robin_audio", dest.path="robin_audio")
unwanted_mp3 <- dir(path="robin_audio", pattern="*.mp3")
file.remove(paste0("robin_audio/", unwanted_mp3))

robin_wav <- readWave("robin_audio/Turdusphilomelos-song_297816.wav")
robin_wav

# Oscillogram and spectrogram for robin song example
# Oscillogram
oscillo(robin_wav)
oscillo(robin_wav, from = 0.59, to = 0.60)
# Spectrogram for song thrush
SpectrogramSingle(sound.file = "robin_audio/Erithacusrubecula-song_297816.wav",
                  Colors = "Colors")

robin2_wav <- readWave("robin_audio/Erithacus rubecula-alarm_152372.wav")
robin2_wav

# Oscillogram and spectrogram for robin song example
# Oscillogram
oscillo(robin2_wav)
oscillo(robin2_wav, from = 0.59, to = 0.60)
# Spectrogram for robin
SpectrogramSingle(sound.file = "robin_audio/Erithacusrubecula-alarm_152372.wav",
                  Colors = "Colors")

# Feature extraction for robins via MFCC and PCA ####
robin_mfcc <- MFCCFunction(input.dir = "robin_audio",
                           max.freq=7000)
dim(robin_mfcc)
robin_pca <- ordi_pca(robin_mfcc[, -1], scale=TRUE)
summary(robin_pca)$cont[[1]][1:3,1:4]

# Plot PCA scores
robin_sco <- ordi_scores(robin_pca, display="sites")
robin_sco <- mutate(robin_sco, group_code = robin_mfcc$Class)

ggplot(robin_sco, aes(x=PC1, y=PC2, colour=group_code)) +
   geom_point() 





