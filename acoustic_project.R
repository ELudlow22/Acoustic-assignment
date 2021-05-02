library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(stringr)



male_tawny_songs <- query_xc(qword = 'Strix aluco type:male type:song len:5-25', download = FALSE)

female_tawny_songs <- query_xc(qword = 'Strix aluco type:female type:song len:5-25', download = FALSE)





map_xc(male_tawny_songs, leaflet.map = TRUE)
map_xc(female_tawny_songs, leaflet.map = TRUE)


# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("male_tawny_songs"))
dir.create(file.path("female_tawny_songs"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = male_tawny_songs, path="male_tawny_songs")
query_xc(X = female_tawny_songs, path="female_tawny_songs")



old_files <- list.files("male_tawny_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-male_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)


old_files <- list.files("female_tawny_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-female_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)


dir.create(file.path("tawny_audio"))
file.copy(from=paste0("male_tawny_songs/",list.files("male_tawny_songs")),
          to="tawny_audio")
file.copy(from=paste0("female_tawny_songs/",list.files("female_tawny_songs")),
          to="tawny_audio")

mp32wav(path="tawny_audio", dest.path="tawny_audio")
unwanted_mp3 <- dir(path="tawny_audio", pattern="*.mp3")
file.remove(paste0("tawny__audio/", unwanted_mp3))


male_tawny_wav <- readWave("tawny_audio/Strixaluco-male_197132.wav")
male_tawny_wav

oscillo(male_tawny_wav)

oscillo(male_tawny_wav, from = 0.59, to = 0.60)









cuckoo_songs <- query_xc(qword = 'Erithacus rubecula  type:song len:5-25', download = FALSE)

cuckoo_alarm <- query_xc(qword = 'Erithacus rubecula  type:alarm len:5-25', download = FALSE)


songthrush_songs <- query_xc(qword = 'Turdus philomelos  type:song len:5-25', download = FALSE)

songthrush_alarm <- query_xc(qword = 'Turdus philomelos  type:alarm len:5-25', download = FALSE)


blackcap_songs <- query_xc(qword = 'Sylvia atricapilla  type:song len:5-25', download = FALSE)

blackcap_alarm <- query_xc(qword = 'Sylvia atricapilla  type:alarm len:5-25', download = FALSE)

