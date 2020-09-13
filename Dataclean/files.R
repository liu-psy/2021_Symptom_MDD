# This script is used for extract rs-fRMI data
setwd("E:/WorkingSpace/Project/2021_Symptom_MDD/Dataclean")
data <- read.csv("E:/WorkingSpace/Project/2021_Symptom_MDD/Dataclean/data.csv")

files <- paste0(
  "H:/GBB/GBB_s", data$Number, "/", "GBB_s", data$Number, "_4DVolume.nii"
  )
file_to <- paste0("F:/GBB_MDD/", "GBB_s", data$Number, "_4DVolume.nii")
all_files <- list.files(list.files("H:/GBB", full.names = T), full.names = TRUE)

files %in% all_files
files[!files %in% all_files]
file.copy(files, file_to, overwrite = TRUE)