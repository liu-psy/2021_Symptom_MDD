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

# make a FC file list for PLS
FC_files <- list.files("F:/FC", 
  pattern = "^ROICorrelation_FisherZ_ROISignal_GBB_s....._4DVolume.txt",
  full.names = TRUE)
write.table(FC_files, "files.txt", col.names = FALSE, row.names = FALSE)

# make a behavior list for PLS
write.table(data$Age, "age.txt", col.names = FALSE, row.names = FALSE)
