# This script is used for combining all inventories.
# Number of participants, Gender, Age are included.
# Inventories: BAI, BDI, RRS, ISI, STAI, Loneliness, ERQ, ATQ

# Increase the heap size to 1g. A very important step. 
# Make sure your computer (laptop) has at least 1g of extra memory.
options(java.parameters = "-Xmx1g")
library(xlsx)
library(dplyr)
library(stringr)

# working directory ------------------------------------------------------------
setwd("E:/WorkingSpace/Project/2021_Symptom_MDD/Rawdata")

integration <- function(Inventory, irrelevantvars, namepostions, items) {
  # exclude irrelevant variables
  ifelse(irrelevantvars == 0, modify <- Inventory, modify <- Inventory[, -irrelevantvars])
  # change name of variables
  names(modify)[namepostions] <- paste0(substitute(Inventory), "_", items)
  # exclude duplicated participants by number
  cat(substitute(Inventory),"- Duplicated partcipants:", 
    sum(duplicated(modify$Number)), "\n")
  modify <- modify[!duplicated(modify$Number), ]
  return(modify)
}

# Automatic Thoughts Questionnaire
ATQ <- read.xlsx2("Data.xlsx", sheetIndex = 1, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 6), rep("numeric", 31)))
ATQ <- integration(ATQ, c(1:2, 5:6, 37), 3:32, 1:30) 

# Beck Anxiety Inventory
BAI <- read.xlsx2("Data.xlsx", sheetIndex = 2, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 2), rep("numeric", 22)))
BAI <- integration(BAI, 24, 3:23, 1:21)

# Beck Depression Inventory
BDI <- read.xlsx2("Data.xlsx", sheetIndex = 3, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 9), "numeric", rep("character", 2), rep("numeric", 22)))
BDI <- integration(BDI, c(1:6, 11:12, 34), 5:25, 1:21)

# Emotion Regulation Questionnaire
# ERQ <- read.xlsx2("Data.xlsx", sheetIndex = 4, stringsAsFactors = FALSE,
#   colClass = c(rep("character", 4), rep("numeric", 13)))
# ERQ <- integration(ERQ, c(3:4, 15:17), 3:12, 1:10)

# Insomnia Severity Index
ISI <- read.xlsx2("Data.xlsx", sheetIndex = 5, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 8), rep("numeric", 8)))
ISI <- integration(ISI, c(1:2, 6:8, 16), 4:10, 1:7)

# Loneliness
Loneliness <- read.xlsx2("Data.xlsx", sheetIndex = 6, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 14), rep("numeric", 6)))
Loneliness <- integration(Loneliness, c(1:6, 10:14, 20), 4:8, 1:5)

# Ruminative Responses Scale, only dimensions
RRS <- read.xlsx2("Data.xlsx", sheetIndex = 7, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 2), rep("numeric", 4)))
RRS <- integration(RRS, 5:6, 3:4, 1:2)
names(RRS)[3:4] <- c("RRS_Reflection", "RRS_Brooding")

# State-Trait Anxiety Inventory
STAI <- read.xlsx2("Data.xlsx", sheetIndex = 8, stringsAsFactors = FALSE,
  colClasses = c(rep("character", 2), rep("numeric", 21)))
STAI <- integration(STAI, 23, 3:22, 21:40)

# Combine all inventories ------------------------------------------------------
data <- ATQ %>% inner_join(BAI, by = "Number") %>% inner_join(BDI, by = "Number") %>% 
  inner_join(ISI, by = "Number") %>% inner_join(Loneliness, by = "Number") %>% 
  inner_join(RRS, by = "Number") %>% inner_join(STAI, by = "Number")
  
# check unique name and gender
extra_information <- data[, str_detect(names(data), "^Name|^Gender|Number")]
extra_information[sapply(apply(extra_information, 1, unique), length) > 3, ]

# For privacy safety, name would delete. And 15361 should be excluded by eye inspection
data <- subset(data, Number != 15361, select = -c(Gender.y, Gender)) 
data <- data[, !str_detect(names(data), "^Name")] 

cat("\n452 partcipants were included for further analysis\n")
# Modify variables -------------------------------------------------------------
data <- data %>% select(Gender.x, Number, Age, everything()) %>% 
  rename(Gender = Gender.x)
data$Gender <- factor(data$Gender, levels = c("女", "男"), labels = 0:1)

write.csv(data, "RawData.csv", row.names = FALSE)