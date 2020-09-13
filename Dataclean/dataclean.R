# Inventories: ATQ, BAI, BDI, ERQ, ISI, Loneliness, RSS, STAI
# 1. Imupate missing-value (if necessary)
# 2. Regressed out age and gender
# 3. Sorting out the sub-dimensions of the inventories
library(dplyr)
library(mice)
library(stringr)
library(ggpubr)
library(ggcorrplot)
library(patchwork)
library(ggplot2)

# Set working directory and load data ------------------------------------------
setwd("E:/WorkingSpace/Project/2021_Symptom_MDD/Dataclean")

data <- read.csv("RawData.csv")
data$Gender <- factor(data$Gender, levels = 0:1, labels = c("Female", "Male"))

# Preprocessing ----------------------------------------------------------------
# Remove participants (no R-fMRI data)
data <- data[data$Number != 16189, ]

# Check gender and age
with(data, table(Gender, Age))

range(data$Age, na.rm = TRUE)
cat("Find outliers\n")
data[which(data$Age == 11), ]
data[which(data$Age == 16), ]
data[which(data$Age == 26), ]
# Compared with other data in GBB database, the age of 15385 should be 21
# The other two participants' age are right
data[which(data$Age == 11), ]$Age <- 21

# Check NA; only in age
ceiling(which(is.na(data)) / nrow(data))
cat("Age contains NAs\n")
data[, 3]

# Imputed by random forest 
data <- complete(mice(data, m = 10, method = "rf", seed = 1))

# Regressed out age and gender -------------------------------------------------
# regressed_age_gender <- function(vars) {
#   fit <- lm(vars ~ Age + Gender, data = data)
#   output <- residuals(fit) + coef(fit)[[1]]
#   return(output)
# }
# data[, 4:119] <- apply(data[, 4:119], 2, function(x) regressed_age_gender(x))

# Check and add dimensions of inventories --------------------------------------
# If the correlation coefficient between sub-dimensions in a inventory exceeds 0.8, 
# one of the dimensions will be removed arbitrarily
check_inventories <- function(inventory) {
  vars_list <- names(data)
  result <- data[, str_detect(vars_list, paste0("^", substitute(inventory)))]
  cat(substitute(inventory), "items:", ncol(result), "\t")
  cat(substitute(inventory), "range:", range(result), "\n")
  return(result)
}

# Automatic Thoughts Questionnaire
ATQ <- check_inventories("ATQ")

# Personal Maladjustment and Desire for Change = PmDC
# Negative Self-Concept and Negative Expectations = NscNe
# Low Self-Esteem = LSE
ATQ$ATQ_PmDc <- rowSums(ATQ[, c(7, 10, 14, 20, 26)])
ATQ$ATQ_NscNe <- rowSums(ATQ[, c(2:3, 9, 21, 23:24, 28)])
ATQ$ATQ_LSE <- rowSums(ATQ[, c(17:18)])
ATQ$ATQ_Helplessnes <- rowSums(ATQ[, c(29:30)])

cor(ATQ[, c("ATQ_PmDc", "ATQ_NscNe", "ATQ_LSE", "ATQ_Helplessnes")])

# Beck Anxiety Inventory
BAI <- check_inventories("BAI")
# Fix BAI range
BAI <- BAI - 1

BAI$BAI_Somatic <- rowSums(BAI[, c(1:3, 6:8, 12:13, 17, 19:21)])
BAI$BAI_Cognitive <- rowSums(BAI[, c(4:5, 9:11, 14:16, 18)])
# Check sub-dimensions correlation matrix
cor(BAI[, c("BAI_Somatic", "BAI_Cognitive")])

# Beck Depression Inventory
BDI <- check_inventories("BDI")

BDI$BDI_Cognitive_Affection <- rowSums(BDI[, c(1:3, 5:9, 13:14)])
BDI$BDI_Somatic_Vegetative <- rowSums(BDI[, c(4, 10:12, 15:21)])
cor(BDI[, c("BDI_Cognitive_Affection", "BDI_Somatic_Vegetative")])

# Emotion Regulation Questionnaire;
# ERQ <- check_inventories("ERQ")
# ERQ$ERQ_Cognitive_Reappraisal <- rowSums(ERQ[, c(1, 3, 5, 7:8, 10)])
# ERQ$ERQ_Expressive_Suppression <- rowSums(ERQ[, c(2, 4, 6, 9)])

# cor(ERQ[, c("ERQ_Cognitive_Reappraisal", "ERQ_Expressive_Suppression")])

# Insomnia Severity Index
ISI <- check_inventories("ISI")

# PA1 <- fa.parallel(ISI, fa = "both", sim = FALSE, n.iter = 10000, quant = 0.95,
#   main = "Parallel Analysis")
ISI$ISI_Sum <- rowSums(ISI)

# Loneliness
Loneliness <- check_inventories("Loneliness")
# PA2 <- fa.parallel(Loneliness, fa = "both", sim = FALSE, n.iter = 10000, quant = 0.95,
#   main = "Parallel Analysis")
Loneliness$Loneliness_Sum <- rowSums(Loneliness)

# Ruminative Responses Scale, only dimensions
RRS <- check_inventories("RRS")
cor(RRS)

# State-Trait Anxiety Inventory - Trait version
STAI <- check_inventories("STAI")

STAI$STAI_Anxiety <- rowSums(STAI[, c(21, 23:27, 30, 32:36, 39) - 20])
STAI$STAI_Depression <- rowSums(STAI[, c(22, 28:29, 31, 37:38, 40) - 20])

cor(STAI[, c("STAI_Anxiety", "STAI_Depression")])

# Combine all sub-dimensions
DVs <- data.frame(
  ATQ[, c("ATQ_PmDc", "ATQ_NscNe", "ATQ_LSE", "ATQ_Helplessnes")],
  BAI[, c("BAI_Somatic", "BAI_Cognitive")], 
  BDI[, c("BDI_Cognitive_Affection", "BDI_Somatic_Vegetative")],
# ERQ[, c("ERQ_Cognitive_Reappraisal", "ERQ_Expressive_Suppression")],
  ISI_Sum = ISI$ISI_Sum,
  Loneliness_Sum = Loneliness$Loneliness_Sum,
  RRS,
  STAI[, c("STAI_Anxiety", "STAI_Depression")]
)

# Excluded BDI >= 3
BDI_scaled <- scale(rowSums(BDI[, 1:21]))
data <- data[abs(BDI_scaled) < 3, ]
DVs <- DVs[abs(BDI_scaled) < 3, ]

# Plot correlation matrix of DVs
ggcorrplot(
  cor(DVs), 
  method = "square",
  type = "full", 
  hc.order = TRUE,
  colors = c("#4393C3", "white", "#EF3B2C"), 
  tl.cex = 20,
  legend.title = NULL
  )
ggsave("Figure1.pdf", width = 30, height = 20, units = "cm")

# Plot density map of DVs
plot_density <- function(vars) {
  ggdensity(
    DVs[, vars], 
    add = "mean", 
    size = 2, 
    xlab = "Value", 
    ylab = "Density", 
    title = names(DVs)[vars]
    ) + 
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15, face = "bold"),
      title = element_text(size = 15)
    )
}

(p_list <- lapply(1:ncol(DVs), plot_density))
names(p_list) <- paste0("p", 1:ncol(DVs))
with(p_list, p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + 
    p13 + p14)
ggsave("Figure2.pdf", width = 30, height = 30, units = "cm")

# Output data ------------------------------------------------------------------
write.csv(DVs, "DVs.csv", row.names = FALSE)
write.csv(data, "data.csv", row.names = FALSE)