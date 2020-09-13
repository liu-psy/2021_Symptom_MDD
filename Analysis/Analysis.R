library(dendextend)
library(dynamicTreeCut)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(gplots)
library(psych)
library(patchwork)
library(reshape2)
library(stringr)

# Set working directory and load data ------------------------------------------
setwd("E:/WorkingSpace/Project/2021_Symptom_MDD/Analysis")
data <- read.csv("E:/WorkingSpace/Project/2021_Symptom_MDD/Dataclean/data.csv")
DVs <- read.csv("E:/WorkingSpace/Project/2021_Symptom_MDD/Dataclean/DVs.csv")

# BDI --------------------------------------------------------------------------
BDI <- data[, str_detect(names(data), "^BDI")]
BDI_table <- data.frame(table(rowSums(BDI)))
BDI_table$Var1 <- as.numeric(as.character(BDI_table$Var1))

cat("\nBDI total score > 10:", sum(rowSums(BDI) > 10))

ggplot(BDI_table, aes(Var1, Freq, fill = Var1)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) + 
  geom_text(label = BDI_table$Freq, vjust = 00, alpha = 0.5, size = 8) + 
  labs(x = "BDI total score", y = "Counts") +
  scale_y_continuous(breaks = seq(0, 55, 5)) +
  scale_x_continuous(breaks = 0:max(BDI_table$Var1)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25, face = c(rep("plain", 11), rep("bold.italic", 14))),
    axis.text.y = element_text(size = 25),
    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
    panel.background = element_rect(fill = "white")
  )
ggsave("Figure3.pdf", width = 15, height = 10)

# Plot BDI density 
plot_density <- function(vars) {
  ggdensity(
    BDI[, vars], 
    add   = "mean", 
    size  = 2, 
    xlab  = "Value", 
    ylab  = "Density", 
    title = names(BDI)[vars]
  ) + 
    theme(
      axis.text  = element_text(size = 15),
      axis.title = element_text(size = 15, face = "bold"),
      title      = element_text(size = 15)
    )
}
(p_list <- lapply(seq(BDI), plot_density))
names(p_list) <- paste0("p", seq(BDI))
with(p_list, p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + 
    p13 + p14 + p15 + p16 + p17 + p18 + p19 + p20 + p21)
ggsave("Figure4.pdf", width = 15, height = 10)

# Gender test ------------------------------------------------------------------
# BDI
fit <- aov(rowSums(BDI) ~ data$Gender)
summary(fit)
# DVs
multi_aov <- function(vars, x) {
  Data <- x
  fit <- aov(x[[vars]] ~ data$Gender)
  summary(fit)
}
sapply(names(DVs), multi_aov, DVs)
sapply(names(BDI), multi_aov, BDI)

# Factor analysis --------------------------------------------------------------
# Parallel analysis
PA <- fa.parallel(DVs, fa = "fa", sim = FALSE, n.iter = 10000, quant = 0.95)

pa_table <- data.frame(t(rbind(PA$fa.values, PA$fa.simr)))
names(pa_table) <- c("Acutual Data", "Resampled Data")
pa_table <- melt(pa_table)
pa_table$Factors <- rep(seq(DVs), times = 2)

ggplot(pa_table, aes(Factors, value, color = variable, linetype = variable, 
  shape = variable)) +
  geom_line(size = 2) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = seq(DVs)) +
  scale_y_continuous(breaks = seq(0, 7, by = 1)) +
  scale_color_manual(values = c("#B30000", "#2166AC")) + 
  scale_shape_manual(values = c(17, 20)) +
  labs(y = "Eigen value") +
  theme(
    title = element_text(size = 30),
    text = element_text(size = 30),
    legend.position = c(0.8, 0.8),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    axis.text.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
    panel.background = element_rect(fill = "white")
  )
ggsave("Figure5.pdf", width = 15, height = 9)

# EFA with oblimin rotation
EFA <- fa(DVs, nfactors = 4, rotate = "oblimin", fm = "ml", n.iter = 10000,
  scores = "tenBerge")
EFA
cat("Explained variance:", mean(EFA$communality))

# Hierarchical Clustering of DVs based on loading matrix
hc <- hclust(dist(scale(EFA$loadings)))
plot(hc)

# Detection of clusters in hierarchical clustering dendrograms
cutreeDynamicTree(hc, maxTreeHeight = ceiling(max(hc$height)), minModuleSize = 1)

# Plot dendrogram
p1 <- hc %>% as.dendrogram() %>% 
  set("branches_k_color", c("#E41A1C", "#377EB8", "#4DAF4A"), k = 3) %>%
  set("branches_k_lty", rep("twodash", 2), k = 2) %>% 
  set("branches_lwd", 3) %>%
  set("labels", NULL) %>%
  as.ggdend() %>% 
  ggplot()

# Plot the heatmap of loading matrix
loading_table <- melt(as.matrix(unclass(EFA$loadings[hc$order, c(3, 4, 1, 2)])))
p2 <- ggplot(loading_table, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradientn(
    colors = bluered(20), 
    limits = c(-1, 1.1),
    labels = c(-1, 0, 1.1), 
    breaks = c(-1, 0, 1.1)
  ) + 
  theme(
    legend.key.height = unit(5, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 20, angle = 90),
    legend.text = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
  ) +
  guides(
    fill = guide_colorbar(
      title = "Factor loading", 
      title.position = "right",
      title.hjust = 0.5,
      ticks = FALSE
      )
    ) + 
  geom_vline(
    xintercept = c(2, 8, 10) + 0.5, 
    size = 1.2, 
    linetype = 2,
    color = "grey50"
    )

p1 + p2 + plot_layout(nrow = 2, heights = c(1, 1.5))
ggsave("Figure6.pdf", width = 30, height = 20)