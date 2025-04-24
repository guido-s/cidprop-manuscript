##############################################
# USE CASE 2: health-related quality of life #
##############################################

library("readxl")
library("dplyr")

# Read dataset on exercise HF
#
dat2 <- read_excel("Exercise_HF_MLWHF.xlsx") %>%
  arrange(Year) # order according to year of publication


# Install development version of R package meta from GitHub (with vignette):
#remotes::install_github("guido-s/meta", build_vignettes = TRUE)
# Install development version of R package meta from GitHub (without vignette):
#remotes::install_github("guido-s/meta")
library("meta")
settings.meta(digits = 2, digits.cid = 0)

# Conduct meta-analysis
#
m2 <- metacont(n.e = Total_E, mean.e = Mean_E, sd.e = SD_E,
  n.c = Total_C, mean.c = Mean_C, sd.c = SD_C,
  studlab = Study,
  # 
  common = FALSE,
  #
  method.tau = "DL",
  #
  label.e = "Exercise", label.c = "Control",
  label.left = "Favours exercise",
  label.right = "Favours control",
  #
  data = dat2)
#
m2

# Calculate expected proportions for minimal clinically important difference
#
cid2 <- cidprop(m2, cid = 5, small.values = "desirable")
cid2

# Save forest plot as PDF-file
#
forest(m2, xlim = c(-30, 20),
  #
  prediction = TRUE, col.predict = "blue",
  smlab = "Mean difference (MLWHF)",
  #
  cid = 5,
  col.cid = "transparent",
  fill.cid.below = "lightgreen",
  fill.cid.above = "lightpink1",
  #
  digits = 2, digits.sd = 2, digits.mean = 2, digits.tau2 = 1,
  #
  col.square = "grey60",
  col.square.lines = "grey60",
  #
  colgap.left = "0.2inch",
  #
  file = "2_Exercise_HF_MLWHF_forest_plot.pdf", width = 10.5,
  rows.gr = 1)

# Prediction plot with legend
#
# Note, R packages 'ggpubr' and 'gridExtra' must be installed to use
# argument 'legend = TRUE'
#
cairo_pdf("2_Exercise_HF_MLWHF_prediction_plot.pdf", width = 10.5)
#
plot(cid2, legend = TRUE, col.predict = "blue",
  xlim = c(-30, 20),
  labels.x = c(-30, -20, -10, 0, 10, 20, 30))
#
dev.off()
