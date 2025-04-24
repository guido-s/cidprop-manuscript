#################################
# USE CASE 1: caesarean section #
#################################

library("readxl")
library("dplyr")

# Read dataset on caesarean sections
#
dat1 <- read_excel("Midwifery_caesarean_section.xlsx") %>%
  arrange(Year) # order according to year of publication


# Install development version of R package meta from GitHub (with vignette):
#remotes::install_github("guido-s/meta", build_vignettes = TRUE)
# Install development version of R package meta from GitHub (without vignette):
#remotes::install_github("guido-s/meta")
library("meta")
settings.meta(digits = 2, digits.cid = 2)

# Conduct meta-analysis
#
m1 <- metabin(event.e = Events_E, n.e = Total_E,
  event.c = Events_C, n.c = Total_C,
  studlab = Study,
  #
  common = FALSE,
  prediction = TRUE,
  method.tau = "DL",
  #
  label.e = "Midwife continuity models",
  label.c = "Other models of care",
  label.left = "Favours midwife continuity models",
  label.right = "Favours other models of care",
  #
  data = dat1)
#
m1

# Calculate expected proportions for minimal clinically important difference
#
cid1 <- cidprop(m1, cid = 0.8, small.values = "desirable")
cid1

# Save forest plot as PDF-file
#
forest(m1, xlim = c(0.5, 1.8),
  #
  prediction = TRUE, col.predict = "blue",
  smlab = "RR for caesarean birth",
  #
  cid = 0.8,
  col.cid = "transparent",
  fill.cid.below = "lightgreen",
  fill.cid.above = "lightpink1",
  #
  col.square = "grey60",
  col.square.lines = "grey60",
  #
  colgap.left = "0.4inch",
  #
  file = "1_Midwifery_caesarean_section_forest_plot.pdf", width = 10.5,
  rows.gr = 1)

# Prediction plot with legend
#
# Note, R packages 'ggpubr' and 'gridExtra' must be installed to use
# argument 'legend = TRUE'
#
cairo_pdf("1_Midwifery_caesarean_section_prediction_plot.pdf", width = 12)
#
plot(cid1, legend = TRUE, col.predict = "blue",
  xlim = c(0.5, 1.8),
  labels.x = c(0.5, 0.75, 1, 1.5, 1.8))
#
dev.off()
