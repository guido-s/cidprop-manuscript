##############################################
# USE CASE 2: health-related quality of life #
##############################################

library(readxl)
library(dplyr)
library(tibble)

# Read dataset on exercise HF
#
dat2 <- read_excel("Exercise_HF_MLWHF.xlsx") %>%
  arrange(Year) # order according to year of publication


library(meta)
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


#
#
# Mathur & VanderWeele (2020), Epidemiology
#
#

library(MetaUtility)

d2 <- as.data.frame(m2) %>%
  mutate(varTE = seTE^2)

# Recommended calibrated method
#
set.seed(1919)
# Lower decision threshold
ps1_low <-
  prop_stronger(q = -5,
                tail = "below",
                estimate.method = "calibrated",
                ci.method = "calibrated",
                dat = d2,
                yi.name = "TE",
                vi.name = "varTE",
                R = 1000)
# Upper decision threshold
ps1_upp <-
  prop_stronger(q = 5,
                tail = "above",
                estimate.method = "calibrated",
                ci.method = "calibrated",
                dat = d2,
                yi.name = "TE",
                vi.name = "varTE",
                R = 1000)
#
bind_rows(ps1_low, ps1_upp) %>%
  mutate(Threshold = c("\u2264 -5", "\u2265 5"),
         grps = paste(c("Beneficial", "Harmful"), "effect"),
         Percent = paste0(round(100 * est, 1), "%")) %>%
  relocate(Threshold, .before = Percent) %>%
  column_to_rownames(var = "grps") %>%
  select(Threshold, Percent)


# Parametric approach (based on standard normal distribution)
#
# Lower decision threshold
ps2_low <-
  prop_stronger(q = -5,
                tail = "below",
                estimate.method = "parametric",
                ci.method = "parametric",
                M = m2$TE.random,
                se.M = m2$seTE.random,
                t2 = m2$tau2,
                bootstrap = "never")
# Upper decision threshold
ps2_upp <-
  prop_stronger(q = 5,
                tail = "above",
                estimate.method = "parametric",
                ci.method = "parametric",
                M = m2$TE.random,
                se.M = m2$seTE.random,
                t2 = m2$tau2,
                bootstrap = "never")
#
bind_rows(ps2_low, ps2_upp) %>%
  mutate(Threshold = c("\u2264 -5", "\u2265 5"),
         grps = paste(c("Beneficial", "Harmful"), "effect"),
         Percent = paste0(round(100 * est, 1), "%")) %>%
  relocate(Threshold, .before = Percent) %>%
  column_to_rownames(var = "grps") %>%
  select(Threshold, Percent)
