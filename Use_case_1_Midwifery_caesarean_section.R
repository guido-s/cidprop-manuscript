#################################
# USE CASE 1: caesarean section #
#################################

library(readxl)
library(dplyr)
library(tibble)

# Read dataset on caesarean sections
#
dat1 <- read_excel("Midwifery_caesarean_section.xlsx") %>%
  arrange(Year) # order according to year of publication


library(meta)
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


#
#
# Mathur & VanderWeele (2020), Epidemiology
#
#

library(MetaUtility)

d1 <- as.data.frame(m1) %>%
  mutate(varTE = seTE^2)

# Recommended calibrated method
#
set.seed(1910)
# Lower decision threshold
ps1_low <-
  prop_stronger(q = log(0.8),
                tail = "below",
                estimate.method = "calibrated",
                ci.method = "calibrated",
                dat = d1,
                yi.name = "TE",
                vi.name = "varTE",
                R = 1000)
# Upper decision threshold
ps1_upp <-
  prop_stronger(q = log(1.25),
                tail = "above",
                estimate.method = "calibrated",
                ci.method = "calibrated",
                dat = d1,
                yi.name = "TE",
                vi.name = "varTE",
                R = 1000)
#
bind_rows(ps1_low, ps1_upp) %>%
  mutate(Threshold = c("\u2264 0.8", "\u2265 1.25"),
         grps = paste(c("Beneficial", "Harmful"), "effect"),
         Percent = paste0(round(100 * est, 1), "%")) %>%
  relocate(Threshold, .before = Percent) %>%
  column_to_rownames(var = "grps") %>%
  select(Threshold, Percent)


# Parametric approach (based on standard normal distribution)
#
# Lower decision threshold
ps2_low <-
  prop_stronger(q = log(0.8),
                tail = "below",
                estimate.method = "parametric",
                ci.method = "parametric",
                M = m1$TE.random,
                se.M = m1$seTE.random,
                t2 = m1$tau2,
                bootstrap = "never")
# Upper decision threshold
ps2_upp <-
  prop_stronger(q = log(1.25),
                tail = "above",
                estimate.method = "parametric",
                ci.method = "parametric",
                M = m1$TE.random,
                se.M = m1$seTE.random,
                t2 = m1$tau2,
                bootstrap = "never")
#
bind_rows(ps2_low, ps2_upp) %>%
  mutate(Threshold = c("\u2264 0.8", "\u2265 1.25"),
         grps = paste(c("Beneficial", "Harmful"), "effect"),
         Percent = paste0(round(100 * est, 1), "%")) %>%
  relocate(Threshold, .before = Percent) %>%
  column_to_rownames(var = "grps") %>%
  select(Threshold, Percent)
