library(openMSE)
library(dplyr)
library(ggplot2)

# ---- Load functions ----
fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))

# ---- Load Base Case MOM ----
# created in 1_Generate_MOM.R
RS_GG_MOM <- readRDS('OMs/BaseCaseMOM.rda')


# ---- Simulate Historical Fishery ----

RS_GG_hist <- SimulateMOM(RS_GG_MOM)


# ---- Plot Historical Simulated Fishery ----

# total biomass
plot_B(RS_GG_hist)
plot_B(RS_GG_hist, 'mt')
plot_B(RS_GG_hist, type='rel')

# spawning biomas
plot_SB(RS_GG_hist)
plot_SB(RS_GG_hist, type='rel')

# landings & discards
plot_C(RS_GG_hist)
plot_C(RS_GG_hist, 'mt')
plot_C(RS_GG_hist, type='byfleet')

