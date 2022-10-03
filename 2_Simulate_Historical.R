library(openMSE)
library(dplyr)
library(ggplot2)
library(cowplot)

# ---- Load functions ----
fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))

# ---- Load Base Case MOM ----
# created in 1_Generate_MOM.R
RS_GG_MOM <- readRDS('OMs/BaseCaseMOM.rda')



# ---- Simulate Historical Fishery ----

RS_GG_hist <- SimulateMOM(RS_GG_MOM)
saveRDS(RS_GG_hist, 'Hist_Objects/RS_GG_hist.rda')


# ---- Plot Historical Simulated Fishery ----
RS_GG_hist <- readRDS('Hist_Objects/RS_GG_hist.rda')


# total biomass
plot_B(RS_GG_hist, incRef = FALSE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/Biomass_lbs.png', width=8, height=5)

plot_B(RS_GG_hist, incRef = FALSE, incLeg=FALSE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/Biomass_lbs_no_legend.png', width=4, height=2.5)


# landings & discards
plot_C(RS_GG_hist)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/Landings_Disc_lbs.png', width=10, height=5)















lot_B(RS_GG_hist, 'mt')
ggsave('docs/Presentations/img/B_mt.png', width=8, height=5)

plot_B(RS_GG_hist, type='rel')
ggsave('docs/Presentations/img/B_rel.png', width=8, height=5)

# spawning biomass
plot_SB(RS_GG_hist)
ggsave('docs/Presentations/img/SB.png', width=10, height=5)

plot_SB(RS_GG_hist, type='rel')
ggsave('docs/Presentations/img/SB_rel.png', width=10, height=5)



plot_C(RS_GG_hist, 'mt')

plot_C(RS_GG_hist, type='byfleet', 'mt')
ggsave('docs/Presentations/img/Landings_Disc_fleet.png', width=10, height=6)

# multiHist=RS_GG_hist
