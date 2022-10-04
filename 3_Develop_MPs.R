library(openMSE)
library(dplyr)
library(ggplot2)

# ---- Load functions ----
fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))

# ---- Load Base Case Hist ----
# created in 2_Simulate_Historical.R
RS_GG_hist <- readRDS('Hist_Objects/RS_GG_hist.rda')


# ---- Develop Simple Example Management Procedures -----

# Make stock-fleet specific data list from historical simulations
# for testing purposes
info <- get_info(RS_GG_hist)
DataList <- vector('list', info$n.stocks)
for (p in 1:info$n.stocks) {
  DataList[[p]] <- vector('list', info$n.fleets)
  for (f in 1:info$n.fleets) {
    DataList[[p]][[f]] <- RS_GG_hist[[p]][[f]]@Data
  }
}


# A super simple example MMP that holds each fleet at current effort
current_effort <- function(x, DataList, ...) {
  # First level is stocks
  nStocks <- length(DataList)
  # Second level is fleets (same dimensions among stocks)
  nFleets <- length(DataList[[1]])

  # The hierarchical list we are going to put recommendations in
  RecList <- new('list')

  for(ss in 1:nStocks){
    # List of recommendations by fleet within stock
    RecList[[ss]] <- new('list')

    for(ff in 1:nFleets){
      # New blank recommendations object
      Rec <- new("Rec")
      # Effort held constant at the same level as the last historical year
      Rec@Effort <- 1
      # Store recommendations object in RecList
      RecList[[ss]][[ff]] <- Rec
    }
  }
  # Return the RecList
  RecList
}
# Assign our new function the correct class
class(current_effort) <- 'MMP'

# Test out the new MP:
current_effort(1, DataList)
# returns a hierarchical list with Effort=1 for each Stock and Fleet


current_catch <- function(x, DataList, ...) {
  nStocks <- length(DataList)
  nFleets <- length(DataList[[1]])
  RecList <- new('list')
  for(ss in 1:nStocks){
    RecList[[ss]] <- new('list')
    for(ff in 1:nFleets){
      Rec <- new("Rec")
      Rec@TAC <- DataList[[ss]][[ff]]@MPrec[x]
      RecList[[ss]][[ff]] <- Rec
    }
  }
  RecList
}
# Assign our new function the correct class
class(current_catch) <- 'MMP'


half_effort <- function(x, DataList, ...) {
  # First level is stocks
  nStocks <- length(DataList)
  # Second level is fleets (same dimensions among stocks)
  nFleets <- length(DataList[[1]])

  # The hierarchical list we are going to put recommendations in
  RecList <- new('list')

  for(ss in 1:nStocks){
    # List of recommendations by fleet within stock
    RecList[[ss]] <- new('list')

    for(ff in 1:nFleets){
      # New blank recommendations object
      Rec <- new("Rec")
      # Effort held constant at the same level as the last historical year
      Rec@Effort <- 0.5
      # Store recommendations object in RecList
      RecList[[ss]][[ff]] <- Rec
    }
  }
  # Return the RecList
  RecList
}
# Assign our new function the correct class
class(half_effort) <- 'MMP'


third_effort <- function(x, DataList, ...) {
  # First level is stocks
  nStocks <- length(DataList)
  # Second level is fleets (same dimensions among stocks)
  nFleets <- length(DataList[[1]])

  # The hierarchical list we are going to put recommendations in
  RecList <- new('list')

  for(ss in 1:nStocks){
    # List of recommendations by fleet within stock
    RecList[[ss]] <- new('list')

    for(ff in 1:nFleets){
      # New blank recommendations object
      Rec <- new("Rec")
      # Effort held constant at the same level as the last historical year
      Rec@Effort <- 0.333
      # Store recommendations object in RecList
      RecList[[ss]][[ff]] <- Rec
    }
  }
  # Return the RecList
  RecList
}
# Assign our new function the correct class
class(third_effort) <- 'MMP'

MMSE <- ProjectMOM(RS_GG_hist, MPs=c('current_effort',
                                     'half_effort',
                                     'third_effort',
                                     'current_catch'), dropHist = FALSE)

MMSE@multiHist <- RS_GG_hist

plot_B_proj(MMSE, mps=1, sims=1, incyears=2019)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_1.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1, incyears=2020:2025)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_2.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1, incyears=2020:2030)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_3.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1, incyears=2020:2035)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_4.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1, incyears=2020:2040)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_5.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1:2)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_6.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1:3)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_7.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1:4)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_8.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1, sims=1:5)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_9.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=1)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_10.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=2)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_11.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=3)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_12.png', width=6, height=2.5)

plot_B_proj(MMSE, mps=4)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/proj_13.png', width=6, height=2.5)


# add reference points

plot_B_proj(MMSE, mps=1, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/biomas_ref_1.png', width=4, height=2)

plot_B_proj(MMSE, mps=2, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/biomas_ref_2.png', width=4, height=2)

plot_B_proj(MMSE, mps=3, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/biomas_ref_3.png', width=4, height=2)

plot_B_proj(MMSE, mps=4, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/biomas_ref_4.png', width=4, height=2)

# plot catches
plot_C_proj(MMSE, mps=1, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/catch_1.png', width=4, height=2)
plot_C_proj(MMSE, mps=2, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/catch_2.png', width=4, height=2)

plot_C_proj(MMSE, mps=3, incRef = TRUE)
ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/catch_3.png', width=4, height=2)

plot_C_proj(MMSE, mps=4, incRef = TRUE)

ggsave('img/2022_Oct_Snapper_Grouper_Advisory_Panel/biomas_ref_1.png', width=4, height=2)



# trade-off plots


plot_B_proj(MMSE, mps=1:2, incyears=2020:2025)


plot_B_proj(MMSE, mps=1, maxyr=2030)
plot_B_proj(MMSE, mps=1, maxyr=2035)






# total biomass
plot_B(RS_GG_hist)
plot_B_proj(MMSE)


plot_B(RS_GG_hist, 'mt')
plot_B_proj(MMSE, 'mt')

plot_B(RS_GG_hist, type='rel')
plot_B_proj(MMSE, type='rel')

# spawning biomass
plot_SB(RS_GG_hist)

plot_SB_proj(MMSE)
ggsave('docs/Presentations/img/SB_proj.png', width=10, height=6)

plot_SB(RS_GG_hist, type='rel')
plot_SB_proj(MMSE, type='rel')

# landings & discards
plot_C(RS_GG_hist)
plot_C(RS_GG_hist, 'mt')
plot_C(RS_GG_hist, type='byfleet', 'mt')

plot_C_proj(MMSE)
ggsave('docs/Presentations/img/Landings_Disc_proj.png', width=10, height=6)

plot_C_proj(MMSE, type='byfleet')
ggsave('docs/Presentations/img/Landings_Disc_fleet_proj.png', width=10, height=6)


plot_C_proj(MMSE, incHist=TRUE)
ggsave('docs/Presentations/img/Landings_Disc_overall_proj.png', width=10, height=6)

plot(MMSE@multiHist$`Red Snapper`$cHL@SampPars$Stock$Perr_y[1,], type='l')
MMSE@multiHist$Gag$cHL@SampPars$Stock$Perr_y[1,]









# By Stock and Fleet:
#   1. current_effort
#   2. current_catch
#   3. current_effort & change discard mortality
#   4. fleet-specific TACs

# ---- Develop Slightly More Complex Example Management Procedures -----
