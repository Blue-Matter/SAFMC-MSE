library(SAMSE)

# ---- Run Historical Simulations ----
# OM <- readRDS('Training/RS.om')
# Hist <- Simulate(OM, silent=TRUE)


# ---- Management Procedures ----
avail('MP') # built-in MPs

?Islope3 # help documentation

# ---- Custom Management Procedures ----

# ---- A Simple MP - Current Effort ----

# `x` - Simulation number
# `Data` - `nsim` versions of simulated fishery data
# `...` - extra arguments (not used)

Rec <- new("Rec") # management recommendations
slotNames(Rec)
# TAC - total allowable catch
# Effort - fishing effort relative to last historical year
# Spatial - open or close areas
#

CurrentEffort <- function(x, Data, ...) {
  Rec <- new("Rec") # create management recommendations object
  Rec@Effort <- 1 # keep effort the same as last historical
  Rec # return `Rec` object
}
class(CurrentEffort) <- 'MP' # assign to class `MP`


MSE2 <- ProjectMOM(Hist, MPs='CurrentEffort')

# Effort for all 3 fleets stays at 1 (same as last historical)
MSE2@Effort[1,1,,,]



# ---- Little more complicated -----
# Status Quo (SQ)
# Set fishing mortality (Effort) for each fleet to geometric mean from last 3 years

# x - simulation number
# DataList - a list of Simulated Data for each Stock (1) and Fleet (3)

SQ <- function(x, DataList, ...) {
  RecList <- Create_Rec_List(DataList)
  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])

  year_info <- Get_Year_Info(DataList)
  yr_ind <- year_info$Last_3_Yrs

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- exp(mean(log(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))
      lastF <- DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind[length(yr_ind)]]

      deltaE <- meanF/lastF
      if (!is.finite(deltaE)) deltaE <- 1E-5
      RecList[[s]][[f]]@Effort <- deltaE
    }
  }
  RecList
}
class(SQ) <- 'MMP' # class `MMP`
# MMP - returns a `Rec` object for each stock and fleet
# option to set individual management controls by stock and fleet

MSE3 <- ProjectMOM(Hist, MPs=c('CurrentEffort', 'SQ'))

effort1 <- MSE3@Effort[1,1,,,1] # effort in first time step
rownames(effort1) <- OM@Fleets[[1]] |> names()
colnames(effort1) <- c('CurrentEffort', 'SQ')

effort1

# ---- Spatial Closure ----

# Close Area 1 and keep effort fixed at current level
Close_Area_1 <- function(x, DataList, ...) {
  RecList <- Create_Rec_List(DataList)
  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      RecList[[s]][[f]]@Spatial <- c(0,1)
      RecList[[s]][[f]]@Allocate <- 1
    }
  }
  RecList
}
class(Close_Area_1) <- 'MMP' # class `MMP`


MPs <- c('CurrentEffort', 'Close_Area_1')
MSE4 <- ProjectMOM(Hist, MPs=MPs)

# Compare Performance
SB_SBMSY <- apply(MSE4@SB_SBMSY, 3:4, mean) # mean SB/SBMSY over simulations
dimnames(SB_SBMSY) <- list(MP=MPs, Year=ProjYears)
SB_SBMSY <- array2DF(SB_SBMSY)
SB_SBMSY$Variable <- 'SB/SBMSY'

Landings <- apply(MSE4@Catch, 3:5, mean) # mean landings over simulations
Landings <- apply(Landings, 2:3, sum) # sum over fleets

dimnames(Landings) <- list(MP=MPs,
                           Year=ProjYears)

Landings <- array2DF(Landings)
Landings$Variable <- 'Landings'

DF <- dplyr::bind_rows(SB_SBMSY, Landings)
DF$Year <- as.numeric(DF$Year)

ggplot(DF, aes(x=Year, y=Value, color=MP)) +
  facet_wrap(~Variable, scale='free') +
  geom_line() +
  theme_classic() +
  expand_limits(y=0)







