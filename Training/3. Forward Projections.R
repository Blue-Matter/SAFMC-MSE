library(SAMSE)

# ---- Run Historical Simulations ----
# OM <- readRDS('Training/RS.om')
# Hist <- Simulate(OM, silent=TRUE)


# ---- Run Closed-Loop Simulations ----

# Simple Example Management Options

# curE - all fleets fixed at current effort
# CurC - all fleets fixed at curent catch
# FMSYref - fishing at FMSY exactly
# FMSYref75 - fishing at 0.75FMSY exactly
# NFref - no fishing reference MP

MPs <- c('curE', 'CurC', 'FMSYref', 'FMSYref75', 'NFref')

MSE <- ProjectMOM(Hist, MPs=MPs)


# ---- Explore MSE Results ----

class(MSE) # MMSE S4 object (multi-MSE)
slotNames(MSE)

# Plot the landings by Fleet and MP
Landings <- openMSE::get_Landings(MSE)
Landings$MP[is.na(Landings$MP)] <- 'Historical'

LandingsMedianFleet <- Landings |>
  dplyr::group_by(Year, Fleet, MP) |>
  dplyr::summarise(Landings=median(Value))

ggplot(LandingsMedianFleet, aes(x=Year, y=Landings, color=MP)) +
  facet_wrap(~Fleet) +
  geom_line()

# Plot the landings by MP
LandingsMedian <- Landings |>
  dplyr::group_by(Sim, Year, MP) |>
  dplyr::summarise(Landings=sum(Value)) |>
  dplyr::group_by(Year, MP) |>
  dplyr::summarise(Landings=median(Landings))

ggplot(LandingsMedian, aes(x=Year, y=Landings, color=MP)) +
  geom_line()


# Plot Spawning Biomass
SB <- openMSE::get_SSB(MSE)
SB$MP[is.na(SB$MP)] <- 'Historical'

SBMedian <- SB |>
  dplyr::group_by(Year, MP) |>
  dplyr::summarise('Spawning Biomass'=median(Value))

ggplot(SBMedian, aes(x=Year, y=`Spawning Biomass`, color=MP)) +
  geom_line()





