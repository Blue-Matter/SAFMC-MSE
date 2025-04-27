library(SAMSE)

# ---- Load OPeragting Model ----
OM <- readRDS('Training/RS.om')

# ---- Simulate Historical Fishery ----

Hist <- Simulate(OM)

class(Hist)
# List of `Hist` objects
# Hierarchical list - Stock by Fleet

names(Hist)

names(Hist$`Red Snapper`)

class(Hist$`Red Snapper`$`Commercial Line`)

# `Hist` object contains the historical fishery dynamics
# https://openmse.com/object-hist/
class?Hist

class()slotNames(Hist$`Red Snapper`$`Commercial Line`)

# ---- Compare with BAM Output ----

Compare_Biomass(Hist, bamExtras::rdat_RedSnapper)

# ---- Explore Historical Fishery Dynamics ----

CHLandings <- Hist$`Red Snapper`$`Commercial Line`@TSdata$Landings
RHLandings <- Hist$`Red Snapper`$`Recreational Headboat`@TSdata$Landings
GRLandings <- Hist$`Red Snapper`$`General Recreational`@TSdata$Landings

CHDiscards <- Hist$`Red Snapper`$`Commercial Line`@TSdata$Discards
RHDiscards <- Hist$`Red Snapper`$`Recreational Headboat`@TSdata$Discards
GRDiscards <- Hist$`Red Snapper`$`General Recreational`@TSdata$Discards

dim(CHLandings) # 10 simulations, 70 historical years, 2 areas

CHLandings[1:5,1,] # Historical period identical over simulations

HistYears <- Hist$`Red Snapper`$`Commercial Line`@Data@Year

CatchDF <- dplyr::bind_rows(
  data.frame(Year=HistYears,
             Fleet='Commercial Line',
             Landings=rowSums(CHLandings[1,,]),
             Discards=rowSums(CHDiscards[1,,])),
  data.frame(Year=HistYears,
             Fleet='Recreational  Headboat',
             Landings=rowSums(RHLandings[1,,]),
             Discards=rowSums(RHDiscards[1,,])),
  data.frame(Year=HistYears,
             Fleet='General Recreational',
             Landings=rowSums(GRLandings[1,,]),
             Discards=rowSums(GRDiscards[1,,]))
)

pCatchDF <- CatchDF |> tidyr::pivot_longer(cols=c('Landings', 'Discards'),
                                          names_to='Type',
                                          values_to='Catch')

ggplot(pCatchDF, aes(x=Year, y=Catch, color=Type)) +
  facet_wrap(~Fleet) +
  geom_line()


