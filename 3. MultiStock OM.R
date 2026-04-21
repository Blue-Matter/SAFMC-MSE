
# Need to have common fleets for all stocks

library(SAMSE)

FleetTable()

OM_List <- LoadOM()

StockFleetTable(OM_List)

OM_List_Combined <- OM_List

# Combine Fleets
# Rename Fleets


OM_List_Combined$GagGrouper <- OM_List$GagGrouper |>
  CombineFleets(FleetList = list(
    cOT = c('cDV')
  ))

Rename

OM_List_Combined$GagGrouper@Obs$`Gag Grouper`
OM_List_Combined$GagGrouper@Data$`Gag Grouper`@Landings@Name

# TODO
# - Obs
# - Data
# - then CombineOM


OM_List_Combined$ScampGrouper <- OM_List$ScampGrouper |>
  CombineFleets(FleetList = list(
    cOT = c('cGN')
  ))

OM_List_Combined$ScampGrouper <- OM_List$ScampGrouper |>
  CombineFleets(FleetList = list(
    cOT = c('cGN')
  ))

OM_List_Combined$GrayTriggerfish <- OM_List$GrayTriggerfish |>
  CombineFleets(FleetList = list(
    cHL = c('cHLn', 'cHLs'),
    rGN = c('rGNn', 'rGNs'),
    rHB = c('rHBs')
  ))

OM_List_Combined$BlackSeaBass <- OM_List$BlackSeaBass |>
  CombineFleets(FleetList = list(
    cOT = c('cPT')
  ))

OM_List_Combined$SnowyGrouper <- OM_List$SnowyGrouper |>
  CombineFleets(FleetList = list(
    cOT = c('cLL')
  ))

OM_List_Combined$Tilefish <- OM_List$Tilefish |>
  CombineFleets(FleetList = list(
    cOT = c('cLL')
  ))

OM_List_Combined$VermilionSnapper <- OM_List$VermilionSnapper |>
  CombineFleets(FleetList = list(
    cOT = c('cTW')
  ))

OM_List_Combined$RedPorgy <- OM_List$RedPorgy |>
  CombineFleets(FleetList = list(
    cOT = c('cTW')
  ))

MissFleetDF <- MissingFleets(OM_List_Combined)

OM <- OM_List_Combined$GrayTriggerfish

AddDummuFleet <- function(OM, FleetName) {

}

StockFleetTable(OM_List_Combined)




# do rest of stocks

Stock <- 'ScampGrouper'

Hist <- Simulate(OM_List[[Stock]])
Hist2 <- Simulate(OM_List_Combined[[Stock]])

df1 <- Biomass(Hist, df=TRUE)
df2 <- Biomass(Hist2, df=TRUE)

plot(df1$Year, df1$Value)
lines(df2$Year, df2$Value)




