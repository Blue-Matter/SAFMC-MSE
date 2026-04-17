
# Need to have common fleets for all stocks

library(SAMSE)

FleetTable()
StockFleetTable()

OM_List <- LoadOM()

OM_List_Combined <- OM_List

# Combine Fleets

OM_List_Combined$GrayTriggerfish <- OM_List$GrayTriggerfish |>
  CombineFleets(FleetList = list(
    cHL = c('cHLn', 'cHLs'),
    rGN = c('rGNn', 'rGNs'),
    rHB = c('rHBs')
  ))

# do rest of stocks

Hist <- Simulate(OM_List$GrayTriggerfish)
Hist2 <- Simulate(GrayTriggerfish)

df1 <- Biomass(Hist, df=TRUE)
df2 <- Biomass(Hist2, df=TRUE)

plot(df1$Year, df1$Value)
lines(df2$Year, df2$Value)



