
# Need to have common fleets for all stocks

library(SAMSE)

OMpath <- "../SAFMC-MSE/Objects/OM"
# OMpath <- "Objects/OM"


# List of all the OM that have been imported
OM_List <- LoadOM(OMpath=OMpath)

# List of Stock and Fleets
FleetTable(OM_List)
StockFleetTable(OM_List)

CommonFleets <- c('cHL', 'rHB', 'rGN', 'cOT')

StockFleetList <- list(

  GagGrouper = list(
    cOT = c('cDV')
  ),

  ScampGrouper = list(
    cOT = c('cGN')
  ),

  GrayTriggerfish = list(
    cHL = c('cHLn', 'cHLs'),
    rGN = c('rGNn', 'rGNs'),
    rHB = c('rHBs')
  ),

  BlackSeaBass = list(
    cOT = c('cPT')
  ),

  SnowyGrouper = list(
    cOT = c('cLL')
  ),

  Tilefish = list(
    cOT = c('cLL')
  ),

  VermilionSnapper = list(
    cOT = c('cOT', 'cTW')
  ),

  RedPorgy = list(
    cOT = c('cTW')
  )
)

OM_List <- MakeCommonFleets(OM_List, CommonFleets, StockFleetList, OMpath=OMpath)


OM_List <- LoadOM(OMpath=OMpath, type='MultiStock')


Multi_OM_Files <- ListOMFiles(OMpath=OMpath, type='MultiStock')


