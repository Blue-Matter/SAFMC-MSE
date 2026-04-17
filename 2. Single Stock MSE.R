

# Spatial Regions - Assume entire SAFMC management area



# Individual MSE

OM <- LoadOM('Gag Grouper')

Hist <- Simulate(OM)

MSE <- Project(Hist, MPs='CurrentEffort')



OM_List <- purrr::map(OM_List, Rename, Fleets=Fleets)
FleetNames_List <- purrr::map(OM_List, FleetNames)
FleetNames_to_dt(FleetNames_List)

OM_List$`Scamp Grouper`@Name


Hist <- Simulate(OM_List[[1]])

Hist@Landings |> dimnames()

OM <- OM_List[[1]]
FleetNames(OM)
t <- PopulateOM(OM)
FleetNames(t)


LoadArgs(Simulate_om)

Hist@Landings |> dimnames()

