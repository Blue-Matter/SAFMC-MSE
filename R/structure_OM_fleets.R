
#' Create Seasonal Fleet Structured OM from BAM Assessment
#'
#' Currently only tested for Red Snapper and Gag
#'
#' Should work for other assessments, provided they have a similar
#' fleet structure to RS and Gag, i.e., Commercial Handline, Recreational Headboat,
#' and General Recreational Landing and Discards fleets
#'
#' @param rdat An `rdat` object from the `BAMextras` package
#' @param stock_name Name of the stock object
#' @param nsim Number of simulations in the MSE
#' @param pyears Number of projection years in the MSE
#' @param CAL_bins The Catch-at-Length bins used to generate CAL data. Must be the
#' same across all stocks and fleets in a multi-species OM
#' @param Fleet_Structure A data.frame with the name of the fleets, and index
#' values for the position of the Landing and Discard for each fleet
#' @param seasonal_F_List A list of n fleets long, with each element containing
#' a dataframe with the relative F for the On-Season and Off-Season fleets, the discard
#' mortality, the Discard rate, and the name of the fleet
#'
#' @return an object of class `MOM` with fleets structure into On and Off Seasons
#' @export
structure_OM <- function(rdat, stock_name, nsim, pyears, CAL_bins, Fleet_Structure,
                         seasonal_F_List) {

  bin_width <- CAL_bins[2] - CAL_bins[1]
  CAL_mids <- seq(0.5*bin_width, by=bin_width, length.out=length((CAL_bins)-1))

  ##  Convert all weights to metric (kg) ----
  rdat$a.series$weight <- rdat$parms$wgt.a*
    rdat$a.series$length^rdat$parms$wgt.b

  ## Import SEDAR Assessment into OM ----
  OM <- BAM2MOM(rdat=rdat, stock_name=stock_name,
                         nsim = nsim, proyears = pyears)

  # Add CALbins to cpars
  nfleet <- length(OM@cpars[[1]])
  for (f in 1:nfleet) {
    OM@cpars[[1]][[f]]$CAL_bins  <- CAL_bins
    OM@cpars[[1]][[f]]$CAL_binsmid <- CAL_mids
  }

  ## Combine Landing and Discard Fleets together into Removals ----
  fleet_structure <- vector('list', nrow(Fleet_Structure))
  for (i in seq_along(fleet_structure)) {
    ind <- Fleet_Structure[i, 2:3]
    ind <- ind[!is.na(ind)]
    fleet_structure[[i]] <- ind
  }
  OM_combined <- combineFleets(OM, fleets=fleet_structure)

  # Generate OM with Seasonal Fleets ----
  OM_season <- OM_combined

  nf <- length(seasonal_F_List)
  for (i in 1:nf) {
    seasonal_F_DF <- seasonal_F_List[[i]]
    fleet <- unique(seasonal_F_DF$Fleet)
    temp <- Fleet_Structure %>% filter(Name==fleet)
    OM_season <- AddSeasonal_RS(OM_season, seasonal_F_DF,
                                On.Fleet=temp$Landing,
                                Off.Fleet=temp$Discard,
                                Fleet=temp$Name)
  }
  OM_season
}
