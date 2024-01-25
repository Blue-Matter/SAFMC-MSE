
#' Combine Discard and Landing Fleets in an MOM object
#'
#' @param MOM An object of class `MOM`
#' @param stock The stock number (currently only works for 1 stock)
#' @param fleets List with each element numeric vector length 2 to indicate landing and discard fleet.
#' Length 1 means only a landing fleet
#'
#' @return An object of class `MOM` with `length(fleets)` fleets
#' @export
#'
combineFleets <- function(MOM, stock=1, fleets=list(c(1,4),
                                                    c(2,5),
                                                    c(3,6))) {
  # currently only works for 1 stock
  n.stock <- length(MOM@Stocks)
  n.fleet <- length(MOM@Fleets[[1]])
  nyears <- MOM@Fleets[[1]][[1]]@nyears

  nsim <- MOM@nsim
  newMOM <- MOM
  maxage <- MOM@Stocks[[1]]@maxage
  n_age <- maxage+1
  currentYr <- MOM@Fleets[[1]][[1]]@CurrentYr
  years <- rev(seq(currentYr, length.out=nyears, by=-1))
  proyears <- MOM@proyears
  p.years <- seq(currentYr+1, length.out=proyears, by=1)
  all.years <- c(years, p.years)

  for (fl in 1:length(fleets)) {
    flt <- fleets[[fl]]

    ## Calculate overall fishing mortality
    f1 <- MOM@cpars[[1]][[flt[1]]]$Find * matrix(MOM@cpars[[1]][[flt[1]]]$qs, nrow=nsim, ncol=nyears)
    if (length(flt)>1)
      f2 <- MOM@cpars[[1]][[flt[2]]]$Find * matrix(MOM@cpars[[1]][[flt[2]]]$qs, nrow=nsim, ncol=nyears)

    f1 <- replicate(n_age,f1) %>% aperm(., (c(1,3,2)))
    if (length(flt)>1)
      f2 <- replicate(n_age,f2) %>% aperm(., (c(1,3,2)))

    f1_at_age <- f1 * MOM@cpars[[1]][[flt[1]]]$V[,,1:nyears]
    if (length(flt)>1)
      f2_at_age <- f2 * MOM@cpars[[1]][[flt[2]]]$V[,,1:nyears]

    if (length(flt)>1) {
      f1_comb_at_age <- f1_at_age + f2_at_age
    } else {
      f1_comb_at_age <- f1_at_age
    }

    Find <- apply(f1_comb_at_age, c(1,3), max)

    ## Calculate overall selectivity
    FMax <- replicate(n_age, Find) %>% aperm(., c(1,3,2))
    histV <- f1_comb_at_age/  FMax

    NA_years <- which(!is.finite(histV[1,1,]))
    for (y in nyears:1) {
      if (y %in% NA_years) {
        histV[,,y] <- histV[,,y+1]
      }
    }

    # add projection years
    projV <- replicate(proyears, histV[,,nyears])
    V <- abind::abind(histV, projV, along=3)

    ## Calculate catch frac
    if (length(flt)>1) {
      newMOM@CatchFrac[[1]][,flt[1]] <- apply(MOM@CatchFrac[[1]][,flt], 1, sum)
    } else {
      newMOM@CatchFrac[[1]][,flt[1]] <- MOM@CatchFrac[[1]][,flt]
    }

    newMOM@cpars[[1]][[flt[1]]]$Find <- Find
    newMOM@cpars[[1]][[flt[1]]]$V <- V

  }
  ii <- which(lapply(fleets, length)>1)
  disc.fleets <- sapply(fleets[ii], "[[", 2)

  # Remove discard fleets
  newMOM@Fleets[[1]][disc.fleets] <- NULL
  newMOM@cpars[[1]][disc.fleets] <- NULL



  newMOM
}


#' Calculate removals by season for Red Snapper Commercial handline
#'
#' @param year numeric year
#' @param logbook commercial logbook dataframe
#' @param season_df season dataframe
#'
#' @return data.frame
#' @export
calc_seasons_RS_Comm <- function(year, logbook, season_df) {

  df <- logbook %>% filter(LandingYear ==year)
  df$Date <- lubridate::mdy(df$UnloadDate)
  df$Month <- lubridate::month(df$Date)
  df$Day <- lubridate::day(df$Date)
  df$open <- FALSE

  # Assign open/closed season
  open_dates <- season_df %>% filter(Year ==year)
  for (i in 1:nrow(open_dates)) {
    if (!is.na(open_dates$Date_Open[i])) {
      int <- lubridate::interval(open_dates$Date_Open[i], open_dates$Date_Closed[i]-1)
      df$open[df$Date %within% int] <- TRUE
    }
  }

  # Dead discards
  disc_mort <- unique(open_dates$Disc_M)
  df <- df %>% mutate(ReportedDiscard.Alive=as.numeric(ReportedDiscard.Alive)*disc_mort,
                      Disc.AvgInd.Wgt=as.numeric(Disc.AvgInd.Wgt),
                      Discard_Weight=(ReportedDiscard.Alive)*Disc.AvgInd.Wgt)

  # Landings
  Landings <- df %>% filter(open==TRUE) %>% summarize(Landings=sum(WholePounds /1000, na.rm=T),
                                                      Discard_Weight=sum(Discard_Weight/1000, na.rm=T))

  Discard_Rate <- Landings$Discard_Weight/(Landings$Landings+Landings$Discard_Weight)
  if (!is.finite(Discard_Rate)) Discard_Rate <- NA

  Off_Season <- df %>% filter(open==FALSE) %>% summarize(Discard_Weight=sum(Discard_Weight, na.rm=T))/1000
  out <- data.frame(Year=year,
                    On.Season=Landings$Landings /(Landings$Landings+Off_Season$Discard_Weight))
  out$Off.Season <- 1-out$On.Season
  out$Fdisc <- disc_mort
  out$Discard_Rate <- Discard_Rate
  out
}

#' @describeIn calc_seasons_RS_Comm Calculate season for Red Snapper Headboat
#' @export
calc_seasons_RS_HB <- function(year, logbook, season_df) {

  df <- logbook %>% filter(Year ==year)
  df$Month <- df$Month
  df$Day <- df$Day
  df$Date <- lubridate::dmy(paste(df$Day, df$Month, df$Year, sep='-'))
  df$open <- FALSE


  # Assign open/closed season
  open_dates <- season_df %>% filter(Year ==year)
  for (i in 1:nrow(open_dates)) {
    if (!is.na(open_dates$Date_Open[i])) {
      int <- lubridate::interval(open_dates$Date_Open[i], open_dates$Date_Closed[i]-1)
      df$open[df$Date %within% int] <- TRUE
    }
  }

  # Dead discards
  disc_mort <- unique(open_dates$Disc_M)

  df <- df %>% mutate(Numkept=as.numeric(Numkept),
                      Discard_Alive=as.numeric(ReleasedLive),
                      Discard_Dead=as.numeric(ReleasedDead),
                      Discard_D_Total=(Discard_Alive*disc_mort)+Discard_Dead)

  # Landings
  Landings <- df %>% filter(open==TRUE) %>%  summarize(Landings=sum(Numkept, na.rm=T),
                                                       Discard_D_Total=sum(Discard_D_Total, na.rm=T))

  Discard_Rate <- Landings$Discard_D_Total/(Landings$Landings+Landings$Discard_D_Total)
  if (!is.finite(Discard_Rate)) Discard_Rate <- NA

  Off_Season <- df %>% filter(open==FALSE) %>% summarize(Discard_D_Total=sum(Discard_D_Total, na.rm=T))

  out <- data.frame(Year=year,
                    On.Season=Landings$Landings /(Landings$Landings+Off_Season$Discard_D_Total))
  out$Off.Season <- 1-out$On.Season
  out$Fdisc <- disc_mort
  out$Discard_Rate <- Discard_Rate
  out
}

#' Genarate OM with On- and Off-Season Fleets - Red Snapper
#'
#' @param MOM An object of class `MOM`
#' @param relF A dataframe with the relative F for On- and Off-Season for each year
#' @param On.Fleet Index for On-Season Fleet
#' @param Off.Fleet Index for Off-Season Fleet
#' @param Fleet Name of the Fleet
#'
#' @return An object of class `MOM`
#' @export
AddSeasonal_RS <- function(MOM, relF, On.Fleet=1, Off.Fleet=4, Fleet='Commercial Handline') {
  nsim <- MOM@nsim
  currentYr <- MOM@Fleets[[1]][[1]]@CurrentYr
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  Hist.Years <- rev(seq(currentYr, by=-1, length.out=nyears))
  allyears <- MOM@proyears + nyears
  proyears <- MOM@proyears

  MOM@Fleets[[1]][[Off.Fleet]] <- MOM@Fleets[[1]][[On.Fleet]]
  names(MOM@Fleets[[1]])[On.Fleet] <- paste(Fleet, ' On-Season', sep=':')
  names(MOM@Fleets[[1]])[Off.Fleet] <- paste(Fleet, ' Off-Season', sep=':')

  on.season.cpars <- MOM@cpars[[1]][[On.Fleet]]
  off.season.cpars <- MOM@cpars[[1]][[On.Fleet]]

  # Discard mortality
  off.season.cpars$Fdisc_array1 <- array(0, dim=dim(off.season.cpars$V))
  on.season.cpars$Fdisc_array1 <- array(0, dim=dim(on.season.cpars$V))

  nbins <- length(off.season.cpars$CAL_binsmid)
  off.season.cpars$Fdisc_array2 <- array(0, dim=c(nsim, nbins, nyears+proyears))
  on.season.cpars$Fdisc_array2 <- array(0, dim=c(nsim, nbins, nyears+proyears))

  for (y in seq_along(Hist.Years)) {
    yr <- Hist.Years[y]
    Fdisc <- relF %>% filter(Year >= yr) %>% select(Fdisc)
    on.season.cpars$Fdisc_array1[,,y] <- Fdisc$Fdisc[1]
    off.season.cpars$Fdisc_array1[,,y] <- Fdisc$Fdisc[1]

    on.season.cpars$Fdisc_array2[,,y] <- Fdisc$Fdisc[1]
    off.season.cpars$Fdisc_array2[,,y] <- Fdisc$Fdisc[1]

  }

  # projection years
  on.season.cpars$Fdisc_array1[,,(nyears+1):allyears] <- on.season.cpars$Fdisc_array1[,,nyears]
  off.season.cpars$Fdisc_array1[,,(nyears+1):allyears] <- off.season.cpars$Fdisc_array1[,,nyears]

  on.season.cpars$Fdisc_array2[,,(nyears+1):allyears] <- on.season.cpars$Fdisc_array2[,,nyears]
  off.season.cpars$Fdisc_array2[,,(nyears+1):allyears] <- off.season.cpars$Fdisc_array2[,,nyears]

  # Off-Season - retention = 0
  off.season.cpars$retA <- array(0, dim=dim(off.season.cpars$V))

  # On-Season Discard Rate
  on.season.cpars$DR_y <- array(0, dim=c(nsim, allyears))
  for (y in seq_along(Hist.Years)) {
    yr <- Hist.Years[y]
    DR <- relF %>% filter(Year == yr) %>% select(Discard_Rate)
    if (length(DR$Discard_Rate)>0) {
      if (is.na(DR$Discard_Rate[1] )) DR$Discard_Rate[1]  <- 0
      on.season.cpars$DR_y[,y] <- DR$Discard_Rate[1]
    } else {
      on.season.cpars$DR_y[,y] <- 0
    }
  }
  # projection years
  on.season.cpars$DR_y[,(nyears+1):allyears] <- on.season.cpars$DR_y[,nyears]


  # Fishing mortality
  for (y in seq_along(Hist.Years)) {
    yr <- Hist.Years[y]
    relFDF <- relF %>% filter(Year==yr)
    if (nrow(relFDF)<1) {
      off.season.cpars$Find[,y] <- 0
    } else {

      on.season.cpars$Find[,y] <-  on.season.cpars$Find[,y] * relFDF$On.Season
      off.season.cpars$Find[,y] <- (off.season.cpars$Find[,y] * relFDF$Off.Season) / off.season.cpars$Fdisc_array1[,1,y]
    }
  }

  MOM@cpars[[1]][[On.Fleet]] <- on.season.cpars
  MOM@cpars[[1]][[Off.Fleet]] <- off.season.cpars
  MOM
}
