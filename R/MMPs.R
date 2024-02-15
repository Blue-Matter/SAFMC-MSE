
calc_status_quo <- function(x, DataList, incE=list()) {

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  Years <- DataList[[1]][[1]]@Year
  LHYear <- DataList[[1]][[1]]@LHYear
  pyear <- length(Years[Years>LHYear])

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])
      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))

      if (length(incE)>0) {
        flname <- strsplit(as.character(fleets[f]), ':')[[1]][1]
        if (flname %in% names(incE)) {
          ind <- which(names(incE)==flname)
          eff_inc <- (1+incE[[ind]])^pyear
          this_Fleet_Management <-  this_Fleet_Management %>%
            dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF*eff_inc))
        }
      }
    }
  }
  this_Fleet_Management
}

add_size_limits <- function(Fleet_Management, MLL=list()) {
  if (length(MLL)<1) return(Fleet_Management)
  for (i in seq_along(MLL)) {
    stock <- names(MLL)[i]
    mll <- inch2mm(MLL[[i]])
    Fleet_Management <- Fleet_Management %>%
      dplyr::mutate(MLL=replace(MLL, Stock==stock, mll))

  }
  Fleet_Management
}

Effort_multiplier <- function(Fleet_Management, Eff_multi=list()) {
  if (length(Eff_multi)<1)
    return(Fleet_Management)
  for (i in seq_along(Eff_multi)) {
    fleet <- names(Eff_multi)[i]
    multi <- Eff_multi[[i]]
    Fleet_Management <- Fleet_Management %>% mutate(F = ifelse(Fleets%in% fleet, multi*F, F))
  }
  Fleet_Management
}


#' Example fixed F MP that sets F for each fleet to the mean from the last 3 historical years
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
StatusQuo <- function(x, DataList, incE=list(), MLL=list(), Eff_multi=list(), ...) {
  this_Fleet_Management <- calc_status_quo(x, DataList, incE)
  this_Fleet_Management <- add_size_limits(this_Fleet_Management, MLL=MLL)
  this_Fleet_Management <- Effort_multiplier(this_Fleet_Management, Eff_multi)
  Fleet_MMP(x, DataList, Fleet_Management=this_Fleet_Management)
}
# define as class `MMP`
class(StatusQuo) <- 'MMP'


#' @describeIn StatusQuo Status Quo MP with General Recreational Effort increasing by 2% per year
#' @export
StatusQuo_IncRecEff <- function(x, DataList, incE=list('General Recreational'=0.02), ...) {
  StatusQuo(x, DataList, incE=incE)
}
# define as class `MMP`
class(StatusQuo_IncRecEff) <- 'MMP'


#' @describeIn StatusQuo StatusQuo with Effort for General Recreational Fleet reduce by 20% for both stocks
#' @export
RecEff20 <- function(x, DataList, incE=list(), MLL=list(), Eff_multi=list('General Recreational'=0.8), ...) {
  StatusQuo(x, DataList, incE=incE, MLL, Eff_multi)
}
# define as class `MMP`
class(RecEff20) <- 'MMP'


#' @describeIn StatusQuo RecEff20 with General Recreational Effort increasing by 2% per year
#' @export
RecEff20_IncRecEff <- function(x, DataList,
                                    incE=list('General Recreational'=0.02),
                                    MLL=list(),
                                    Eff_multi=list('General Recreational'=0.8), ...) {

  StatusQuo(x, DataList, incE=incE, MLL, Eff_multi)
}
# define as class `MMP`
class(RecEff20_IncRecEff) <- 'MMP'

#' @describeIn StatusQuo StatusQuo with Effort for General Recreational Fleet reduce by 40% for both stocks
#' @export
RecEff40 <- function(x, DataList,
                     incE=list(),
                     MLL=list(),
                     Eff_multi=list('General Recreational'=0.6), ...) {
  StatusQuo(x, DataList, incE=incE, MLL, Eff_multi)
}
class(RecEff40) <- 'MMP'

#' @describeIn StatusQuo RecEff40 with General Recreational Effort increasing by 2% per year
#' @export
RecEff40_IncRecEff <- function(x, DataList,
                     incE=list('General Recreational'=0.02),
                     MLL=list(),
                     Eff_multi=list('General Recreational'=0.6), ...) {
  StatusQuo(x, DataList, incE=incE, MLL, Eff_multi)
}
class(RecEff40_IncRecEff) <- 'MMP'



#' Example fixed F MP that sets the F for each stock to the MFMT
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
Ftarget <- function(x, DataList, incE=list(), MLL=list(), ...) {

  this_Fleet_Management <- calc_status_quo(x, DataList)

  # Calculate relative F for each fleet (by Stock)
  this_Fleet_Management <- this_Fleet_Management %>% group_by(Stock) %>% mutate(Frat=F/sum(F))

  MFMT <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                     MFMT=c(0.21, 0.42))

  # Set overall F to MFMT for each stock
  this_Fleet_Management <- left_join(this_Fleet_Management, MFMT, by='Stock')
  this_Fleet_Management <- this_Fleet_Management %>% mutate(F=MFMT*Frat)

  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
# define as class `MMP`
class(Ftarget) <- 'MMP'


#' @describeIn Ftarget Ftarget with General Recreational Effort increasing by 2% per year
#' @export
Ftarget_IncRecEff <- function(x, DataList, ...) {

  this_Fleet_Management <- calc_status_quo(x, DataList)

  # Calculate relative F for each fleet (by Stock)
  this_Fleet_Management <- this_Fleet_Management %>% group_by(Stock) %>% mutate(Frat=F/sum(F))

  MFMT <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                     MFMT=c(0.21, 0.42))

  # Set overall F to MFMT for each stock
  this_Fleet_Management <- left_join(this_Fleet_Management, MFMT, by='Stock')
  this_Fleet_Management <- this_Fleet_Management %>% mutate(F=MFMT*Frat)


  fleets <- unique(Fleet_Management$Fleet)
  nfleets <- length(fleets)

  Years <- DataList[[1]][[1]]@Year
  LHYear <- DataList[[1]][[1]]@LHYear
  pyear <- length(Years[Years>LHYear])

  rec_fleets <- fleets[grepl('General Recreational', fleets)]
  # increase rec effort
  if (length(incE)>0) {
    eff_inc <- (1+incE$`General Recreational`)^pyear
    this_Fleet_Management <- this_Fleet_Management %>% mutate(F = ifelse(Fleet%in% rec_fleets, F*eff_inc, F))
  }

  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
# define as class `MMP`
class(Ftarget_IncRecEff) <- 'MMP'



#' @describeIn StatusQuo StatusQuo with Minimum Legal Length for both stocks (20 inch for Red Snapper and 25 inch for Gag)
#' @export
MLL20_25 <- function(x, DataList, incE=list(), MLL=list(`Red Snapper`=20, `Gag Grouper`=25), ...) {

  StatusQuo(x, DataList, MLL=MLL, ...)
}
# define as class `MMP`
class(MLL20_25) <- 'MMP'


#' @describeIn StatusQuo MLL20_25 with General Recreational Effort increasing by 2% per year
#' @export
MLL20_25_IncRecEff <- function(x, DataList,
                                    incE=list('General Recreational'=0.02),
                                    MLL=list(`Red Snapper`=20, `Gag Grouper`=25),
                                    ...) {
  StatusQuo(x, DataList, incE=incE, MLL=MLL, ...)
}
# define as class `MMP`
class(MLL20_25_IncRecEff) <- 'MMP'

#' @describeIn StatusQuo StatusQuo with Minimum Legal Length for both stocks (25 inch for Red Snapper and 25 inch for Gag)
#' @export
MLL25_25 <- function(x, DataList, incE=list(), MLL=list(`Red Snapper`=25, `Gag Grouper`=25), ...) {

  StatusQuo(x, DataList, MLL=MLL, ...)
}
# define as class `MMP`
class(MLL25_25) <- 'MMP'


#' @describeIn StatusQuo MLL25_25 with General Recreational Effort increasing by 2% per year
#' @export
MLL25_25_IncRecEff <- function(x, DataList,
                               incE=list('General Recreational'=0.02),
                               MLL=list(`Red Snapper`=25, `Gag Grouper`=25),
                               ...) {
  StatusQuo(x, DataList, incE=incE, MLL=MLL, ...)
}
# define as class `MMP`
class(MLL25_25_IncRecEff) <- 'MMP'





#' Example fixed catch MP (removals)
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
fixedC_mean3 <- function(x, DataList, ...) {

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  this_Fleet_Management <- Fleet_Management
  nyears <- ncol(DataList[[1]][[1]]@Misc$FleetPars$Find)

  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # historical removals
      meanC <- mean(apply(DataList[[s]][[f]]@Misc$FleetPars$CB[x,,(nyears-2):nyears,], 2, sum))

      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(Catch=replace(Catch, Stock==stocks[s] &Fleet==fleets[f], meanC))
    }
  }
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
class(fixedC_mean3) <- 'MMP'


