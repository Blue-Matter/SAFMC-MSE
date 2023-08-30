

#' Example fixed F MP that sets F for each fleet to the mean from the last 3 historical years
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
StatusQuo <- function(x, DataList, ...) {

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])
      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }
  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, Fleet_Management=this_Fleet_Management)
}
# define as class `MMP`
class(StatusQuo) <- 'MMP'

#' Example fixed F MP with MLL
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
StatusQuo_MLL <- function(x, DataList, ...) {

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])
      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }

  # Set MLL
  MLL_RS <- inch2mm(20) # 20 inch MLL
  this_Fleet_Management <- this_Fleet_Management %>%
    dplyr::mutate(MLL=replace(MLL, Stock=='Red Snapper', MLL_RS))

  MLL_GG <- inch2mm(25) # 25 inch MLL
  this_Fleet_Management <- this_Fleet_Management %>%
    dplyr::mutate(MLL=replace(MLL, Stock=='Gag Grouper', MLL_GG))

  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
# define as class `MMP`
class(StatusQuo_MLL) <- 'MMP'

#' Example fixed F MP that sets the F for each stock to the MFMT
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
Ftarget <- function(x, DataList, ...) {

  MFMT <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                     MFMT=c(0.21, 0.42))

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])

      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }

  # Calculate relative F for each fleet (by Stock)
  this_Fleet_Management <- this_Fleet_Management %>% group_by(Stock) %>% mutate(Frat=F/sum(F))


  # Set overall F to MFMT for each stock
  this_Fleet_Management <- left_join(this_Fleet_Management, MFMT, by='Stock')
  this_Fleet_Management <- this_Fleet_Management %>% mutate(F=MFMT*Frat)


  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
# define as class `MMP`
class(Ftarget) <- 'MMP'


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


