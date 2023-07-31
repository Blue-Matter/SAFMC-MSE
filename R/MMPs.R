

#' Example fixed F MP
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
#'
fixedF_mean3 <- function(x, DataList, ...) {
  out <<- DataList

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  this_Fleet_Management <- Fleet_Management
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      meanF <- mean(tail(DataList[[s]][[f]]@Misc$FleetPars$Find[x,],3))
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
class(fixedF_mean3) <- 'MMP'

#' Example fixed F MP with MLL
#'
#' @param x Simulation number
#' @param DataList A list of `Data` objects
#' @param ... Additional arguments. Not used
#'
#' @return A list of `Rec` objects
#' @export
fixedF_mean3_MLL <- function(x, DataList, ...) {

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
      meanF <- mean(tail(DataList[[s]][[f]]@Misc$FleetPars$Find[x,],3))
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
class(fixedF_mean3_MLL) <- 'MMP'



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


