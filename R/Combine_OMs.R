
#' Combine Multiple Single-Stock MOM objects into one Multi-Stock MOM
#'
#' @param OMList A list of `MOM` objects
#' @param Name  Name of the output MOM object
#' @param fill_yrs Number of years to calculate mean values to fill fishing effort (`cpars$Find`),
#' for any OMs that have a terminal year before the maximum terminal year of all OMs
#'
#' @return An object of class [MOM()]
#' @export
#'
Combine_OMs <- function(OMList, Name=NULL, fill_yrs=3) {

  stop('not working yet. Need to deal with MOMs with different terminal year')

  # ---- Checks ----
  classes <- lapply(OMList, class) |> unlist()
  if (!all(classes=='MOM'))
    stop('`OMList` must be a list of `MOM` objects')

  nsims <- lapply(OMList, slot, 'nsim') |> unlist()
  if (!all(nsims==mean(nsims)))
    stop('`nsim` must be the same for all `MOM` objects')


  proyears <- lapply(OMList, slot, name='proyears') |> unlist()
  if (!all(proyears==proyears[1]))
    stop('proyears must be the same for all `MOM` objects')

  fleets <- list()
  for (s in seq_along(OMList)) {
    n.fleets <- OMList[[s]]@Fleets[[1]] |> length()
    fleet.names <- names(OMList[[s]]@Fleets[[1]])
    fleets[[s]] <- data.frame(Fleets=fleet.names)
  }
  if (!length(unique(fleets)) == 1)
    stop('Must have the same number and same names for fleets in each `MOM` object')


  # ---- Define output OM ----
  nsim <- nsims[1]
  proyears <- proyears[1]
  MOM <- OMList[[1]]
  if (!is.null(Name)) {
    MOM@Name <- Name
  } else {
    names <- lapply(OMList, slot, 'Name')
    MOM@Name <- paste(names, collapse=', ')
  }


  # ---- Set Maximum Age ----
  # needs to be the same for all stocks
  stocks <- lapply(OMList, slot, name='Stocks')
  n.stocks <- length(stocks)
  maxage <- lapply(lapply(stocks, '[[', 1), slot, name='maxage') |> unlist() |> max()
  nage <- maxage+1
  # combine stock slots
  MOM@Stocks <- vector('list', n.stocks)
  for (s in 1:n.stocks) {
    MOM@Stocks[[s]] <- stocks[[s]][[1]]
    MOM@Stocks[[s]]@maxage <- maxage
    names(MOM@Stocks)[s] <- stocks[[s]][[1]]@Name
  }

  # ---- Combine fleets, obs, imps, & cpars ----
  MOM@Fleets <-  vector('list', n.stocks)
  names(MOM@Fleets) <-  names(MOM@Stocks)
  MOM@Obs <- vector('list', n.stocks)
  names(MOM@Obs) <-  names(MOM@Stocks)
  MOM@Imps <- vector('list', n.stocks)
  names(MOM@Imps) <-  names(MOM@Stocks)
  MOM@cpars <-  vector('list', n.stocks)
  names(MOM@cpars) <-  names(MOM@Stocks)
  MOM@CatchFrac <- vector('list', n.stocks)
  names(MOM@CatchFrac) <-  names(MOM@Stocks)

  for (s in seq_along(OMList)) {
    n.fleet <- length(OMList[[s]]@Fleets[[1]])
    MOM@Fleets[[s]] <- vector('list', n.fleet)
    MOM@cpars[[s]] <- vector('list', n.fleet)
    for (fl in 1:n.fleet) {
      MOM@Fleets[[s]][[fl]] <- OMList[[s]]@Fleets[[1]][[fl]]
      MOM@Obs[[s]][[fl]] <- OMList[[s]]@Obs[[1]][[fl]]
      MOM@Imps[[s]][[fl]] <- OMList[[s]]@Imps[[1]][[fl]]
      MOM@cpars[[s]][[fl]] <- OMList[[s]]@cpars[[1]][[fl]]
    }
    MOM@CatchFrac[[s]] <- OMList[[s]]@CatchFrac[[1]]

    # add fleet names
    names(MOM@Fleets[[s]]) <- fleet.names
    names(MOM@Obs[[s]]) <- fleet.names
    names(MOM@Imps[[s]]) <- fleet.names
    names(MOM@cpars[[s]]) <- fleet.names
  }


  # ---- Check Current Year ----
  get_nyear <- function(MOM) {
    data.frame(CurrentYr= MOM@Fleets[[1]][[1]]@CurrentYr,
               NYr=MOM@Fleets[[1]][[1]]@nyears,
               Stock=MOM@Stocks[[1]]@Name)
  }


  nyear_list <- lapply(OMList, get_nyear)
  nyear_df <- do.call('rbind', nyear_list)

  max_hist_year <- max(nyear_df$CurrentYr)
  min_hist_year <- min(nyear_df$CurrentYr)

  # ---- Update cpars for missing years and age-classes ----
  curYr <- max_year
  ind <- which.max(nyear_df$NYr)

  if (nyear_df$CurrentYr[ind]<curYr) {
    nyear_out <- nyear_df$NYr[ind] + curYr-nyear_df$CurrentYr[ind]
  } else {
    nyear_out <- nyear_df$NYr[ind]
  }

  stop('need to update for OMs that have different terminal years ')

  hist.yrs <- rev(seq(curYr, by=-1, length.out=nyear_out))
  nareas <- dim(MOM@cpars[[1]][[1]]$mov)[3]

  for (s in seq_along(OMList)) {
    n.fleet <- length(OMList[[s]]@Fleets[[1]])
    for (fl in 1:n.fleet) {
      MOM@Fleets[[s]][[fl]]@nyears <- nyear_out
      MOM@Fleets[[s]][[fl]]@CurrentYr <- curYr
      cpars <- MOM@cpars[[s]][[fl]]

      stock_nyear <- nyear_df$NYr[s]
      MOM@cpars[[s]][[fl]] <- update_cpars(cpars, stock_nyear, nyear_out, nage, proyears, nareas, fill_yrs)
    }
  }


  # ---- Set TAC for Off-Season fleets to Removals instead of Retained Landings ----
  MOM@cpars$control$TAC <- 'removals'

  # ---- Set Observation Parameters for OM ----
  for (s in  seq_along(OMList)) {
    n.fleet <- length(OMList[[s]]@Fleets[[1]])
    for (fl in 1:n.fleet) {
      # set CAL samples high so can use results in PMs
      MOM@Obs[[s]][[fl]]@CAL_ESS <- c(10000, 10000)
      MOM@Obs[[s]][[fl]]@CAL_nsamp <- c(10000, 10000)
    }
  }

  MOM
}

update_cpars <- function(cpars, stock_nyear, nyear_out, nage, proyears, nareas, fill_yrs=3) {
  D <- dim(cpars$Len_age)
  this.nage <- D[2]
  this.nyear <- dim(cpars$Find)[2]
  nsim <- D[1]


  if (stock_nyear !=this.nyear | nage!=this.nage) {
    # need to update
    cpars.dim <- lapply(cpars, dim)
    for (i in seq_along(cpars.dim)) {
      if (!is.null(cpars.dim[[i]])) {

        # update 4D arrays
        if (length(cpars.dim[[i]])==4) {
          if (names(cpars.dim[i]) =='mov') {
            dummy.age <- nage - cpars.dim[[i]][2]
            dummy.val <- cpars[[i]][1,cpars.dim[[i]][2],,]
            dummy.val <- array(dummy.val, dim=c(nsim, dummy.age, nareas, nareas))
            cpars[[i]] <- abind::abind(cpars[[i]], dummy.val, along=2)
          }
        } else if (length(cpars.dim[[i]])==3) {
          dummy.age <- nage - cpars.dim[[i]][2]
          dummy.yr <- stock_nyear - (cpars.dim[[i]][3]-proyears)

          # add ages
          if(names(cpars.dim[i]) != 'Fdisc_array2') {
            if (names(cpars.dim[i]) == 'V' | names(cpars.dim[i]) == 'retA') {
              # use the value from the last age class
              dummy.val <- cpars[[i]][1,cpars.dim[[i]][2],this.nyear]
              dummy.val <- array(dummy.val, dim=c(nsim, dummy.age, this.nyear+proyears))
            } else {
              # populate with zeros
              if (names(cpars.dim[i]) == 'initdist' ) {
                dummy.val <- array(0, dim=c(nsim, dummy.age, nareas))
              } else {
                dummy.val <- array(0, dim=c(nsim, dummy.age, this.nyear+proyears))
              }
            }
          }

          if(names(cpars.dim[i]) != 'Fdisc_array2') {
            cpars[[i]] <- abind::abind(cpars[[i]], dummy.val, along=2)
          }

          # add years
          if(names(cpars.dim[i]) != 'initdist') {
            dummy.val <- replicate(dummy.yr, cpars[[i]][,,1])
            cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=3)
          }

        } else if (length(cpars.dim[[i]])==2) {
          # add years
          if (names(cpars.dim[i]) == 'Perr_y') {
            dummy.age <- nage - this.nage

            # first add 1s for additional ages
            dummy.val <- matrix(1, nrow=nsim, ncol=dummy.age)
            cpars[[i]] <- cbind(dummy.val, cpars[[i]])

            # add additional years
            add.yrs <- (stock_nyear+proyears+nage-1) - dim(cpars[[i]])[2]
            new_Perr_y <- array(NA, dim=c(nsim, (stock_nyear+proyears+nage-1)))

            # populate first n_age
            new_Perr_y[,1:nage] <- cpars[[i]][,1:nage]

            # add additional initial years
            new_Perr_y[,(nage+1):(add.yrs+nage)] <- 1
            #
            # # add additional initial years
            # new_Perr_y[,1:add.yrs] <- 1
            #
            # # populate first n_age
            # new_Perr_y[,(add.yrs+1):(add.yrs+nage)] <- cpars[[i]][,1:nage]
            #

            # populate rest
            new_Perr_y[,(add.yrs+nage+1):ncol(new_Perr_y)] <- cpars[[i]][,(nage+1):ncol(cpars[[i]])]
            cpars[[i]] <- new_Perr_y

          } else {
            if (names(cpars.dim[i]) == 'Find') {
              val <- 0
              dummy.yr <- (stock_nyear)-(cpars.dim[[i]][2])
              dummy.val <- matrix(val, nrow=nsim, ncol=dummy.yr)
              cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=2)
            } else {
              val <- 0
              dummy.yr <- (stock_nyear)-(cpars.dim[[i]][2]-proyears)
              dummy.val <- matrix(val, nrow=nsim, ncol=dummy.yr)
              cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=2)
            }

          }
        }
      }
    }
  }

  if (stock_nyear != nyear_out) {
    add_yrs <- nyear_out - stock_nyear
    # update Find
    # calculate geometric mean
    histFs <- cpars$Find * cpars$qs
    meanF <- apply(histFs[,(stock_nyear-fill_yrs+1):stock_nyear], 1, geomean)
    meanE <- meanF / cpars$qs
    cpars$Find <- cbind(cpars$Find,
                        replicate(add_yrs, meanE)
    )



  }

  cpars
}




