#
# MOM <- GGMOM
# fleets_df <- GG_fleets
# discMortList <- GG_discMort

Combine_discard_fleets <- function(MOM, fleets_df, discMortList) {
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

  all.fleets <- fleets_df$code
  disc.fleets.ind <- grepl('\\.D', all.fleets)
  main.fleets.ind <- !disc.fleets.ind
  disc.fleets <- which(disc.fleets.ind)
  main.fleets <- which(main.fleets.ind)

  newMOM@CatchFrac <- list()
  newMOM@CatchFrac[[1]] <- matrix(NA, nrow=nsim, ncol=length(main.fleets))

  for (fl in main.fleets) {
    disc.fleet <- which(fleets_df$code == paste0(fleets_df$code[fl], '.D'))

    if (length(disc.fleet) >0 && !is.na(disc.fleet)) {

      # Combine CatchFrac
      newMOM@CatchFrac[[1]][,fl] <- apply(MOM@CatchFrac[[1]][,c(fl, disc.fleet)], 1, sum)

      # create Fdisc_array
      Fdisc_year <- discMortList[[fleets_df$code[fl]]]
      fdisc_DF <- data.frame(Year=all.years, Fdisc=NA)
      for (i in 1:nrow(Fdisc_year)) {
        if (i ==1) {
          fst.yr <- all.years[1]
        } else {
          fst.yr <- lst.yr+1
        }
        lst.yr <- Fdisc_year[i,2]
        fdisc_DF$Fdisc[fdisc_DF$Year%in%fst.yr:lst.yr] <- Fdisc_year[i,1]
      }

      Fdisc_array1 <- fdisc_DF$Fdisc
      Fdisc_array1 <- replicate(maxage+1, Fdisc_array1)
      Fdisc_array1 <- replicate(nsim, Fdisc_array1)
      Fdisc_array1 <- aperm(Fdisc_array1, c(3,2,1))
      Fdisc_array1[,,(nyears+1):(nyears+proyears)] <- Fdisc_array1[,,nyears]
      newMOM@cpars[[1]][[fl]]$Fdisc_array1 <- Fdisc_array1

      # calculate realized selectivity and retention-at-age curves
      retA_real <- MOM@cpars[[1]][[fl]]$V[,,1:nyears]
      disc_sel <- MOM@cpars[[1]][[disc.fleet]]$V[,,1:nyears]

      F_ret <-  MOM@cpars[[1]][[fl]]$Find * matrix(MOM@cpars[[1]][[fl]]$qs, nrow=nsim, ncol=nyears)
      F_dead_disc <- MOM@cpars[[1]][[disc.fleet]]$Find * matrix(MOM@cpars[[1]][[fl]]$qs, nrow=nsim, ncol=nyears)

      F_ret_age <- aperm(replicate(n_age, F_ret), c(1,3,2)) * retA_real
      F_dead_disc_age <- aperm(replicate(n_age, F_dead_disc), c(1,3,2)) * disc_sel
      F_tot_age <- F_ret_age + F_dead_disc_age

      correct_V <- function(V) {
        nsim <- nrow(V)
        for (i in 1:nsim) {
          v.max <- apply(V[i,,], 2, max)
          v.max[!is.finite(v.max)] <- 0
          ind <- which(v.max==0)
          ind2 <- min(which(v.max!=0))
          V[i,,ind] <- V[i,,ind2]
        }
        V
      }

      F_tot_max <- apply(F_tot_age, c(1,3), max)
      F_tot_max <- replicate(n_age, F_tot_max) %>% aperm(., c(1,3,2))
      V_real <- F_tot_age/F_tot_max
      V_real <- correct_V(V=V_real)

      F_ret_max <- apply(F_ret_age, c(1,3), max)
      F_ret_max <- replicate(n_age, F_ret_max) %>% aperm(., c(1,3,2))
      retA_real <- F_ret_age/F_tot_max
      retA_real <- correct_V(V=retA_real)

      # selectivity & retention for projection years
      V_real_p <- V_real[,,nyears]
      V_real_p <- replicate(proyears, V_real_p)
      V_real <- abind::abind(V_real, V_real_p, along=3)

      retA_real_p <- retA_real[,,nyears]
      retA_real_p <- replicate(proyears, retA_real_p)
      retA_real <- abind::abind(retA_real, retA_real_p, along=3)

      # back-calculate V and retA
      V <- ((V_real - retA_real)/Fdisc_array1)+retA_real
      V[!is.finite(V)] <- 1E-15
      V <- V/(replicate(n_age, apply(V, c(1,3), max)) %>% aperm(., c(1,3,2)))
      retA <- retA_real/V
      retA[!is.finite(retA)] <- 1E-15
      retA <- retA/(replicate(n_age, apply(retA, c(1,3), max)) %>% aperm(., c(1,3,2)))

      retA_real2 <- V * retA
      V_real2 <- retA_real + ((V-retA_real) * Fdisc_array1)

      newMOM@cpars[[1]][[fl]]$Find <- apply(F_tot_max, c(1,3), max)
      newMOM@cpars[[1]][[fl]]$retA <- retA
      newMOM@cpars[[1]][[fl]]$V <- V
      newMOM@cpars[[1]][[fl]]$retA_real <- retA_real
      newMOM@cpars[[1]][[fl]]$V_real <- V_real
    } else {
      newMOM@CatchFrac[[1]][,fl] <- MOM@CatchFrac[[1]][,fl]
    }
  }

  # remove Fleet and cpars for discard fleets
  newMOM@Fleets[[1]][disc.fleets] <- NULL
  newMOM@cpars[[1]][disc.fleets] <- NULL

  newMOM
}

get_nyear <- function(MOM) {
  data.frame(CurrentYr= MOM@Fleets[[1]][[1]]@CurrentYr,
             NYr=MOM@Fleets[[1]][[1]]@nyears)
}

Combine_MOM <- function(MOMlist, name='Combined MOM') {
  MOM <- MOMlist[[1]]
  MOM@Name <- name

  n.moms <- length(MOMlist)

  # checks
  nsims <- lapply(MOMlist, slot, name='nsim') %>% unlist()
  if (!all(nsims==nsims[1]))
    stop('nsim must be the same for all MOMs')
  nsim <- nsim[1]
  proyears <- lapply(MOMlist, slot, name='proyears') %>% unlist()
  proyears <- proyears[1]
  if (!all(proyears==proyears[1]))
    stop('proyears must be the same for all MOMs')

  # check fleet names
  fl_list <- list()
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    fleet.names <- lapply(MOMlist[[s]]@Fleets[[1]], slot, name='Name') %>%
      unlist() %>% as.character()
    fl_list[[s]] <- data.frame(Fleets=fleet.names)
  }
  all.fleets <- do.call('rbind', fl_list) %>% unique()  %>% unlist()
  n.fleet.by.stock <- lapply(fl_list, nrow) %>% unlist()
  max.fleet <- n.fleet.by.stock%>% max()

  # add dummy fleets to MOM
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    if (n.fleets <max.fleet) {
      # need to add dummy fleet(s)
      missing.fleets <- all.fleets[!all.fleets %in% (unlist(fl_list[[s]]))]
      for (fl in seq_along(missing.fleets)) {
        dummy.fleet <- as.vector(missing.fleets[fl])
        n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
        MOMlist[[s]]@Fleets[[1]][[n.fleets+1]] <- new('Fleet')
        MOMlist[[s]]@Obs[[1]][[n.fleets+1]] <- new('Obs')
        MOMlist[[s]]@Imps[[1]][[n.fleets+1]] <- new('Imp')
        n.cpars <- MOMlist[[s]]@cpars[[1]] %>% length()
        MOMlist[[s]]@cpars[[1]][[n.cpars+1]] <- list()

        # copy fleet, obs, and imp properties and cpars from fleet in other MOM (the first one it finds)
        for (ss in 1:length(fl_list)) {
          ind <- which(dummy.fleet == (fl_list[[ss]] %>% unlist()))
          if (length(ind)>0)
            break()
        }
        MOMlist[[s]]@Fleets[[1]][[n.fleets+1]] <- MOMlist[[ss]]@Fleets[[1]][[ind]]
        MOMlist[[s]]@Fleets[[1]][[n.fleets+1]]@Name <- dummy.fleet
        MOMlist[[s]]@cpars[[1]][[n.fleets+1]] <- MOMlist[[ss]]@cpars[[1]][[ind]]
        MOMlist[[s]]@Obs[[1]][[n.fleets+1]] <- MOMlist[[ss]]@Obs[[1]][[ind]]
        MOMlist[[s]]@Imps[[1]][[n.fleets+1]] <- MOMlist[[ss]]@Imps[[1]][[ind]]

        MOMlist[[s]]@CatchFrac[[1]] <- cbind(MOMlist[[s]]@CatchFrac[[1]],rep(0, nsim))
        # set q to 0
        MOMlist[[s]]@cpars[[1]][[n.fleets+1]]$qs <- rep(0, nsim)

      }
    }
  }

  # re-order fleets, cpars, and CatchFrac so names match
  fl_list <- list()
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    fleet.names <- lapply(MOMlist[[s]]@Fleets[[1]], slot, name='Name') %>%
      unlist() %>% as.character()
    fl_list[[s]] <- data.frame(Fleets=fleet.names)
  }
  # re-order to match Stock 1
  for (s in 2:n.moms) {
    ind <- match(unlist(fl_list[[1]]), unlist(fl_list[[s]]))
    MOMlist[[s]]@Fleets[[1]] <-  MOMlist[[s]]@Fleets[[1]][ind]
    MOMlist[[s]]@CatchFrac[[1]] <- MOMlist[[s]]@CatchFrac[[1]][,ind]
    MOMlist[[s]]@cpars[[1]] <-  MOMlist[[s]]@cpars[[1]][ind]
    MOMlist[[s]]@Obs[[1]] <-  MOMlist[[s]]@Obs[[1]][ind]
    MOMlist[[s]]@Imps[[1]] <-  MOMlist[[s]]@Imps[[1]][ind]
  }

  # final check on fleet names
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    fleet.names <- lapply(MOMlist[[s]]@Fleets[[1]], slot, name='Name') %>% unlist() %>% as.character()
    fl_list[[s]] <- data.frame(Fleets=fleet.names)
  }
  n.fleets.per.stock <- lapply(fl_list, nrow) %>% unlist()
  if (!all(n.fleets.per.stock==max.fleet))
    stop('Error adding dummy fleets')

  stocks <- lapply(MOMlist, slot, name='Stocks')
  n.stocks <- length(stocks)

  # calculate maximum age - needs to be the same for all stocks
  maxage <- lapply(lapply(stocks, '[[', 1), slot, name='maxage') %>% unlist() %>% max()
  nage <- maxage+1
  # combine stock slots
  MOM@Stocks <- vector('list', n.stocks)
  for (s in 1:n.stocks) {
    MOM@Stocks[[s]] <- stocks[[s]][[1]]
    MOM@Stocks[[s]]@maxage <- maxage
    names(MOM@Stocks)[s] <- stocks[[s]][[1]]@Name
  }

  # combine fleets, obs, imps, & cpars
  MOM@Fleets <-  vector('list', n.stocks)
  names(MOM@Fleets) <-  MOM@Stocks
  MOM@Obs <- vector('list', n.stocks)
  names(MOM@Obs) <-  MOM@Stocks
  MOM@Imps <- vector('list', n.stocks)
  names(MOM@Imps) <-  MOM@Stocks
  MOM@cpars <-  vector('list', n.stocks)
  names(MOM@cpars) <-  MOM@Stocks

  MOM@CatchFrac <- vector('list', n.stocks)

  # MOM@Allocation
  # MOM@Complexes
  # MOM@SexPars
  # MOM@Rel

  for (s in 1:n.moms) {
    n.fleet <- length(MOMlist[[s]]@Fleets[[1]])
    MOM@Fleets[[s]] <- vector('list', n.fleet)
    MOM@cpars[[s]] <- vector('list', n.fleet)
    for (fl in 1:n.fleet) {
      MOM@Fleets[[s]][[fl]] <- MOMlist[[s]]@Fleets[[1]][[fl]]
      MOM@Obs[[s]][[fl]] <- MOMlist[[s]]@Obs[[1]][[fl]]
      MOM@Imps[[s]][[fl]] <- MOMlist[[s]]@Imps[[1]][[fl]]
      MOM@cpars[[s]][[fl]] <- MOMlist[[s]]@cpars[[1]][[fl]]
    }
    MOM@CatchFrac[[s]] <- MOMlist[[s]]@CatchFrac[[1]]

    # add fleet names
    names(MOM@Fleets[[s]]) <- fleet.names
    names(MOM@Obs[[s]]) <- fleet.names
    names(MOM@Imps[[s]]) <- fleet.names
    names(MOM@cpars[[s]]) <- fleet.names
  }

  # update all arrays to match n.age and n.years
  nyear_df <- lapply(MOMlist, get_nyear) %>% do.call('rbind', .)
  nyear_df$Stock <- 1:length(MOMlist)
  if (!all(nyear_df$CurrentYr == nyear_df$CurrentYr[1]))
    stop('Current Year must be the same for all Stocks/Fleets for this function to work')

  curYr <- nyear_df$CurrentYr[1]
  nyear <- nyear_df$NYr %>% max()
  hist.yrs <- rev(seq(curYr, by=-1, length.out=nyear))
  for (s in 1:n.moms) {
    n.fleet <- length(MOMlist[[s]]@Fleets[[1]])
    for (fl in 1:n.fleet) {
      MOM@Fleets[[s]][[fl]]@nyears <- nyear
      cpars <- MOM@cpars[[s]][[fl]]
      MOM@cpars[[s]][[fl]] <- update_cpars(cpars, nyear, nage, proyears)
    }
  }

  # combine Imps
  MOM
}


update_cpars <- function(cpars, nyear, nage, proyears) {
  D <- dim(cpars$Len_age)
  this.nage <- D[2]
  this.nyear <- dim(cpars$Find)[2]
  if (nyear !=this.nyear | nage!=this.nage) {
    # need to update
    cpars.dim <- lapply(cpars, dim)
    for (i in seq_along(cpars.dim)) {
      if (!is.null(cpars.dim[[i]])) {
        # update 3D arrays
        if (length(cpars.dim[[i]])==3) {
          dummy.age <- nage - cpars.dim[[i]][2]
          dummy.yr <- nyear - (cpars.dim[[i]][3]-proyears)

          # add ages
          dummy.val <- array(0, dim=c(nsim, dummy.age, this.nyear+proyears))
          cpars[[i]] <- abind::abind(cpars[[i]], dummy.val, along=2)

          # add years
          dummy.val <- replicate(dummy.yr, cpars[[i]][,,1])
          cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=3)
        }
        # update 2D matrices
        if (length(cpars.dim[[i]])==2) {
          # add years
          if (names(cpars.dim[i]) == 'Perr_y') {
            dummy.age <- nage - this.nage

            # first add 1s for additional ages
            dummy.val <- matrix(1, nrow=nsim, ncol=dummy.age)
            cpars[[i]] <- cbind(dummy.val, cpars[[i]])

            # add additional years
            add.yrs <- (nyear+proyears+nage-1) - dim(cpars[[i]])[2]
            new_Perr_y <- array(NA, dim=c(nsim, (nyear+proyears+nage-1)))
            # populate first n_age
            new_Perr_y[,1:nage] <- cpars[[i]][,1:nage]

            # add additional initial years
            new_Perr_y[,(nage+1):(add.yrs+nage)] <- 1

            # populate rest
            new_Perr_y[,(add.yrs+nage+1):ncol(new_Perr_y)] <- cpars[[i]][,(nage+1):ncol(cpars[[i]])]
            cpars[[i]] <- new_Perr_y

          } else {
            val <- 0
            dummy.yr <- (nyear)-cpars.dim[[i]][2]
            dummy.val <- matrix(val, nrow=nsim, ncol=dummy.yr)
            cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=2)
          }
        }
      }
    }
  }
  cpars
}
