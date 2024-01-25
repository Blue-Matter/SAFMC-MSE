
library(SAMSE)

# Location of raw data files
data.dir <- 'G:/Shared drives/BM shared/1. Projects/Snapper - Grouper SAFMC/Data/MSE data'

# MSE Parameters
nsim <- 50 # number of simulations
pyears <- 20 # number of projection years

# Observation Model
# CAL bins (mm)
bin_width <- 50
CAL_bins <- seq(from=0, to=1500, by=bin_width)
CAL_mids <- seq(0.5*bin_width, by=bin_width, length.out=length((CAL_bins)-1))

Obs_Model <- Perfect_Info



# --- build functions ----

add_size_limits_GG <- function(GGMOM_season, GG_Comm_Season, GG_Rec_Season,
                               Comm_SzLim, RecSzLim) {
  # add size limit - knife-edge
  current_yr <- GGMOM_season@Fleets[[1]][[1]]@CurrentYr
  nyears <- GGMOM_season@Fleets[[1]][[1]]@nyears
  proyears <- GGMOM_season@proyears
  Years <- rev(seq(current_yr, by=-1, length=nyears))

  # Commercial
  retA <- array(1, dim=dim(GGMOM_season@cpars[[1]][[1]]$V))
  Com_SL <- GG_Comm_Season %>% distinct(Year, Size_Limit)
  Com_SL <- bind_rows(Comm_SzLim,
                      Com_SL)
  for (i in seq_along(Com_SL$Year)) {
    year <- Com_SL$Year[i]
    ind <- match(year, Years)
    sl <- Com_SL$Size_Limit[i]
    sl_ind <- which(GGMOM_season@cpars[[1]][[1]]$Len_age[1,,ind]<sl)
    retA[,sl_ind,ind] <- 0
  }
  # projection years
  retA[,,(nyears+1):(nyears+proyears)] <- retA[,,(nyears)]

  GGMOM_season@cpars[[1]][[1]]$retA <- retA

  # Recreational
  retA <- array(1, dim=dim(GGMOM_season@cpars[[1]][[1]]$V))

  Rec_SL <- GG_Rec_Season %>% distinct(Year, Size_Limit)

  Rec_SL <- bind_rows(RecSzLim,
                      Rec_SL)

  for (i in seq_along(Rec_SL$Year)) {
    year <- Rec_SL$Year[i]
    ind <- match(year, Years)
    sl <- Rec_SL$Size_Limit[i]
    sl_ind <- which(GGMOM_season@cpars[[1]][[3]]$Len_age[1,,ind]<sl)
    retA[,sl_ind,ind] <- 0
  }
  # projection years
  retA[,,(nyears+1):(nyears+proyears)] <- retA[,,(nyears)]

  GGMOM_season@cpars[[1]][[3]]$retA <- retA
  GGMOM_season@cpars[[1]][[4]]$retA <- retA
  GGMOM_season
}

Combine_OMs <- function(MOMlist, Name, truncsd=2) {

  MOM <- MOMlist[[1]]
  MOM@Name <- Name
  n.moms <- length(MOMlist)

  # checks
  nsims <- lapply(MOMlist, slot, name='nsim') %>% unlist()
  if (!all(nsims==nsims[1]))
    stop('nsim must be the same for all MOMs')
  nsim <- nsims[1]
  proyears <- lapply(MOMlist, slot, name='proyears') %>% unlist()
  if (!all(proyears==proyears[1]))
    stop('proyears must be the same for all MOMs')
  proyears <- proyears[1]

  # fleet names
  fl_list <- list()
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    fleet.names <- names(MOMlist[[s]]@Fleets[[1]])
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

        nms <- names(MOMlist[[s]]@Fleets[[1]])
        nms[length(nms)] <- missing.fleets[fl]
        names(MOMlist[[s]]@Fleets[[1]]) <- nms

      }
    }

  }

  # re-order fleets, cpars, and CatchFrac so names match
  fl_list <- list()
  for (s in 1:n.moms) {
    n.fleets <- MOMlist[[s]]@Fleets[[1]] %>% length()
    fleet.names <- names(MOMlist[[s]]@Fleets[[1]])
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
    fleet.names <- names(MOMlist[[s]]@Fleets[[1]])
    fl_list[[s]] <- data.frame(Fleets=fleet.names)
  }
  n.fleets.per.stock <- lapply(fl_list, nrow) %>% unlist()
  if (!all(n.fleets.per.stock==max.fleet))
    stop('Error adding dummy fleets')

  # calculate maximum age - needs to be the same for all stocks
  stocks <- lapply(MOMlist, slot, name='Stocks')
  n.stocks <- length(stocks)
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
  names(MOM@Fleets) <-  names(MOM@Stocks)
  MOM@Obs <- vector('list', n.stocks)
  names(MOM@Obs) <-  names(MOM@Stocks)
  MOM@Imps <- vector('list', n.stocks)
  names(MOM@Imps) <-  names(MOM@Stocks)
  MOM@cpars <-  vector('list', n.stocks)
  names(MOM@cpars) <-  names(MOM@Stocks)
  MOM@CatchFrac <- vector('list', n.stocks)
  names(MOM@CatchFrac) <-  names(MOM@Stocks)

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
  get_nyear <- function(MOM) {
    data.frame(CurrentYr= MOM@Fleets[[1]][[1]]@CurrentYr,
               NYr=MOM@Fleets[[1]][[1]]@nyears)
  }

  nyear_df <- lapply(MOMlist, get_nyear) %>% do.call('rbind', .)
  nyear_df$Stock <- 1:length(MOMlist)
  if (!all(nyear_df$CurrentYr == nyear_df$CurrentYr[1]))
    stop('Current Year must be the same for all Stocks/Fleets for this function to work')


  # update cpars - for missing years and age-classes
  curYr <- nyear_df$CurrentYr[1]
  nyear <- nyear_df$NYr %>% max()
  hist.yrs <- rev(seq(curYr, by=-1, length.out=nyear))

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
            if(names(cpars.dim[i]) != 'Fdisc_array2') {
              if (names(cpars.dim[i]) == 'V' | names(cpars.dim[i]) == 'retA') {
                # use the value from the last age class
                dummy.val <- cpars[[i]][1,cpars.dim[[i]][2],this.nyear]
                dummy.val <- array(dummy.val, dim=c(nsim, dummy.age, this.nyear+proyears))
              } else {
                # populate with zeros
                dummy.val <- array(0, dim=c(nsim, dummy.age, this.nyear+proyears))
              }
            }

            if(names(cpars.dim[i]) != 'Fdisc_array2') {
              cpars[[i]] <- abind::abind(cpars[[i]], dummy.val, along=2)
            }


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
                dummy.yr <- (nyear)-(cpars.dim[[i]][2])
                dummy.val <- matrix(val, nrow=nsim, ncol=dummy.yr)
                cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=2)
              } else {
                val <- 0
                dummy.yr <- (nyear)-(cpars.dim[[i]][2]-proyears)
                dummy.val <- matrix(val, nrow=nsim, ncol=dummy.yr)
                cpars[[i]] <- abind::abind(dummy.val, cpars[[i]], along=2)
              }

            }
          }
        }
      }
    }
    cpars
  }

  for (s in 1:n.moms) {
    n.fleet <- length(MOMlist[[s]]@Fleets[[1]])
    for (fl in 1:n.fleet) {
      MOM@Fleets[[s]][[fl]]@nyears <- nyear
      cpars <- MOM@cpars[[s]][[fl]]
      MOM@cpars[[s]][[fl]] <- update_cpars(cpars, nyear, nage, proyears)
    }
  }

  # set TAC for Off-Season fleets to Removals instead of Retained Landings
  MOM@cpars$control$TAC <- 'removals'


  # Set Observation Parameters for OM

  for (s in 1:n.moms) {
    n.fleet <- length(MOMlist[[s]]@Fleets[[1]])
    for (fl in 1:n.fleet) {

      # Set all Obs to Perfect_Info for now
      MOM@Obs[[s]][[fl]] <- Obs_Model

      # set CAL samples high so can use results in PMs
      MOM@Obs[[s]][[fl]]@CAL_ESS <- c(10000, 10000)
      MOM@Obs[[s]][[fl]]@CAL_nsamp <- c(10000, 10000)
    }
  }

  #
  if (!is.null(truncsd)) {
    MOM <- Generate_Future_Rec_Devs(MOM, truncsd=truncsd)
  }

  MOM
}


calc_region_depth_dist <- function(areas_df, df_age_dist, frac_region_DF) {
  Age_Region <- dplyr::left_join(areas_df, df_age_dist, relationship = "many-to-many")
  Frac_Age_Region <- dplyr::left_join(Age_Region, frac_region_DF) %>%
    group_by(Age, Stock) %>%
    mutate(Frac_Depth_Area=Frac_Depth*Frac_Area)
  Frac_Age_Region$Region <- factor(Frac_Age_Region$Region, levels(areas_df$Region))
  Frac_Age_Region$Stock <- factor(Frac_Age_Region$Stock,unique(Frac_Age_Region$Stock), ordered = TRUE)
  Frac_Age_Region
}


add_spatial_to_OM <- function(MOM, average_prob, frac_other, Frac_Age_Region) {
  Ages <- Frac_Age_Region$Age %>% unique()
  nage <- length(Ages)
  nareas <- Frac_Age_Region$Region %>% unique() %>% length() *  Frac_Age_Region$Depth %>% unique() %>% length()
  Stocks <- Frac_Age_Region$Stock %>% unique()
  for (st in seq_along(Stocks)) {
    mov <- array(NA, dim=c(nage, nareas, nareas))
    initdist <- array(0,c(MOM@nsim,nage,nareas))
    for (a in seq_along(Ages)) {
      temp <- Frac_Age_Region %>% filter(Age==Ages[a], Stock==Stocks[st])
      mov_age <- makemov2(temp$Frac_Depth_Area/sum(temp$Frac_Depth_Area), prob=average_prob[[st]], probE=1,
                          frac_other=frac_other[[st]], plot=F)
      mov[a,,] <- mov_age
      initdist[, a,] <-  t(replicate(MOM@nsim,CalcAsymptoticDist(mov_age, temp$Frac_Depth_Area/sum(temp$Frac_Depth_Area))[1,]))

    }
    mov <- replicate(MOM@nsim, mov) %>% aperm(., c(4,1,2,3))
    MOM@cpars[[st]][[1]]$mov <- mov
    MOM@cpars[[st]][[1]]$initdist <- initdist
    # MOM@cpars[[st]][[1]]$Asize <- matrix(unique(Frac_Age_Region$Relative.Size), nrow=MOM@nsim, ncol=nareas, byrow=T)
  }
  MOM
}

