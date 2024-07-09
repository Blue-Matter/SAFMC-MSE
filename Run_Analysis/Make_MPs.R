
# TODO
# MLL for the three stocks
# - calculate optimal from YPR

Name <- DataList[[1]][[1]]@Name
get_MLL <- function(Name) {

  grepl('Red Snapper', Name)


}


Rec_Reduction

SQ
SQ_FR
SQ_MLL
SQ_NS
SQ_OS
SQ_FR_MLL
SQ_FR_MLL_NS
SQ_FR_MLL_OS

MPs <- list(SQ=paste('SQ', Rec_Reduction, sep='_'),
            SQ_FR=paste('SQ_FR', Rec_Reduction, sep='_'),
            SQ_MLL= paste('SQ_MLL', Rec_Reduction, sep='_'),
            SQ_NS= paste('SQ_NS', Rec_Reduction, sep='_'),
            SQ_OS= paste('SQ_OS', Rec_Reduction, sep='_'),
            SQ_FR_MLL= paste('SQ_FR_MLL', Rec_Reduction, sep='_'),
            SQ_FR_MLL_NS= paste('SQ_FR_MLL_NS', Rec_Reduction, sep='_'),
            SQ_FR_MLL_OS= paste('SQ_FR_MLL_OS', Rec_Reduction, sep='_')
)

OM <- readRDS('OM_Objects/BaseCase_RS.OM')
Hist <- Simulate(OM, nsim=2)




SQ <- function(x, DataList, ...) {
  rec_out <- Create_Rec_List(DataList)
  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])

  year_info <- Get_Year_Info(DataList)
  yr_ind <- year_info$Last_3_Yrs

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- exp(mean(log(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))
      lastF <- DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind[length(yr_ind)]]

      deltaE <- meanF/lastF
      if (!is.finite(deltaE)) deltaE <- 1E-5
      rec_out[[s]][[f]]@Effort <- deltaE
    }
  }
  rec_out
}
class(SQ) <- 'MMP'

Modify_Fleet_Effort <- function(rec_out, Effort_Mod=list(0,0,0.05,0), ...) {
  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      rec_out[[s]][[f]]@Effort <- rec_out[[s]][[f]]@Effort * (1-Effort_Mod[[fl]])
    }
  }
  rec_out
}
class(Modify_Fleet_Effort) <- 'MMP'



SQ_FR <- function(x, DataList, First_Management_Year=2025,...) {

  rec_out <- SQ(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {
      #R_age <- DataList[[s]][[f]]@Misc$FleetPars$retA[x,,yr_ind]
      #R_age <- R_age/max(R_age)

      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Fdisc <- DataList[[s]][[f]]@Misc$FleetPars$Fdisc_array1[x,,yr_ind]

      r_age <- V_age
      r_age[which.max(r_age):length(r_age)] <- 1
      r_age <- r_age/max(r_age)
      rec_out[[s]][[f]]@Misc$R_age <- r_age # R_age
      rec_out[[s]][[f]]@Misc$Fdisc <- Fdisc
    }
  }
  rec_out
}
class(SQ_FR) <- 'MMP'

SQ_MLL <- function(x, DataList, First_Management_Year=2025, ...) {
  rec_out <- SQ(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  MLL <- get_MLL(DataList[[1]][[1]]@Name)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {

      # calculate retention-at-length
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Len_age <- DataList[[s]][[f]]@Misc$StockPars$Len_age[x,,yr_ind]
      LenCV <- DataList[[s]][[f]]@Misc$StockPars$LenCV[x]
      Len_SD <- LenCV*Len_age

      CAL_binsmid <- seq(0, max(Len_age)+2*max(Len_SD), by=5)
      sel_at_length <- rep(1, length(CAL_binsmid))
      sel_at_length[CAL_binsmid<=MLL] <- 0

      ret_a <- MSEtool:::calcVatAge(matrix(Len_age),
                                    matrix(Len_SD),
                                    matrix(sel_at_length),
                                    length(Len_age),
                                    nyears=1,
                                    proyears=0,
                                    CAL_binsmid)
      ret_a <- ret_a[,1]
      ret_a[!is.finite(ret_a)] <- 0

      rec_out[[s]][[f]]@Misc$R_age <-  rec_out[[s]][[f]]@Misc$R_age * ret_a
    }
  }
  rec_out
}
class(SQ_MLL) <- 'MMP'

SQ_NS <- function(x, DataList, First_Management_Year=2025, ...) {
  rec_out <- SQ(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      rec_out[[s]][[f]]@Spatial <- c(1,0,1,0,1,0)
      rec_out[[s]][[f]]@Allocate <- 1
    }
  }
  rec_out
}
class(SQ_NS) <- 'MMP'

SQ_OS <- function(x, DataList, First_Management_Year=2025, ...) {
  rec_out <- SQ(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)
  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      rec_out[[s]][[f]]@Spatial <- c(0,1, 0,1,0,1)
      rec_out[[s]][[f]]@Allocate <- 1
    }
  }
  rec_out
}
class(SQ_OS) <- 'MMP'

SQ_FR_MLL <- function(x, DataList, First_Management_Year=2025, MLL=600, ...) {
  rec_out <- SQ_FR(x, DataList)

  SQ_MLL(x, DataList)

  stop()
  # apply MLL above

  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {

      # calculate retention-at-length
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Len_age <- DataList[[s]][[f]]@Misc$StockPars$Len_age[x,,yr_ind]
      LenCV <- DataList[[s]][[f]]@Misc$StockPars$LenCV[x]
      Len_SD <- LenCV*Len_age

      CAL_binsmid <- seq(0, max(Len_age)+2*max(Len_SD), by=5)
      sel_at_length <- rep(1, length(CAL_binsmid))
      sel_at_length[CAL_binsmid<=MLL] <- 0

      ret_a <- MSEtool:::calcVatAge(matrix(Len_age),
                           matrix(Len_SD),
                           matrix(sel_at_length),
                           length(Len_age),
                           nyears=1,
                           proyears=0,
                           CAL_binsmid)
      ret_a <- ret_a[,1]
      ret_a[!is.finite(ret_a)] <- 0

      rec_out[[s]][[f]]@Misc$R_age <-  rec_out[[s]][[f]]@Misc$R_age * ret_a
    }
  }
  rec_out
}
class(SQ_FR_MLL) <- 'MMP'

SQ_FR_MLL_NS <- function(x, DataList, First_Management_Year=2025, MLL=600, ...) {
  rec_out <- SQ_FR(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {

      # calculate retention-at-length
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Len_age <- DataList[[s]][[f]]@Misc$StockPars$Len_age[x,,yr_ind]
      LenCV <- DataList[[s]][[f]]@Misc$StockPars$LenCV[x]
      Len_SD <- LenCV*Len_age

      CAL_binsmid <- seq(0, max(Len_age)+2*max(Len_SD), by=5)
      sel_at_length <- rep(1, length(CAL_binsmid))
      sel_at_length[CAL_binsmid<=MLL] <- 0

      ret_a <- MSEtool:::calcVatAge(matrix(Len_age),
                                    matrix(Len_SD),
                                    matrix(sel_at_length),
                                    length(Len_age),
                                    nyears=1,
                                    proyears=0,
                                    CAL_binsmid)
      ret_a <- ret_a[,1]
      ret_a[!is.finite(ret_a)] <- 0

      rec_out[[s]][[f]]@Misc$R_age <-  rec_out[[s]][[f]]@Misc$R_age * ret_a
    }
  }
  rec_out
}
class(SQ_FR_MLL_NS) <- 'MMP'

SQ_FR_MLL_OS <- function(x, DataList, First_Management_Year=2025, MLL=600, ...) {
  rec_out <- SQ_FR(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(rec_out)

  nstocks <- length(rec_out)
  nfleets <- length(rec_out[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)

  # apply MLL to retention curve
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {

      # calculate retention-at-length
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Len_age <- DataList[[s]][[f]]@Misc$StockPars$Len_age[x,,yr_ind]
      LenCV <- DataList[[s]][[f]]@Misc$StockPars$LenCV[x]
      Len_SD <- LenCV*Len_age

      CAL_binsmid <- seq(0, max(Len_age)+2*max(Len_SD), by=5)
      sel_at_length <- rep(1, length(CAL_binsmid))
      sel_at_length[CAL_binsmid<=MLL] <- 0

      ret_a <- MSEtool:::calcVatAge(matrix(Len_age),
                                    matrix(Len_SD),
                                    matrix(sel_at_length),
                                    length(Len_age),
                                    nyears=1,
                                    proyears=0,
                                    CAL_binsmid)
      ret_a <- ret_a[,1]
      ret_a[!is.finite(ret_a)] <- 0

      rec_out[[s]][[f]]@Misc$R_age <-  rec_out[[s]][[f]]@Misc$R_age * ret_a
    }
  }
  rec_out
}
class(SQ_FR_MLL_OS) <- 'MMP'

MSE <- ProjectMOM(Hist, MPs=c('SQ', 'SQ_NS', 'SQ_OS'))

matplot(t(MSE@SB_SBMSY[1,1,,]), type='b', ylim=c(0,2))



MSE <- ProjectMOM(Hist, MPs=c('SQ', 'SQ_FR', 'SQ_FR_MLL'), extended = TRUE)



DataList <- select_MP(MSE@PPD)

select_MP <- function(PPD, MP=1) {
  DataList <- vector('list', length(PPD))
  for (s in 1:length(DataList)) {
    DataList[[s]] <- vector('list', length(PPD[[s]]))
    for (f in 1:length(DataList[[s]])) {
      DataList[[s]][[f]] <- PPD[[s]][[f]][[MP]]
    }
  }
  DataList
}

plot(MSE@PPD[[1]][[fl]][[2]]@Misc$FleetPars$retA_P[1,,70], type='l', ylim=c(0,1))
lines(MSE@PPD[[1]][[fl]][[2]]@Misc$FleetPars$retA_P[1,,76], col='blue')

plot(MSE@PPD[[1]][[fl]][[2]]@Misc$FleetPars$retA_P_real[1,,70], type='l', ylim=c(0,1))
lines(MSE@PPD[[1]][[fl]][[2]]@Misc$FleetPars$retA_P_real[1,,76], col='blue')

fl <- 3
par(mfrow=c(1,2))
matplot(t(MSE@Removals[1,1,fl,,]), type='b', ylim=c(0,max(c(MSE@Removals[1,1,fl,,], MSE@Catch[1,1,fl,,]))))
matplot(t(MSE@Catch[1,1,fl,,]), type='b', ylim=c(0,max(c(MSE@Removals[1,1,fl,,], MSE@Catch[1,1,fl,,]))))


cbind(MSE@Removals[1,1,fl,2,], MSE@Catch[1,1,fl,2,])
discards <- MSE@Removals[1,1,fl,,]- MSE@Catch[1,1,fl,,]

matplot(t(discards), type='b', ylim=c(0,max(c(MSE@Removals[1,1,fl,,], MSE@Catch[1,1,fl,,]))))


matplot(t(MSE@SB_SBMSY[1,1,,]), type='b', ylim=c(0,2))

matplot(t(MSE@F_FMSY[1,1,fl,,]), type='b', ylim=c(0,5))

fl <- tempfile()
fl
saveRDS(Hist,fl)



# RS General Rec

# BS Recreational Headboat

# BS General Rec



calculate_management <- function(x, DataList, ...) {
  nstocks <- length(DataList)
  nfleets <- length(DataList[[1]])


  # MP Recommendations



}


