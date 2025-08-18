library(SAMSE)

# Black Sea Bass
# Multi-MP Example
# Alternative Recruitment Deviation Scenario

# ---- Settings ----
nsim <- 10 # number of simulations
proyears <- 25 # number of projection years
Obs_Model <- MSEtool::Perfect_Info

rdat <- bamExtras::rdat_BlackSeaBass |>
  bamExtras::standardize_rdat()

# bamExtras::bam2r()

?RCM

OM <- BAM2MOM(rdat,
              stock_name='BSB',
              Obs=Obs_Model,
              nsim=nsim,
              proyears=proyears)

OM@Fleets$BSB |> names()

OM@Fleets$BSB$cHL@nyears


Hist <- Simulate(OM)

Hist$BSB$cHL@SampPars$Stock$Len_age[1,,1] # length in mm

ProcessError <- Hist[[1]][[1]]@SampPars$Stock$Perr_y

nyears <- OM@Fleets$BSB$cHL@nyears
FirstYear <- OM@Fleets$BSB$cHL@CurrentYr -nyears+1

maxage <- OM@Stocks$BSB@maxage
HistYears <- (FirstYear-maxage):OM@Fleets$BSB$cHL@CurrentYr
ProjYears <- (OM@Fleets$BSB$cHL@CurrentYr+1):(OM@Fleets$BSB$cHL@CurrentYr+proyears)

dimnames(ProcessError) <- list(Sim=1:OM@nsim,
                               Year=c(HistYears, ProjYears))

OM@Stocks$BSB@Perr
OM@Stocks$BSB@AC

# 2012 - 2021
logrecdevs <- log(ProcessError[1,46:54])
mu <- mean(logrecdevs)
sd <- sd(logrecdevs)
AC <- acf(logrecdevs)$acf[2,1,1]

mu <- mu + -0.5 * sd^2  * (1 - AC)/sqrt(1 - AC^2)

rldevs <- rnorm(n=nsim*proyears,
                mean=mu,
                sd=sd)
rldevs <- matrix(rldevs, nsim, proyears)

for (y in 2:proyears) {
  rldevs[,y] <- AC *rldevs[,y-1] + rldevs[,y]*sqrt(1-AC^2)
}

devs <- exp(rldevs)

dimnames(devs) <- list(Sim=1:OM@nsim,
                               Year=c(ProjYears))


HistRecent <- Hist

# update rec devs for projection years
HistRecent[[1]][[1]]@SampPars$Stock$Perr_y[,56:80] <- devs


myMP <- function(x, DataList, ...) {

  # fl <- tempfile()
  # print(fl)
  # saveRDS(DataList, fl)

  RecList <- Create_Rec_List(DataList)
  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])

  tiny <- 1E-6
  yr_ind <- 41:44

  # Commercial Line
  RecList[[1]][[1]]@Effort <- tiny
  RecList[[1]][[5]]@Effort <- tiny

  # Pot
  RecList[[1]][[2]]@Effort <- tiny

  # Headboat
  RecList[[1]][[3]]@Effort <- tiny

  meanFLandings <- exp(mean(log(DataList[[1]][[3]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))
  meanFDiscards <- exp(mean(log(DataList[[1]][[6]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))

  RecList[[1]][[6]]@Effort <- 1 + meanFLandings/(meanFDiscards+meanFLandings)

  # Rec
  meanFLandings <- exp(mean(log(DataList[[1]][[4]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))
  meanFDiscards <- exp(mean(log(DataList[[1]][[7]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))

  RecList[[1]][[4]]@Effort <- 0.5
  RecList[[1]][[7]]@Effort <- 1 + meanFLandings/(meanFDiscards+meanFLandings)


  # class?Rec
  # RecList[[1]][[7]]@LR5 <- 308 # 12 inch MLL
  # RecList[[1]][[7]]@LRF <- 310

  # hack approach - avoids gnarly internal conversion issues from LR5 approach
  # RecList[[s]][[f]]@Misc$R_age <- c(0,0,0,0.25,0.45, ...) # length maxage+1 # R_age - vector of retention at age

  RecList
}
class(myMP) <- 'MMP'

MSE <- ProjectMOM(Hist, MP=c('myMP', 'curE'))

MSE_recent_recdevs <- ProjectMOM(HistRecent, MP=c('myMP', 'curE'))


plot(MSE)
plot(MSE_recent_recdevs)

MSE@SSB |> dim() # nsim, nstock, nMP, projection years

RefPoint <- 300
pSBRef <- MSE@SSB > RefPoint
apply(pSBRef,3:4, mean)



SBbase <- apply(MSE@SB_SBMSY, 3:4, mean)
SBrecent <- apply(MSE_recent_recdevs@SB_SBMSY, 3:4, mean)

plot(SBbase[1,], ylim=c(0, 2), type='l')
lines(SBrecent[1,], col='blue')


HistRemovals <- openMSE::get_Removals(Hist)
ProjRemovals <- MSE@Removals


Fmort <- openMSE::get_F(MSE) |> dplyr::filter(Sim==1)

ggplot(Fmort, aes(x=Year, y=Value, color=MP)) +
  facet_grid(~Fleet, scale='free') +
  geom_line()


Fmort |> filter(MP=='curE', Fleet=='rGN')
