library(SAMSE)

MOM_multiHist <- readRDS('Hist_Objects/MOM_BaseCase.hist')


## Rebuild MSEtool
## Write MP code for solving for TAC
##


MMSE <- ProjectMOM(MOM_multiHist, MPs='curE')

MMSE@Stocks %>% names()

MMSE@SB_SBMSY[1,,1,]

MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$BMSY[1]/1000

MOM_multiHist[[1]][[1]]@Ref$ByYear$F_SPR[1,,y]

x <- 1
y <- 70
M_at_Age <- MOM_multiHist[[1]][[1]]@AtAge$N.Mortality[x,,y]
maxage <- length(M_at_Age)-1
Wt_at_Age  <- MOM_multiHist[[1]][[1]]@AtAge$Weight[x,,y]
Mat_at_Age <- MOM_multiHist[[1]][[1]]@SampPars$Stock$Mat_age[x,,y]
Fec_at_Age <- MOM_multiHist[[1]][[1]]@SampPars$Stock$Fec_Age[x,,y]

F_at_age <- rep(0, maxage+1)
nfleets <- length(MOM_multiHist[[1]])
for (fl in 1:nfleets) {
  F_at_age <- F_at_age + rowSums(MOM_multiHist[[1]][[fl]]@AtAge$F.Mortality[x,,y,])
}

V_at_Age <- F_at_age/max(F_at_age)

R0x <- MOM_multiHist[[1]][[1]]@SampPars$Stock$R0[x]
hx <- MOM_multiHist[[1]][[1]]@SampPars$Stock$hs[x]
SSBpR <- MOM_multiHist[[1]][[1]]@SampPars$Stock$SSBpR[x,1]
SRrelx <- MOM_multiHist[[1]][[1]]@SampPars$Stock$SRrel[x]

Calc_Eq_SB <- function(logF) {
  ref <- MSEtool:::MSYCalcs(logF,
                     M_at_Age,
                     Wt_at_Age,
                     Mat_at_Age,
                     Fec_at_Age,
                     V_at_Age,
                     maxage,
                     relRfun=function() NULL,
                     SRRpars=list(),
                     R0x,
                     SRrelx,
                     hx,
                     SSBpR,
                     opt = 2,
                     plusgroup = 1L,
                     spawn_time_frac = 0)

  ref[3]
}

Fs <- tt$eq.series$F.eq
SB <- NULL
for (i in seq_along(Fs)) {
  SB[i] <- Calc_Eq_SB(log(Fs[i]))
}

tt = bamExtras::rdat_RedSnapper
plot(tt$eq.series$F.eq, tt$eq.series$SSB.eq, type='l')
lines(Fs, SB, col='blue')

which.min(abs(Fs-0.19699014))
SB[1971]/635426.40


tt$t.series %>% head()

plot(tt$eq.series$F.eq, tt$eq.series$spr.eq/max(tt$eq.series$spr.eq), type='l')
lines(MOM_multiHist[[1]][[1]]@Ref$ByYear$F_SPR[1,,90], seq(0.2, 0.6, by=0.05), col='blue')

which.min(abs(tt$eq.series$spr.eq/max(tt$eq.series$spr.eq)-0.30))
tt$eq.series$F.eq[2058]

tt$spr.brps



tt = rowSums(MOM_multiHist[[1]][[1]]@TSdata$SBiomass[1,,])/MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$SSBMSY[1]
plot(tt, type='l')
abline(h=1, lty=3)


RS_multiHist <- readRDS('Hist_Objects/RS_basecase.hist')






MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$FMSY[1]

MSEgraph::lb2kg(MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$MSY[1])/1000

MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$BMSY[1]/1000


RS_multiHist[[1]][[1]]@Ref$ReferencePoints$SSBMSY[1]

rowSums(RS_multiHist[[1]][[1]]@TSdata$SBiomass[1,,])
rowSums(MOM_multiHist[[1]][[1]]@TSdata$SBiomass[1,,])
