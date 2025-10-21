
# pak::pkg_install('nikolaifish/bamExtras')
# pak::pkg_install('blue-matter/MSEtool')


library(MSEtool)
library(bamExtras)

rdat <- bamExtras::rdat_BlackSeaBass


M_at_Age <- rdat$a.series$M
Wt_at_Age <- rdat$a.series$weight
Mat_at_Age <- rdat$a.series$mat.female
Fec_at_Age <- rdat$a.series$reprod
V_at_Age <- rdat$sel.age$sel.v.wgted.tot
maxage <- max(rdat$a.series$age)
R0 <- rdat$eq.series$R.eq[1]


plot(rdat$eq.series$SSB.eq, rdat$eq.series$D.eq.klb, type='l')
plot(rdat$eq.series$SSB.eq, rdat$eq.series$L.eq.klb, type='l')

ind <- which.max(rdat$eq.series$L.eq.klb)
rdat$eq.series$SSB.eq[ind]
rdat$parms$SSBmsy
(1-0.375) *rdat$eq.series$SSB.eq[ind]
rdat$parms$msst



h <- 0.999 # mean recruitmentmodel
spawn_time_frac <- rdat$parms$spawn.time

N_unfished <- c(R0, R0 * exp(-cumsum(M_at_Age)))[1:(maxage+1)]
SSB_unfished <- Mat_at_Age * Fec_at_Age * N_unfished

SSBpR <- sum(SSB_unfished)/R0


Calc_Eq_SB <- function(logF, R0, h, opt=1) {
  ref <- MSEtool:::MSYCalcs(logF,
                            M_at_Age,
                            Wt_at_Age,
                            Mat_at_Age,
                            Fec_at_Age,
                            V_at_Age,
                            maxage,
                            relRfun=function() NULL,
                            SRRpars=list(),
                            R0x=R0,
                            SRrelx=1,
                            hx=h,
                            SSBpR,
                            opt = 2,
                            plusgroup = 1L,
                            spawn_time_frac = spawn_time_frac)

  if (opt==1) return(ref[3])
  return(ref[1])

}

Fs <- seq(0,3,by=0.01)
SPR <- NULL
SB <- NULL
Removals <- NULL
Retain <- NULL
for (i in seq_along(Fs)) {
  SB[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx)
  Removals[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx, opt=2)
  SPR[i] <- Calc_Eq_SB(log(Fs[i]), R0=1, h=1)
}

SPR <- SPR/max(SPR)

BS_SP_SPR_df <- data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)

par(mfrow=c(2,2))
rdat <- bamExtras::rdat_BlackSeaBass

plot(rdat$eq.series$F.eq, rdat$eq.series$spr.eq/max(rdat$eq.series$spr.eq), ylim=c(0,1), type='l',
     xlab='Apical Fishing Mortality',
     ylab='Spawning Potential Ratio')
lines(BS_SP_SPR_df$F, BS_SP_SPR_df$SPR, col='blue')


plot(rdat$eq.series$F.eq, rdat$eq.series$SSB.eq, type='l',
     xlab='Apical Fishing Mortality',
     ylab='Equilibrium Spawning Biomass')
lines(BS_SP_SPR_df$F, BS_SP_SPR_df$SB, col='blue')


plot(rdat$eq.series$SSB.eq, rdat$eq.series$D.eq.klb/max(rdat$eq.series$D.eq.klb), type='l',
     xlab='Equilibrium Spawning Biomass',
     ylab='Removals')
lines(BS_SP_SPR_df$SB, BS_SP_SPR_df$Removals/max( BS_SP_SPR_df$Removals), col='blue')

ind <- which.max(rdat$eq.series$D.eq.klb/max(rdat$eq.series$D.eq.klb))

(1-0.375) * rdat$eq.series$SSB.eq[ind]





plot(rdat$eq.series$F.eq, rdat$eq.series$L.eq.klb/max(rdat$eq.series$L.eq.klb), type='l')

plot(rdat$eq.series$SSB.eq, rdat$eq.series$L.eq.klb/max(rdat$eq.series$L.eq.klb), type='l')


rdat$parms$msst

ref_df <- readRDS('inst/ref_df.rda')

