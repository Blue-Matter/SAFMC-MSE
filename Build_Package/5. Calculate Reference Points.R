library(SAMSE)

MOM_multiHist <- readRDS('Hist_Objects/MOM_BaseCase.hist')

# ---- Red Snapper -----

Calc_RS_Ref_Points <- function(x, y, multiHist, SPR_targ=0.3) {

  M_at_Age <- multiHist[[1]][[1]]@AtAge$N.Mortality[x,,y]
  maxage <- length(M_at_Age)-1
  Wt_at_Age  <- multiHist[[1]][[1]]@AtAge$Weight[x,,y]
  Mat_at_Age <- multiHist[[1]][[1]]@SampPars$Stock$Mat_age[x,,y]
  Fec_at_Age <- multiHist[[1]][[1]]@SampPars$Stock$Fec_Age[x,,y]

  # Selectivity (removals)
  F_at_age <- rep(0, maxage+1)
  nfleets <- length(multiHist[[1]])
  for (fl in 1:nfleets) {
    F_at_age <- F_at_age + rowSums(multiHist[[1]][[fl]]@AtAge$F.Mortality[x,,y,])
  }
  V_at_Age <- F_at_age/max(F_at_age)


  R0x <- multiHist[[1]][[1]]@SampPars$Stock$R0[x]
  hx <- multiHist[[1]][[1]]@SampPars$Stock$hs[x]
  SSBpR <- multiHist[[1]][[1]]@SampPars$Stock$SSBpR[x,1]
  SRrelx <- multiHist[[1]][[1]]@SampPars$Stock$SRrel[x]


  Fs <- seq(0,1,by=0.01)
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
                              SRrelx,
                              hx=h,
                              SSBpR,
                              opt = 2,
                              plusgroup = 1L,
                              spawn_time_frac = 0)

    if (opt==1) return(ref[3])
    return(ref[1])

  }

  SPR <- NULL
  SB <- NULL
  Removals <- NULL
  Retain <- NULL
  for (i in seq_along(Fs)) {
    SB[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx)
    Removals[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx, opt=2)
    SPR[i] <- Calc_Eq_SB(log(Fs[i]), R0=1, h=1,)

  }
  SPR <- SPR/max(SPR)

  data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)
}


y <- ncol(MOM_multiHist[[1]][[1]]@TSdata$Find)
SP_SPR_df <- Calc_RS_Ref_Points(1, y, MOM_multiHist)

SPR_targ <- 0.3
ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
pdf <- SP_SPR_df %>% tidyr::pivot_longer(., cols=4:5)

RS_Ref_df <- SP_SPR_df[ind,]
RS_Ref_df$MSST <- RS_Ref_df$SB*0.75


p1 <- ggplot(SP_SPR_df, aes(x=F, y=SB)) +
  geom_line(data=data.frame(x=RS_Ref_df$F, y=c(0, RS_Ref_df$SB)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,RS_Ref_df$F), y=RS_Ref_df$SB),
            aes(x=x, y=y),
            linetype=2) +
  geom_hline(data=data.frame(yintercept=RS_Ref_df$MSST, name='SB'),
             aes(yintercept=yintercept), linetype=3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Biomass (1E8 eggs)')

p1

p2 <- ggplot(SP_SPR_df, aes(x=F, y=SPR)) +
  geom_line(data=data.frame(x=RS_Ref_df$F, y=c(0, RS_Ref_df$SPR)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,RS_Ref_df$F), y=RS_Ref_df$SPR),
            aes(x=x, y=y),
            linetype=2) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Potential Ratio (SPR))')

p2

cowplot::plot_grid(p1,p2, labels=c('a)', 'b)'))

ggsave('img/RS_Ref_Points.png', width=6, height=2.5)

saveRDS(RS_Ref_df, 'Objects/RS_Ref_df.rda')


# ---- Gag Grouper ----


Calc_GG_Ref_Points <- function(x, y, multiHist) {

  M_at_Age <- multiHist[[2]][[1]]@AtAge$N.Mortality[x,,y]
  ind <- which(M_at_Age>0)
  M_at_Age <- M_at_Age[ind]
  maxage <- length(M_at_Age)-1
  Wt_at_Age  <- multiHist[[2]][[1]]@AtAge$Weight[x,ind,y]
  Mat_at_Age <- multiHist[[2]][[1]]@SampPars$Stock$Mat_age[x,ind,y]
  Fec_at_Age <- multiHist[[2]][[1]]@SampPars$Stock$Fec_Age[x,ind,y]

  # Selectivity (removals)
  F_at_age <- rep(0, maxage+1)
  nfleets <- length(multiHist[[2]])
  for (fl in 1:nfleets) {
    F_at_age <- F_at_age +   apply(multiHist[[2]][[fl]]@AtAge$F.Mortality[x,ind,(y-2):y,], 1, mean)
  }
  V_at_Age <- F_at_age/max(F_at_age)


  R0x <- multiHist[[2]][[1]]@SampPars$Stock$R0[x]
  hx <- multiHist[[2]][[1]]@SampPars$Stock$hs[x]
  SSBpR <- multiHist[[2]][[1]]@SampPars$Stock$SSBpR[x,1]
  SRrelx <- multiHist[[2]][[1]]@SampPars$Stock$SRrel[x]


  Fs <- seq(0,1,by=0.01)
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
                              SRrelx,
                              hx=h,
                              SSBpR,
                              opt = 2,
                              plusgroup = 1L,
                              spawn_time_frac = 0)

    if (opt==1) return(ref[3])
    return(ref[1])

  }

  SPR <- NULL
  SB <- NULL
  Removals <- NULL
  Retain <- NULL
  for (i in seq_along(Fs)) {
    SB[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx)
    Removals[i] <- Calc_Eq_SB(log(Fs[i]), R0=R0x, h=hx, opt=2)
    SPR[i] <- Calc_Eq_SB(log(Fs[i]), R0=1, h=1,)

  }
  SPR <- SPR/max(SPR)

  data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)
}

y <- ncol(MOM_multiHist[[2]][[1]]@TSdata$Find)
GG_SP_SPR_df <- Calc_GG_Ref_Points(1, y, MOM_multiHist)

ind <- which.max(GG_SP_SPR_df$Removals)
GG_Ref_df <- GG_SP_SPR_df[ind,]
GG_Ref_df$MSST <- GG_Ref_df$SB*0.75

GG_Ref_df

p1 <- ggplot(GG_SP_SPR_df, aes(x=F, y=Removals/1000)) +
  geom_line(data=data.frame(x=GG_Ref_df$F, y=c(0, GG_Ref_df$Removals/1000)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,GG_Ref_df$F), y=GG_Ref_df$Removals/1000),
            aes(x=x, y=y),
            linetype=2) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Removals (mt)')

p2 <- ggplot(GG_SP_SPR_df, aes(x=F, y=SB)) +
  geom_line(data=data.frame(x=GG_Ref_df$F, y=c(0, GG_Ref_df$SB)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,GG_Ref_df$F), y=GG_Ref_df$SB),
            aes(x=x, y=y),
            linetype=2) +
  geom_hline(data=data.frame(yintercept=GG_Ref_df$MSST),
             aes(yintercept=yintercept), linetype=3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Stock Biomass (mt)')

cowplot::plot_grid(p1,p2, labels=c('a)', 'b)'))

ggsave('img/GG_Ref_Points.png', width=6, height=2.5)

saveRDS(GG_Ref_df, 'Objects/GG_Ref_df.rda')


## SB and Reference Points ----
# Spawning Biomass

SSB <- get_SSB(MOM_multiHist) %>% filter(Sim==1)

ref <- RS_Ref_df %>% tidyr::pivot_longer(., col=c(SB, MSST))
p1 <- ggplot(SSB %>% filter(Stock=='Red Snapper'), aes(x=Year, y=Value/1000)) +
  geom_line() +
  labs(y='Spawning Stock (Eggs 1000)') +
  geom_hline(data=ref, aes(yintercept=value/1000, linetype=name)) +
  theme_bw() +
  labs(linetype='Reference Points')

ggsave('img/RS_SSB_ref_hist.png', p1, width=6, height=4)


ref <- GG_Ref_df %>% tidyr::pivot_longer(., col=c(SB, MSST))
p2 <- ggplot(SSB %>% filter(Stock=='Gag Grouper'), aes(x=Year, y=Value/1000)) +
  geom_line() +
  labs(y='Spawning Stock (1000)') +
  geom_hline(data=ref, aes(yintercept=value/1000, linetype=name)) +
  theme_bw() +
  labs(linetype='Reference Points')

ggsave('img/GG_SSB_ref_hist.png', p2, width=6, height=4)









