#' Calculate Biological Reference Points from a single stock `multiHist` object
#'
#' Currently supports 'Red Snapper', 'Gag Grouper', and 'Black Seabass'
#'
#'
#' @param Hist An object of class `multiHist`
#'
#' @return
#' @export
Calculate_Ref_Points <- function(Hist) {

  stock <- names(Hist)[[1]]

  # Red Snapper
  if (stock == 'Red Snapper') {
    return(Calc_RS_Ref_Points(Hist))
  } else if (stock == 'Gag Grouper') {
    return(Calc_GG_Ref_Points(Hist))
  } else if (stock == 'Black Seabass') {
    return(Calc_BS_Ref_Points(Hist))
  } else {
    stop('Need to develop function to calculate reference points for ', stock)
  }




  # Black Seabass
  multiHist[[1]][[1]]@Ref$ReferencePoints$FMSY[1]


}



#' @describeIn Calculate_Ref_Points Calculate reference points for red snapper
Calc_RS_Ref_Points <- function(Hist) {
  x <- 1
  y <- ncol(Hist[[1]][[1]]@TSdata$Find)

  M_at_Age <- Hist[[1]][[1]]@AtAge$N.Mortality[x,,y]
  maxage <- length(M_at_Age)-1
  Wt_at_Age  <- Hist[[1]][[1]]@AtAge$Weight[x,,y]
  Mat_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Mat_age[x,,y]
  Fec_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Fec_Age[x,,y]

  # Selectivity (removals)
  F_at_age <- rep(0, maxage+1)
  F_at_age2 <- rep(0, maxage+1)
  nfleets <- length(Hist[[1]])
  for (fl in 1:nfleets) {
    F_at_age <- F_at_age + rowSums(Hist[[1]][[fl]]@AtAge$F.Mortality[x,,y,])
  }
  V_at_Age <- F_at_age/max(F_at_age)

  R0x <- Hist[[1]][[1]]@SampPars$Stock$R0[x]
  hx <- Hist[[1]][[1]]@SampPars$Stock$hs[x]
  SSBpR <- Hist[[1]][[1]]@SampPars$Stock$SSBpR[x,1]
  SRrelx <- Hist[[1]][[1]]@SampPars$Stock$SRrel[x]

  spawn_time_frac <- Hist[[1]][[1]]@SampPars$Stock$spawn_time_frac[x]
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
                              spawn_time_frac = spawn_time_frac)

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

  SP_SPR_df <- data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)

  SPR_targ <- 0.3
  ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
  rsdf <- SP_SPR_df[ind,] %>% select(F, SPR, SBtarg=SB)
  rsdf$MSST <- rsdf$SB*0.75
  rsdf$MFMT <- rsdf$F
  rsdf$Stock <- 'Red Snapper'
  rsdf
}


#' @describeIn Calculate_Ref_Points Calculate reference points for gag grouper
Calc_GG_Ref_Points <- function(Hist) {
  x <- 1
  y <- ncol(Hist[[1]][[1]]@TSdata$Find)

  M_at_Age <- Hist[[1]][[1]]@AtAge$N.Mortality[x,,y]
  ind <- which(M_at_Age>0)
  M_at_Age <- M_at_Age[ind]
  maxage <- length(M_at_Age)-1
  Wt_at_Age  <- Hist[[1]][[1]]@AtAge$Weight[x,ind,y]
  Mat_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Mat_age[x,ind,y]
  Fec_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Fec_Age[x,ind,y]

  # Selectivity (removals)
  F_at_age <- rep(0, maxage+1)
  nfleets <- length(Hist[[1]])
  for (fl in 1:nfleets) {
    F_at_age <- F_at_age + apply(Hist[[1]][[fl]]@AtAge$F.Mortality[x,ind,(y-2):y,], 1, mean)
  }
  V_at_Age <- F_at_age/max(F_at_age)


  R0x <- Hist[[1]][[1]]@SampPars$Stock$R0[x]
  hx <- Hist[[1]][[1]]@SampPars$Stock$hs[x]
  SSBpR <- Hist[[1]][[1]]@SampPars$Stock$SSBpR[x,1]
  SRrelx <- Hist[[1]][[1]]@SampPars$Stock$SRrel[x]
  spawn_time_frac <- Hist[[1]][[1]]@SampPars$Stock$spawn_time_frac[x]
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
                              spawn_time_frac = spawn_time_frac)

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

  GG_SP_SPR_df <- data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)

  ind <- which.max(GG_SP_SPR_df$Removals)
  GG_Ref_df <- GG_SP_SPR_df[ind,]
  ggdf <- GG_Ref_df %>% select(F, SPR, SBtarg=SB)
  ggdf$MFMT <- GG_Ref_df$F
  ggdf$MSST <- ggdf$SB*0.75
  ggdf$Stock <- "Gag Grouper"
  ggdf
}

#' @describeIn Calculate_Ref_Points Calculate reference points for black seabass
Calc_BS_Ref_Points <- function(Hist) {
  x <- 1
  y <- ncol(Hist[[1]][[1]]@TSdata$Find)


  M_at_Age <- Hist[[1]][[1]]@AtAge$N.Mortality[x,,y]
  ind <- which(M_at_Age>0)
  M_at_Age <- M_at_Age[ind]
  maxage <- length(M_at_Age)-1
  Wt_at_Age  <- Hist[[1]][[1]]@AtAge$Weight[x,ind,y]
  Mat_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Mat_age[x,ind,y]
  Fec_at_Age <- Hist[[1]][[1]]@SampPars$Stock$Fec_Age[x,ind,y]

  # Selectivity (removals)
  F_at_age <- rep(0, maxage+1)
  nfleets <- length(Hist[[1]])
  for (fl in 1:nfleets) {
    F_at_age <- F_at_age + apply(Hist[[1]][[fl]]@AtAge$F.Mortality[x,ind,(y-2):y,], 1, mean)
  }
  V_at_Age <- F_at_age/max(F_at_age)

  R0x <- Hist[[1]][[1]]@SampPars$Stock$R0[x]
  hx <-  Hist[[1]][[1]]@SampPars$Stock$hs[x]
  SSBpR <- Hist[[1]][[1]]@SampPars$Stock$SSBpR[x,1]
  SRrelx <- Hist[[1]][[1]]@SampPars$Stock$SRrel[x]
  spawn_time_frac <- Hist[[1]][[1]]@SampPars$Stock$spawn_time_frac[x]

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
                              spawn_time_frac = spawn_time_frac)

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
    SPR[i] <- Calc_Eq_SB(log(Fs[i]), R0=1, h=1)

  }
  SPR <- SPR/max(SPR)

  BS_SP_SPR_df <- data.frame(Sim=x, Year=y, F=Fs, SPR=SPR, SB=SB, Removals=Removals)

  # par(mfrow=c(2,2))
  #
  # plot(rdat$eq.series$F.eq, rdat$eq.series$spr.eq/max(rdat$eq.series$spr.eq), ylim=c(0,1), type='l')
  # lines(BS_SP_SPR_df$F, BS_SP_SPR_df$SPR, col='blue')
  #
  # plot(rdat$eq.series$F.eq, rdat$eq.series$SSB.eq, type='l')
  # lines(BS_SP_SPR_df$F, BS_SP_SPR_df$SB, col='blue')
  #
  # plot(rdat$eq.series$F.eq, rdat$eq.series$L.eq.klb/max(rdat$eq.series$L.eq.klb), type='l')
  # lines(rdat$eq.series$F.eq, rdat$eq.series$D.eq.klb/max(rdat$eq.series$D.eq.klb), col='red')
  # lines(BS_SP_SPR_df$F, BS_SP_SPR_df$Removals/max(BS_SP_SPR_df$Removals), col='blue')


  ind <- which.max(BS_SP_SPR_df$Removals)
  BS_Ref_df <- BS_SP_SPR_df[ind,]
  bsdf <- BS_Ref_df |> dplyr::select(F, SPR, SBtarg=SB)
  bsdf$MFMT <- BS_Ref_df$F
  M <- mean(Hist[[1]][[1]]@AtAge$N.Mortality[1,4:12,y])
  bsdf$MSST <- (1-M) * bsdf$SB
  bsdf$Stock <- "Black Seabass"
  bsdf

}
