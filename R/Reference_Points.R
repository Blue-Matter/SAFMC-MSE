#' Calculate Biological Reference Points
#'
#' @param multiHist
#'
#' @return
#' @export
Calculate_Ref_Points <- function(multiHist) {

  # Red Snapper
  y <- ncol(multiHist[[1]][[1]]@TSdata$Find)
  SP_SPR_df <- Calc_RS_Ref_Points(1, y, multiHist)
  SPR_targ <- 0.3
  ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
  rsdf <- SP_SPR_df[ind,] %>% select(F, SPR, SBtarg=SB)
  rsdf$MSST <- rsdf$SB*0.75
  rsdf$Stock <- 'Red Snapper'

  # Gag Grouper
  y <- ncol(multiHist[[2]][[1]]@TSdata$Find)
  GG_SP_SPR_df <- Calc_GG_Ref_Points(1, y, multiHist)

  ind <- which.max(GG_SP_SPR_df$Removals)
  GG_Ref_df <- GG_SP_SPR_df[ind,]
  ggdf <- GG_Ref_df %>% select(F, SPR, SBtarg=SB)
  ggdf$MSST <- ggdf$SB*0.75
  ggdf$Stock <- "Gag Grouper"

  bind_rows(rsdf, ggdf)

}



#' Calculate Reference Points for Red Snapper
#'
#' @param x
#' @param y
#' @param multiHist
#'
#' @return
#' @export
#'
#' @examples
Calc_RS_Ref_Points <- function(x, y, multiHist) {

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


#' Calculate Reference Points for Gag Grouper
#'
#' @param x
#' @param y
#' @param multiHist
#'
#' @return
#' @export
#'
#' @examples
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
