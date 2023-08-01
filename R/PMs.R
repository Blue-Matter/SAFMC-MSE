# Performance Metrics

#' Probability SB > MSST
#'
#' @param MMSE
#'
#' @return
#' @export
P_MSST <- function(MMSE=NULL) {
  Ref_Points <- Calculate_Ref_Points(MMSE@multiHist)

  PM <- 'P_MSST'
  caption <- 'Prob. SB > MSST'
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)
  # Red Snapper
  p1 <- apply(MMSE@SSB[,1,,]>=Ref_Points$MSST[Ref_Points$Stock=='Red Snapper'], 2, mean)


  # Gag Grouper
  p2 <- apply(MMSE@SSB[,2,,]>=Ref_Points$MSST[Ref_Points$Stock=='Gag Grouper'], 2, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))

}
class(P_MSST) <- 'PM'

#' Probability F < MFMT
#'
#' @param MMSE
#'
#' @return
#' @export
P_MFMT <- function(MMSE=NULL) {

  Ref_Points <- Calculate_Ref_Points(MMSE@multiHist)

  PM <- 'P_MFMT'
  caption <- 'Prob. F < MFMT'
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)
  # Red Snapper
  Ftot <- apply(MMSE@FM[,1,,,], c(1,3,4), sum) %>% round(2)
  p1 <- apply(Ftot<=Ref_Points$F[Ref_Points$Stock=='Red Snapper'], 2, mean)

  # Gag Grouper
  Ftot <- apply(MMSE@FM[,2,,,], c(1,3,4), sum) %>% round(2)
  p2 <- apply(Ftot<=Ref_Points$F[Ref_Points$Stock=='Gag Grouper'], 2, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))

}
class(P_MFMT) <- 'PM'

#' Mean Landings (t) in first 10 years
#'
#' @param MMSE
#'
#' @return
#' @export
Landings_10 <- function(MMSE) {

  PM <- 'Landings_10'
  caption <- 'Mean Landings (t) in first 10 years'
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  # Red Snapper
  p1 <- apply(MMSE@Catch[,1,,,1:10], 3, mean)

  # Gag Grouper
  p2 <- apply(MMSE@Catch[,2,,,1:10], 3, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))

}
class(Landings_10) <- 'PM'

#' Mean Landings (t) in last 10 years
#'
#' @param MMSE
#'
#' @return
#' @export
Landings_20 <- function(MMSE) {

  PM <- 'Landings_20'
  caption <- 'Mean Landings (t) in last 10 years'
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  # Red Snapper
  p1 <- apply(MMSE@Catch[,1,,,11:20], 3, mean)

  # Gag Grouper
  p2 <- apply(MMSE@Catch[,2,,,11:20], 3, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))

}
class(Landings_20) <- 'PM'

#' Probability of rebuilding to target level by end of projection period
#'
#' @param MMSE
#'
#' @return
#' @export
P_rebuild <- function(MMSE=NULL, SSBtarg=list('Red Snapper'=680555, 'Gag Grouper'=1772)) {

  Ref_Points <- Calculate_Ref_Points(MMSE@multiHist)

  Years <- get_Years(MMSE)

  PM <- 'P_rebuild'
  caption <- paste0('Prob. SB > SBtarg by ', max(Years$Year))

  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  # Red Snapper
  p1 <- apply(MMSE@SSB[,1,,MMSE@proyears]>=Ref_Points$SBtarg[Ref_Points$Stock=='Red Snapper'], 2, mean)


  # Gag Grouper
  p2 <- apply(MMSE@SSB[,2,,MMSE@proyears]>=Ref_Points$SBtarg[Ref_Points$Stock=='Gag Grouper'], 2, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))


}
class(P_rebuild) <- 'PM'

#' Mean ratio of landings to total removals
#'
#' @param MMSE
#'
#' @return
#' @export
Landings_Removals <- function(MMSE) {

  PM <- 'Landings_Removals'
  caption <- 'Mean ratio of landings:overall removals'
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  landing_fleets <- which(Fleet_Details$Season == 'On')
  discard_fleets <- which(Fleet_Details$Season == 'Off')

  # Red Snapper
  landings <- apply(MMSE@Catch[,1,landing_fleets,,], c(1,3), sum)
  removals <- apply(MMSE@Removals[,1,,,], c(1,3), sum)
  p1 <- apply(landings/removals, 2, mean)

  # Gag Grouper
  landings <- apply(MMSE@Catch[,2,landing_fleets,,], c(1,3), sum)
  removals <- apply(MMSE@Removals[,2,,,], c(1,3), sum)
  p2 <- apply(landings/removals, 2, mean)

  data.frame(PM=PM,
             Caption=caption,
             Stock=rep(c('Red Snapper', 'Gag Grouper'), each=nMPs),
             MP=MPs,
             Value=c(p1,p2))

}
class(Landings_Removals) <- 'PM'



# TBD
P_reasonable_size <- function(MMSE) {

}

P_trophy_size <- function(MMSE) {

}


#' Trade-Off Plot of Two Performance Metrics
#'
#' @param MMSE
#' @param PMs
#'
#' @return
#' @export
TradeOff <- function(MMSE, PMs=c('P_MSST', 'P_MFMT')) {
  if (length(PMs)!=2)
    stop('`PMs` must be a character vector of length 2')

  PM_funs <- lapply(PMs, get)
  if (!all(lapply(PM_funs, class) %>% unlist() =='PM'))
    stop('`PMs` must be names of `PM` functions')

  df_list <- list()
  for (i in 1:2) {
    df_list[[i]] <- PM_funs[[i]](MMSE)
  }

  df <- data.frame(x=df_list[[1]]$Value,
                   y=df_list[[2]]$Value,
                   MP=df_list[[1]]$MP,
                   Stock=df_list[[1]]$Stock,
                   xlab=unique(df_list[[1]]$Caption),
                   ylab=unique(df_list[[2]]$Caption))

  df$Stock <- factor(df$Stock, levels=MMSE@Snames, ordered = TRUE)

  ggplot(df, aes(x=x,y=y, color=MP)) +
    facet_wrap(~Stock, scales='free') +
    geom_point() +
    ggrepel::geom_text_repel(aes(label=MP)) +
    expand_limits(y=c(0,1), x=c(0,1)) +
    labs(x=df$xlab, y=df$ylab) +
    theme_bw() +
    guides(color='none')

}
