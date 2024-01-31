

#' Update Recruitment Deviations in the projection years
#'
#' Sampling from a multivariate truncated normal distribution
#'
#' @param MOM An MOM object with cpars populated
#' @param truncsd Integer. The number of standard deviations to truncate
#'
#' @return An updated MOM
#' @export
Generate_Future_Rec_Devs <- function(MOM, truncsd=2, sd_multi=NULL) {
  set.seed(MOM@seed)
  nsim <- MOM@nsim
  pyears <- MOM@proyears
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  maxage <- MOM@Stocks[[1]]@maxage

  nstock <- length(MOM@Stocks)
  hist_devs <- list()
  for (i in 1:nstock) {
    hist_devs[[i]] <- MOM@cpars[[i]][[1]]$Perr_y[1,1:(nyears+maxage)]
  }
  hist_devs <- do.call('cbind', hist_devs)
  ind <- round(hist_devs,3)!=1 # identify years where rec devs are fixed to 1
  ind <- as.logical(apply(ind, 1, prod))
  hist_devs <- hist_devs[ind,]
  lhist_devs <- log(hist_devs)

  covvar <- cov(lhist_devs)
  lower <- -truncsd*apply(lhist_devs, 2, sd)
  upper <- truncsd*apply(lhist_devs, 2, sd)

  if(!is.null(sd_multi)) {
    if (length(sd_multi)!=nstock)
      stop('sd_multi must be length nstock')

    sd <- sqrt(diag(covvar)) * sd_multi
    diag(covvar) <- sd^2
    lower <- -truncsd*sd
    upper <- truncsd*sd
  }

  rldevs <- tmvtnorm::rtmvnorm(n=nsim*pyears,
                               mean=rep(0, nstock),
                               sigma=covvar,
                               lower=lower,
                               upper=upper)



  rldevs <- array(as.vector(rldevs), dim=c(pyears, nsim, nstock)) %>%
    aperm(., c(2,1,3))


  acrldevs <- rldevs
  # add auto-correlation
  for (i in 1:nstock) {
    ac <- MOM@cpars[[i]][[1]]$AC[1]
    acrldevs[,1,i] <- ac * lhist_devs[nrow(lhist_devs),i] + acrldevs[,1,i] * sqrt(1-ac^2)
    for (y in 2:pyears) {
      acrldevs[,y,i] <- ac *acrldevs[,y-1,i] + acrldevs[,y,i]*sqrt(1-ac^2)
    }
  }

  # update OM
  for (i in 1:nstock) {
    MOM@cpars[[i]][[1]]$Perr_y[,(nyears+maxage+1):(nyears+maxage+pyears)] <- exp(acrldevs[,,i])
  }
  MOM
}
