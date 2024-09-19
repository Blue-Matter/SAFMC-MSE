

#' Update Recruitment Deviations in the projection years
#'
#' Sampling from a multivariate truncated normal distribution
#'
#' @param OMList A list of MOM objects with cpars populated
#' @param truncsd Integer. The number of standard deviations to truncate recruitment deviations
#' @param yrind Integer. Number of recent years to include. NULL includes all historical years in common with the stocks
#'
#' @return An updated A list of MOM objects with rec devs updated in cpars
#' @export
Generate_Correlated_Rec_Devs <- function(OMList, truncsd=2, yrind=NULL) {
  set.seed(OMList[[1]]@seed)
  nsim <- OMList[[1]]@nsim
  pyears <- OMList[[1]]@proyears
  nstock <- length(OMList)

  # Years
  df_list <- list()
  for (i in seq_along(OMList)) {
    nyears <- OMList[[i]]@Fleets[[1]][[1]]@nyears
    current_yr <- OMList[[i]]@Fleets[[1]][[1]]@CurrentYr
    hist_yrs <- rev(seq(current_yr, by=-1, length.out=nyears))

    df_list[[i]] <- data.frame(Stock=OMList[[i]]@Stocks[[1]]@Name,
                               Year=hist_yrs)
  }
  df <- do.call('rbind',df_list)
  yr_df <- df |> group_by(Stock) |> summarise(nyear=length(Year),
                                              min=min(Year),
                                              max=max(Year))

  common_years <- max(yr_df$min):min(yr_df$max)
  if (!is.null(yrind)) {
    l <- length(common_years)
    common_years <- common_years[(l-yrind+1):l]
  }

  # Get historical deviations for common years
  hist_dev_list <- list()
  for (i in seq_along(OMList)) {
    OM <- OMList[[i]]
    nyears <- OM@Fleets[[1]][[1]]@nyears
    maxage <- OM@Stocks[[1]]@maxage
    ind <- match(common_years, df_list[[i]]$Year)
    hist_dev_list[[i]] <- OM@cpars[[1]][[1]]$Perr_y[1,(maxage+ind)]
  }
  hist_devs <- do.call('cbind', hist_dev_list)
  lhist_devs <- log(hist_devs)
  covvar <- cov(lhist_devs)
  lower <- -truncsd*apply(lhist_devs, 2, sd)
  upper <- truncsd*apply(lhist_devs, 2, sd)

  mean <- apply(lhist_devs, 2, mean)
  sd <- apply(lhist_devs, 2, sd)

  acf <- apply(lhist_devs, 2, acf, plot=FALSE)
  AC <- rep(NA, length(OMList))
  for (i in 1:nstock) {
    AC[i] <- acf[[i]]$acf[2,1,1]
  }

  mu <- -0.5 * sd^2  * (1 - AC)/sqrt(1 - AC^2)
  if (!is.null(yrind)) {
    mu <- mean -0.5 * sd^2  * (1 - AC)/sqrt(1 - AC^2)
  }

  rldevs <- tmvtnorm::rtmvnorm(n=nsim*pyears,
                               mean=mu,
                               sigma=covvar,
                               lower=lower,
                               upper=upper)

  rldevs <- array(as.vector(rldevs), dim=c(pyears, nsim, nstock)) %>%
    aperm(., c(2,1,3))


  acrldevs <- rldevs
  # add auto-correlation
  for (i in 1:nstock) {
    ac <- OMList[[i]]@cpars[[1]][[1]]$AC[1]
    acrldevs[,1,i] <- ac * lhist_devs[nrow(lhist_devs),i] + acrldevs[,1,i] * sqrt(1-ac^2)
    for (y in 2:pyears) {
      acrldevs[,y,i] <- ac *acrldevs[,y-1,i] + acrldevs[,y,i]*sqrt(1-ac^2)
    }
  }

  # update OM
  for (i in 1:nstock) {
    nyears <- OMList[[i]]@Fleets[[1]][[1]]@nyears
    maxage <- OMList[[i]]@Stocks[[1]]@maxage
    OMList[[i]]@cpars[[1]][[1]]$Perr_y[,(nyears+maxage+1):(nyears+maxage+pyears)] <- exp(acrldevs[,,i])
  }
  OMList
}

#' @describeIn Generate_Correlated_Rec_Devs Scatter plot with marginal histograms
#' @param ncol Number of columns for the plot
#' @export
Plot_Correlated_Rec_Devs <- function(OMList, ncol=3, addtheme=NULL, ylim=NULL, alpha=0.5) {

  library(ggplot2)
  library(ggExtra)
  library(cowplot)

  stock_names <- NULL
  proj_dev_sim_list <- list()
  for (sim in 1:OMList[[1]]@nsim) {
    proj_dev_list <- list()
    for (i in seq_along(OMList)) {
      OM <- OMList[[i]]
      nyears <- OM@Fleets[[1]][[1]]@nyears
      maxage <- OM@Stocks[[1]]@maxage
      pyears <- OM@proyears
      proj_dev_list[[i]] <-  OM@cpars[[1]][[1]]$Perr_y[sim,(nyears+maxage+1):(nyears+maxage+pyears)]
      stock_names[i] <- OMList[[i]]@Stocks[[1]]@Name
    }
    proj_dev_sim_list[[sim]] <- do.call('cbind', proj_dev_list)
  }
  proj_devs <- do.call('rbind', proj_dev_sim_list) |> as.data.frame()
  colnames(proj_devs) <- stock_names


  n <- ncol(proj_devs)
  grid <- expand.grid(1:n, 1:n)
  grid <- grid[grid[,1] != grid[,2],]
  grid <- grid[!duplicated(apply(grid[,1:2], 1, function(row) paste(sort(row), collapse=""))),]

  lproj_devs <- log(proj_devs)
  if (is.null(ylim))
    ylim <- range(lproj_devs)
  plot_list <- list()

  for (i in 1:nrow(grid)) {
    tt <- as.numeric(grid[i,])
    df <- lproj_devs[,c(tt[2],tt[1])]
    p <- ggplot(df, aes(x=.data[[stock_names[tt[2]]]], y=.data[[stock_names[tt[1]]]])) +
      geom_point(alpha=alpha) +
      theme_bw() +
      theme(axis.title=element_text(size=14)) +
      expand_limits(x=ylim, y=ylim)
    if (!is.null(addtheme))
      p <- p + addtheme

    p <- ggMarginal(p, type="histogram")

    plot_list[[i]] <- p

  }

  cowplot::plot_grid(plotlist = plot_list, ncol=ncol)
}

