#' Create data.frames of MSE results and save to disk
#'
#' @param om OM name
#' @param MSE_dir
#'
#' @return Nothing. Saves data.frames to 'Results_Objects'
#' @export
Process_Results <- function(om='BaseCase', MSE_dir='MSE_Objects') {

  MSEtool:::message_info('Processing', om)

  MSE_files <- list.files(MSE_dir)
  MSE_files <- MSE_files[grepl(paste0(om, '_'), MSE_files)]
  OM_fls <- lapply(strsplit(MSE_files, paste0(om, '_')), '[[', 2) |> unlist()
  stocks <- lapply(strsplit(OM_fls, '_'), '[[', 1) |> unlist() |> unique()

  if (!dir.exists('Results_Objects'))
    dir.create('Results_Objects')

  st_list <- list()
  for (st in seq_along(stocks)) {
    stock_code <- stocks[st]
    stock <- switch(stock_code,
                    'RS'="Red snapper",
                    "GG"="Gag grouper",
                    'BS'="Black sea bass"
    )
    MSEtool:::message_info('Processing', stock, paste0(st, '/', length(stocks)))

    stock_mse_files <- MSE_files[grepl(paste(om, stock_code, sep='_'), MSE_files)]
    MP_text <- lapply(strsplit(stock_mse_files, paste0(paste(om, stock_code, sep='_'), '_')), '[[', 2) |>
      unlist()
    MP_names <- unlist(strsplit(MP_text, '.mmse'))

    Ref_DF <- readRDS('Misc_Objects/ref_df.rda')
    Ref_DF$OM <- gsub(' ', '',Ref_DF$OM)
    if (om %in% Ref_DF$OM) {
      Ref_DF <- Ref_DF|> dplyr::filter(Stock==stock, OM==om)
    } else {
      Ref_DF <- Ref_DF|> dplyr::filter(Stock==stock, OM=='BaseCase')
    }

    mp_list <- list()
    for (i in seq_along(stock_mse_files)) {
      MSEtool:::message_info(stock_mse_files[i], paste0(i, '/', length(stock_mse_files)))
      MSE <- readRDS(file.path(MSE_dir, stock_mse_files[i]))
      project_yrs <- seq(MSE@multiHist[[1]][[1]]@OMPars$CurrentYr[1]+1, by=1, length.out=MSE@proyears)

      MP_name <- MP_names[i]
      MP_name <- gsub('_REC1', '', MP_name)
      MP_name <- gsub('_EC', '', MP_name)
      rec_reduction <- lapply(strsplit(MSE@MPs[[1]], paste0(MP_name,'_')), '[[', 2) |> unlist() |>
        as.numeric()

      mps <- MSE@MPs[[1]]
      nMPs <- length(mps)

      # SSB ----
      df <- data.frame(OM=om,
                       Stock=stock,
                       Sim=1:MSE@nsim,
                       Year=rep(project_yrs, each=MSE@nsim*nMPs),
                       MP=rep(mps, each=MSE@nsim),
                       SSB=as.vector(MSE@SSB[,1,,]),
                       MSST=Ref_DF$MSST)

      df$MP_Name <- MP_name
      df$Rec_Reduction <- rep(rec_reduction, each=MSE@nsim)

      nm <- paste0(paste(om, stock_code, MP_name, sep='_'), '.rda')
      if (!dir.exists('Results_Objects/SSB'))
        dir.create('Results_Objects/SSB')
      saveRDS(df, file.path('Results_Objects/SSB', nm))

      # Apical F,  Landings and Discards ----
      nfleet <- length(MSE@Fleets[[1]])
      fleet_names <- MSE@Fnames[,1]

      df <- data.frame(OM=om,
                       Stock=stock,
                       Sim=1:MSE@nsim,
                       Fleet=rep(fleet_names, each=MSE@nsim),
                       MP=rep(mps, each=MSE@nsim*nfleet),
                       Year=rep(project_yrs, each=MSE@nsim*nMPs*nfleet),
                       F=as.vector(MSE@FM[,1,,,]),
                       Landings=as.vector(MSE@Catch[,1,,,]),
                       Discards=as.vector(MSE@Removals[,1,,,] -MSE@Catch[,1,,,]),
                       MFMT=Ref_DF$MFMT)

      df$MP_Name <- MP_name
      df$Rec_Reduction <- rep(rec_reduction, each=MSE@nsim* nfleet)
      if (!dir.exists('Results_Objects/F_Landing_Discards'))
        dir.create('Results_Objects/F_Landing_Discards')
      saveRDS(df, file.path('Results_Objects/F_Landing_Discards', nm))


      # # Prob trophy fish ----
      # trophy_size_inch <- switch(stock_code,
      #                 'RS'=28,
      #                 "GG"=40,
      #                 'BS'=18
      # )
      # trophy_size_mm <- openMSE::inch2mm(trophy_size_inch)
      #
      # nsamp <- 5000
      # for (s in 1:MSE@nsim) {
      #   for (mm in 1:nMPs) {
      #     lens <- list()
      #     for (fl in 1:4) {
      #       yrind <- (MSE@nyears+1):(MSE@nyears+MSE@proyears-1)
      #
      #       N <- apply(MSE@Misc$extended$N[s,1,,fl, yrind,], 1:2, sum) |> t()
      #       V <- MSE@multiHist[[1]][[1]]@SampPars$Fleet$V[s,,yrind] |> t()
      #       vn <- N *V
      #
      #       relN <- apply(vn, 1, sum)
      #
      #
      #       nyear <- dim(vn)[1]
      #       Linf <- rep(MSE@multiHist[[1]][[1]]@OMPars$Linf[1], nyear)
      #       K <- rep(MSE@multiHist[[1]][[1]]@OMPars$K[1], nyear)
      #       t0 <- rep(MSE@multiHist[[1]][[1]]@OMPars$t0[1], nyear)
      #       LenCV <- MSE@multiHist[[1]][[1]]@SampPars$Stock$LenCV[1]
      #       CAL_binsmid <- MSE@multiHist[[1]][[1]]@SampPars$Stock$CAL_binsmid
      #       CAL_bins <- MSE@multiHist[[1]][[1]]@SampPars$Stock$CAL_bins
      #
      #       openMSE::kg2lb(MSE@multiHist[[1]][[1]]@SampPars$Stock$Wt_age[1,,1])
      #
      #       retLa <- MSE@multiHist[[1]][[fl]]@SampPars$Fleet$retL[s,,yrind]
      #
      #       if (grepl('_MLL_', MP_name)) {
      #         mll <- rep(1, length(CAL_binsmid))
      #         mll[CAL_binsmid<get_MLL(stock)] <- 0
      #         retLa <- retLa * mll
      #       }
      #
      #       lencom <- MSEtool:::genSizeComp(vn,
      #                                       CAL_binsmid,
      #                                       CAL_bins,
      #                                       retLa,
      #                                       CAL_ESS=nsamp,
      #                                       CAL_nsamp=nsamp,
      #                                       Linfs=Linf,
      #                                       Ks=K,
      #                                       t0s=t0,
      #                                       LenCV=LenCV,
      #                                       truncSD=2)
      #
      #       for (y in 1:nyear) {
      #         lencom[y,] <- lencom[y,]/sum(lencom[y,]) * relN[y]
      #       }
      #
      #
      #       lens[[fl]] <- lencom
      #     }
      #     lenfleet <- Reduce('+', lens)
      #     ind <- CAL_binsmid>=trophy_size_mm
      #
      #
      #     sum(lenfleet[1,ind])/sum(lenfleet[1,])
      #
      #
      #
      #
      #
      #
      #
      #   }
      # }














    } # end MP loop
  } # end stock loop

}



get_rebuild_year <- function(stock_code) {
  if (stock_code== 'RS') {
    return(2044)
  } else if (stock_code== 'GG') {
    return(2032)
  } else if (stock_code== 'BS') {
    return(2032)
  } else {
    stop('No rebuilding target specified for ', stock_code)
  }

}


#' PM Plotting Functions
#'
#' @param om
#' @param results_dir
#'
#' @return
#' @export
Plot_Prob_Rebuild <- function(om='BaseCase', results_dir='Results_Objects', addtheme=NULL) {

  Results_files <- list.files(file.path(results_dir, 'SSB'))


  ####
  L <- strsplit(Results_files, paste0(om, '_'))
  ind <- which(unlist(lapply(L, length)) == 2)


  OM_fls <- lapply(strsplit(Results_files[ind], paste0(om, '_')), '[[', 2) |> unlist()
  stocks <- lapply(strsplit(OM_fls, '_'), '[[', 1) |> unlist() |> unique()

  st_list <- list()
  for (st in seq_along(stocks)) {
    stock_code <- stocks[st]
    stock <- switch(stock_code,
                    'RS'="Red snapper",
                    "GG"="Gag grouper",
                    'BS'="Black sea bass"
    )

    fls <- Results_files[grepl(paste0(paste(om, stock_code, sep='_'), "_"), Results_files)]

    df_list <- list()
    for (i in seq_along(fls)) {
      df_list[[i]] <- readRDS(file.path(results_dir, 'SSB', fls[i]))
    }
    df <- do.call('rbind', df_list) |> dplyr::filter(Year==get_rebuild_year(stock_code))
    df$Rebuilt <- df$SSB>df$MSST
    st_list[[st]] <- df
  }
  DF <- do.call('rbind', st_list)

  DF <- DF |> dplyr::group_by(Stock, MP_Name, Rec_Reduction) |>
    dplyr::summarise(Prob=mean(Rebuilt), .groups='drop')

  DF$Rec_Reduction <- 1-DF$Rec_Reduction

  DF$Rec_Reduction <- factor(DF$Rec_Reduction, ordered = TRUE,
                             levels=unique(DF$Rec_Reduction))
  DF$MP_Name <- factor(DF$MP_Name, levels=MP_levels(), ordered = TRUE)
  DF$Stock <- factor(DF$Stock, levels=Stock_levels(), ordered = TRUE)

  p <- ggplot2::ggplot(DF, aes(x=Rec_Reduction, y=MP_Name)) +
    facet_grid(~Stock) +
    geom_tile(aes(fill=Prob)) +
    geom_text(aes(label = round(Prob, 2)), size=2) +
    scale_fill_gradient2(low = "red", mid='white', high = "green", midpoint=0.5) +
    theme_bw() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0)) +
    labs(x='Relative Effort General Recreational Fleet',
         y='Management Actions',
         fill='Probability')

  if (!is.null(addtheme))
    p <- p + addtheme

  list(plot=p, DF=DF)
}


organizeDF <- function(df) {
  df$Rec_Reduction <- 1-df$Rec_Reduction
  df$Rec_Reduction <- factor(df$Rec_Reduction, ordered = TRUE,
                             levels=unique(df$Rec_Reduction))
  df$MP_Name <- factor(df$MP_Name, levels=MP_levels())
  df
}

#' @describeIn Plot_Prob_Rebuild Plots of other performance metrics
#' @export
PM_plots <- function(om = 'BaseCase', results_dir='Results_Objects', textsize=2) {

  stock_codes <- c('RS', 'GG', 'BS')
  DF_List <- list()
  for (st in seq_along(stock_codes)) {
    stock_code <- stock_codes[st]

    stock <- switch(stock_code,
                    'RS'="Red snapper",
                    "GG"="Gag grouper",
                    'BS'="Black sea bass"
    )

    # STY & LTY
    Results_files <- list.files(file.path(results_dir, 'F_Landing_Discards'))
    L <- strsplit(Results_files, paste0(om, '_'))
    ind <- which(unlist(lapply(L, length)) == 2)
    Results_files <- Results_files[ind][grepl(paste0('_', stock_code, '_'), Results_files[ind])]

    df_list <- list()
    for (i in seq_along(Results_files)) {
      df_list[[i]] <- readRDS(file.path(results_dir, 'F_Landing_Discards', Results_files[i]))
    }
    df <- do.call('rbind', df_list)

    df <- df |>
      dplyr::group_by(Stock, Sim, MP_Name, Year, Rec_Reduction) |>
      dplyr::summarise(Landings=sum(Landings), Discards=sum(Discards))

    if (om =='EC') {
      hist_file <- paste0(paste0('BaseCase', '_', stock_code), '.hist')
    } else {
      hist_file <- paste0(paste0(om, '_', stock_code), '.hist')
    }
    hist <- readRDS(file.path('Hist_Objects', hist_file))

    # STY - 2025:2029
    Landings <- openMSE::get_Landings(hist) |> dplyr::filter(Sim==1) |>
      dplyr::group_by(Year) |> summarise(Landings=sum(Value))

    Removals <- openMSE::get_Removals(hist) |> dplyr::filter(Sim==1) |>
      dplyr::group_by(Year) |> summarise(Removals=sum(Value))
    Landings$Discards <- Removals$Removals

    lastyr <- max(Landings$Year)
    Landings <- Landings |> dplyr::filter(Year%in%(lastyr-2):lastyr) |>
      summarise(Landings=mean(Landings), Discards=mean(Discards))

    df$relLandings <- df$Landings/Landings$Landings
    df$relDiscards <- df$Discards/Landings$Discards

    STY <- df |> dplyr::filter(Year%in%2025:2029) |>
      group_by(MP_Name, Rec_Reduction) |>
      summarize(Median=median(relLandings))

    STY <- organizeDF(STY)
    STY$Stock <- stock
    STY$PM <- 'Relative Short-Term Landings'

    LTY <- df |> dplyr::filter(Year%in%2035:2039) |>
      group_by(MP_Name, Rec_Reduction) |>
      summarize(Median=median(relLandings))

    LTY <- organizeDF(LTY)
    LTY$Stock <- stock
    LTY$PM <- 'Relative Long-Term Landings'

    # % Discard
    fracDiscard <- df |> group_by(MP_Name, Rec_Reduction) |>
      summarize(Median=median(Discards/(Landings+Discards)))

    fracDiscard <- organizeDF(fracDiscard)
    fracDiscard$Stock <- stock
    fracDiscard$PM <- 'Fraction Discarded'

    DF_List[[st]] <- dplyr::bind_rows(STY, LTY, fracDiscard)
  }

  DF <- do.call('rbind', DF_List)
  DF$Stock <- factor(DF$Stock, levels=Stock_levels(), ordered = TRUE)
  DF$MP_Name <- factor(DF$MP_Name, levels=MP_levels(), ordered = TRUE)

  make_PM_plot <- function(DF, stock='Red snapper', pm='Relative Short-Term Landings',
                           cols=c('green', 'white', 'red'),
                           vals=NULL,
                           xlab='Gen. Rec. Rel. Effort',
                           ylab='') {

    df <- DF |> dplyr::filter(PM==pm,
                              Stock==stock)
    if (is.null(vals))
      vals <- c(1, 1/max(df$Median), 0)
    ggplot2::ggplot(df,
                    aes(x=Rec_Reduction, y=MP_Name)) +
      facet_grid(PM~Stock) +
      geom_tile(aes(fill=Median)) +
      geom_text(aes(label = round(Median, 2)), size=textsize) +
      scale_fill_gradientn(colors=cols, values=vals) +
      theme_bw() +
      scale_x_discrete(expand=c(0,0))+
      scale_y_discrete(expand=c(0,0)) +
      labs(x=xlab,
           y=ylab) +
      guides(fill='none')
  }


  sty_RS <- make_PM_plot(DF)
  sty_GG <- make_PM_plot(DF, 'Gag grouper')
  sty_BS <- make_PM_plot(DF, 'Black sea bass')

  lty_RS <- make_PM_plot(DF, pm='Relative Long-Term Landings')
  lty_GG <- make_PM_plot(DF, 'Gag grouper', pm='Relative Long-Term Landings')
  lty_BS <- make_PM_plot(DF, 'Black sea bass', pm='Relative Long-Term Landings')

  disc_RS <- make_PM_plot(DF, pm='Fraction Discarded', cols=c('red', 'white', 'green'))
  disc_GG <- make_PM_plot(DF, 'Gag grouper', pm='Fraction Discarded', cols=c('red', 'white', 'green'))
  disc_BS <- make_PM_plot(DF, 'Black sea bass', pm='Fraction Discarded', cols=c('red', 'white', 'green'))


  strip_size <- 10
  title_size <- 8
  text_size <- 6

  sty <- sty_RS + theme(strip.background = element_blank(),
                 strip.text.y = element_blank(),
                 axis.title=element_blank(),
                 axis.text.x=element_blank(),
                 strip.text=element_text(size=strip_size),
                 # axis.title.y=element_text(size=title_size),
                 axis.text.y=element_text(size=text_size)) +
    sty_GG +theme(strip.background = element_blank(),
                  strip.text.y = element_blank(),
                  strip.text=element_text(size=strip_size),
                  axis.title=element_blank(),
                  axis.text=element_blank()) +
    sty_BS +theme(axis.title=element_blank(),
                  strip.background = element_blank(),
                  strip.text=element_text(size=strip_size),
                  axis.text=element_blank())


  lty <- lty_RS + theme(strip.background = element_blank(),
                         strip.text = element_blank(),
                         axis.title=element_blank(),
                         axis.text.x=element_blank(),
                         # axis.title.y=element_text(size=title_size),
                         axis.text.y=element_text(size=text_size)) +
    lty_GG + theme(strip.background = element_blank(),
                   strip.text = element_blank(),
                   axis.title=element_blank(),
                   axis.text=element_blank()) +
    lty_BS + theme(strip.background = element_blank(),
                   strip.text.x=element_blank(),
                   strip.text.y=element_text(size=strip_size),
                   axis.title=element_blank(),
                   axis.text=element_blank())

  disc <- disc_RS +theme(strip.background = element_blank(),
                         strip.text = element_blank(),
                         axis.title.y=element_blank(),
                         axis.title.x=element_text(size=title_size),
                         axis.text=element_text(size=text_size)) +
    disc_GG + theme(strip.background = element_blank(),
                    strip.text = element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.title.x=element_text(size=title_size),
                    axis.text.x=element_text(size=text_size)) +
    disc_BS + theme(strip.background = element_blank(),
                    strip.text.x=element_blank(),
                    strip.text.y=element_text(size=strip_size),
                    axis.title=element_blank(),
                    axis.text=element_blank(),
                    axis.title.x=element_text(size=title_size),
                    axis.text.x=element_text(size=text_size))



  p <- sty / lty / disc

  out <- list(plot=p, DF=DF)
  out
}

#' @describeIn Plot_Prob_Rebuild SSB/MSST time-series plot
#' @export
SSB_plot <- function(om='BaseCase', stock_code='RS', results_dir='Results_Objects') {
  Results_files <- list.files(file.path(results_dir, 'SSB'))
  L <- strsplit(Results_files, paste0(om, '_'))
  ind <- which(unlist(lapply(L, length)) == 2)
  Results_files <- Results_files[ind][grepl(paste0('_', stock_code, '_'), Results_files[ind])]

  df_list <- list()
  for (i in seq_along(Results_files)) {
    df_list[[i]] <- readRDS(file.path(results_dir, 'SSB', Results_files[i]))
  }

  DF <- do.call('rbind', df_list) |> dplyr::group_by(Year, MP_Name, Rec_Reduction) |>
    summarise(Median=median(SSB/MSST),
              Upper=quantile(SSB/MSST, 0.95),
              Lower=quantile(SSB/MSST, 0.05),
              MSST=unique(MSST))

  if (om =='EC') {
    hist_file <- paste0(paste0('BaseCase', '_', stock_code), '.hist')
  } else {
    hist_file <- paste0(paste0(om, '_', stock_code), '.hist')
  }
  hist <- readRDS(file.path('Hist_Objects', hist_file))

  histSB <- get_SSB(hist) |> dplyr::filter(Sim==1)
  histSB$Value <- histSB$Value/unique(DF$MSST)

  DF <- organizeDF(DF)

  levels <- levels(DF$Rec_Reduction)
  levels <- levels[seq(1, by=2, length.out=length(levels)/2)]
  DF <- DF |> dplyr::filter(Rec_Reduction %in% levels)

  stock <- switch(stock_code,
                  'RS'="Red snapper",
                  "GG"="Gag grouper",
                  'BS'="Black sea bass"
  )


  cols <- RColorBrewer::brewer.pal(length(levels), 'Dark2')

  label <- 'Gen. Rec. Effort'
  ggplot(DF) +
    geom_line(data=DF, aes(x=Year, y=Median, linetype = Rec_Reduction,
                           color=Rec_Reduction)) +
    facet_wrap(~MP_Name) +
    # geom_line(data=histSB, aes(x=Year, y=Value)) +
    geom_hline(yintercept = 1, linetype=2, color='darkgray') +
    expand_limits(y=0) +
    theme_bw() +
    labs(x='Projection Year',
         y='SB/MSST (median)',
         linetype=label,
         color=label,
         title=stock) +
    scale_color_manual(values=cols) +
    theme(legend.key.width = unit(2, "line"))


}

Landings_plot <- function(om='BaseCase', stock_code='RS', results_dir='Results_Objects', byfleet=FALSE) {

  Results_files <- list.files(file.path(results_dir, 'F_Landing_Discards'))
  L <- strsplit(Results_files, paste0(om, '_'))
  ind <- which(unlist(lapply(L, length)) == 2)
  Results_files <- Results_files[ind][grepl(paste0('_', stock_code, '_'), Results_files[ind])]

  df_list <- list()
  for (i in seq_along(Results_files)) {
    df_list[[i]] <- readRDS(file.path(results_dir, 'F_Landing_Discards', Results_files[i]))
  }

  DF <- do.call('rbind', df_list)

  if (byfleet) {

  } else {
    DF <- DF |> dplyr::group_by(Year, MP_Name, Sim, Rec_Reduction) |>
      summarise(Landings=sum(Landings), Discards=sum(Discards)) |>
      dplyr::group_by(Year, MP_Name, Rec_Reduction) |>
      summarise(Landings=median(Landings), Discards=median(Discards))
  }


  if (om =='EC') {
    hist_file <- paste0(paste0('BaseCase', '_', stock_code), '.hist')
  } else {
    hist_file <- paste0(paste0(om, '_', stock_code), '.hist')
  }
  hist <- readRDS(file.path('Hist_Objects', hist_file))

  Landings <- openMSE::get_Landings(hist) |> dplyr::filter(Sim==1) |>
    dplyr::group_by(Year) |> summarise(Landings=sum(Value))

  Removals <- openMSE::get_Removals(hist) |> dplyr::filter(Sim==1) |>
    dplyr::group_by(Year) |> summarise(Removals=sum(Value))
  Landings$Discards <- Removals$Removals

  lastyr <- max(Landings$Year)
  Landings <- Landings |> dplyr::filter(Year%in%(lastyr-2):lastyr) |>
    summarise(Landings=mean(Landings), Discards=mean(Discards))


  DF <- organizeDF(DF)
  DF$relLandings <- DF$Landings/Landings$Landings
  DF$relDiscards <- DF$Discards/Landings$Discards

  levels <- levels(DF$Rec_Reduction)
  levels <- levels[seq(1, by=2, length.out=length(levels)/2)]
  DF <- DF |> dplyr::filter(Rec_Reduction %in% levels)

  stock <- switch(stock_code,
                  'RS'="Red snapper",
                  "GG"="Gag grouper",
                  'BS'="Black sea bass"
  )


  cols <- RColorBrewer::brewer.pal(length(levels), 'Dark2')

  label <- 'Gen. Rec. Effort'
  pDF <- DF |> dplyr::filter(Year>=2025) |> tidyr::pivot_longer(cols=c(relLandings, relDiscards))

  ggplot(pDF) +
    geom_line(aes(x=Year, y=value,
                  linetype = name,
                  color=name)) +
    facet_grid(MP_Name~Rec_Reduction) +
    geom_hline(yintercept = 1, linetype=2, color='darkgray') +
    expand_limits(y=0) +
    theme_bw() +
    labs(x='Projection Year',
         y='SB/MSST (median)',
         linetype=label,
         color=label,
         title=stock) +
    scale_color_manual(values=cols) +
    theme(legend.key.width = unit(2, "line"))


}


MP_levels <- function() {
  c('SQ', 'SQ_FR', 'SQ_MLL',  'SQ_NS', 'SQ_OS',
    'SQ_FR_MLL',  'SQ_FR_NS', 'SQ_FR_OS',
    'SQ_MLL_NS', 'SQ_MLL_OS',
    'SQ_FR_MLL_NS', 'SQ_FR_MLL_OS'
  )
}

Stock_levels <- function() {
  c('Red snapper', 'Gag grouper', 'Black sea bass')
}


#' @export
Rebuild_Table <- function(df, om='BaseCase', rec_reduction=c(1, 0.45), MinProb=0.5) {

  make_table <- function(df, rec_reduction=1, MinProb=0.5) {
    tdf <- df |> dplyr::group_by(Stock) |>
      dplyr::filter(Rec_Reduction==rec_reduction) |>
      dplyr::mutate(Rebuild=Prob>=MinProb) |>
      dplyr::select(Stock, MP_Name, Rebuild) |>
      arrange(Stock, MP_Name)


    tdf2 <- tdf |> tidyr::pivot_wider(names_from=MP_Name, values_from = Rebuild) |>
      ungroup() |>
      dplyr::select(!Stock) |> t() |>
      data.frame()
    tdf2$MP <- rownames(tdf2)
    colnames(tdf2) <- c(levels(tdf$Stock), 'Management Option')
    rownames(tdf2) <- NULL

    tdf2 <- tdf2 |> relocate('Management Option')

    rs_ind <- which(tdf2$`Red snapper` == TRUE )
    gg_ind <- which(tdf2$`Gag grouper` == TRUE )
    bs_ind <- which(tdf2$`Black sea bass` == TRUE )

    col <- 'darkgray'
    tdf2[,2] <- ''
    tdf2[,3] <- ''
    tdf2[,4]<- ''
    library(flextable)
    library(officer)
    small_border <- fp_border(color="gray", width = 1)


    ft <- flextable(tdf2)
    ft <- ft |> border_inner_h(part="body", border = small_border ) |>
      border_inner_v(part="body", border = small_border )
    ft |> bg(i= rs_ind , bg=col,
             j=2) |>
      bg(i= gg_ind , bg=col,
         j=3) |>
      bg(i= bs_ind , bg=col,
         j=4)
  }

  ntab <- length(rec_reduction)
  tablist <- list()
  for (i in 1:ntab) {
    tablist[[i]] <- make_table(df, rec_reduction[i], MinProb)
  }

  filename <- paste0('img/rebuild/table_', om, '.docx')
  if (ntab == 1) {
    save_as_docx(
      tablist[[1]], ,
      path = filename)
  } else if (ntab == 2) {
    save_as_docx(
      'Plot 1' = tablist[[1]],
      'Plot 2' = tablist[[2]],
      path = filename)
  }

}
#' @describeIn Plot_Prob_Rebuild tradeoff plots
#' @export
trade_off_plot <- function(df, rec_reduction=1, mps_keep=NULL) {

  df <- df |> dplyr::filter(Rec_Reduction==rec_reduction) |>
    tidyr::pivot_wider(names_from = PM, values_from = Median)

  if (!is.null(mps_keep)) {
    df <- df |> dplyr::filter(MP_Name%in% mps_keep)
  }

  cols <- RColorBrewer::brewer.pal(length(mps_keep), 'Dark2')

  p1 <- ggplot(df, aes(x=`Relative Short-Term Landings`, y=`Relative Long-Term Landings`,
                       color=MP_Name)) +
    facet_grid(~Stock) +
    ggrepel::geom_text_repel(aes(label=MP_Name, color=MP_Name)) +
    geom_point() +
    scale_color_manual(values=cols) +
    guides(color='none') +
    theme_bw() +
    expand_limits(x=c(0,1), y=c(0,1)) +
    geom_hline(yintercept = 1, lty=2, color='darkgray') +
    geom_vline(xintercept = 1, lty=2, color='darkgray')


  p2 <- ggplot(df, aes(x=1-`Fraction Discarded`, y=Prob,
                       color=MP_Name)) +
    facet_grid(~Stock) +
    ggrepel::geom_text_repel(aes(label=MP_Name, color=MP_Name)) +
    geom_point() +
    scale_color_manual(values=cols) +
    guides(color='none') +
    theme_bw() +
    expand_limits(x=c(0,1), y=c(0,1)) +
    labs(x='Fraction Retained', y='Probability Rebuild')

  p2 / p1
}
