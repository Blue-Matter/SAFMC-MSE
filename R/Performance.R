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
                       MSST=Ref_DF$MSST,
                       Rebuild=Ref_DF$Rebuild)

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


#' Get the rebuilding target year
#'
#' @param stock_code either 'RS', 'GG', or 'BS'
#' @export
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
    df$Rebuilt <- df$SSB>df$Rebuild
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
    scale_fill_gradient2(low ='#1E88E5', mid='white', high = "#FFC107", midpoint=0.5) +
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
                           cols=c('#1E88E5', 'white', '#FFC107'),
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

  disc_RS <- make_PM_plot(DF, pm='Fraction Discarded', cols=cols)
  disc_GG <- make_PM_plot(DF, 'Gag grouper', pm='Fraction Discarded', cols=cols)
  disc_BS <- make_PM_plot(DF, 'Black sea bass', pm='Fraction Discarded', cols=cols)


  strip_size <- 10
  title_size <- 8
  text_size <- 6
  library(patchwork)
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

#' @describeIn Plot_Prob_Rebuild SSB/Rebuild time-series plot
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
    summarise(Median=median(SSB/Rebuild),
              Upper=quantile(SSB/Rebuild, 0.95),
              Lower=quantile(SSB/Rebuild, 0.05),
              MSST=unique(MSST),
              Rebuild=unique(Rebuild))

  if (om =='EC') {
    hist_file <- paste0(paste0('BaseCase', '_', stock_code), '.hist')
  } else {
    hist_file <- paste0(paste0(om, '_', stock_code), '.hist')
  }
  hist <- readRDS(file.path('Hist_Objects', hist_file))

  histSB <- get_SSB(hist) |> dplyr::filter(Sim==1)
  histSB$Value <- histSB$Value/unique(DF$Rebuild)

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
         y='SB/SBMSY (median)',
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
         y='SB/SBMSY (median)',
         linetype=label,
         color=label,
         title=stock) +
    scale_color_manual(values=cols) +
    theme(legend.key.width = unit(2, "line"))


}

#' @export
MP_levels <- function() {
  c('SQ', 'SQ_FR', 'SQ_MLL',  'SQ_NS', 'SQ_OS',
    'SQ_FR_MLL',  'SQ_FR_NS', 'SQ_FR_OS',
    'SQ_MLL_NS', 'SQ_MLL_OS',
    'SQ_FR_MLL_NS', 'SQ_FR_MLL_OS'
  )
}

#' @export
Stock_levels <- function() {
  c('Red snapper', 'Gag grouper', 'Black sea bass')
}

#' @export
Fleet_levels <- function() {
  c("Commercial Line",
    "Recreational Headboat",
    "General Recreational",
    "Commercial Dive"
  )
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



#### October Performance Calcs ----

add_last_hist_year <- function(hist, proj) {
  hist_val <- hist |> ungroup() |> dplyr::filter(Year==max(Year))
  proj_1 <- proj |> ungroup() |> dplyr::filter(Year==min(proj$Year))
  if (!all(is.na(hist_val$MP))) {
    hist_val <- hist_val |> dplyr::filter(MP %in% proj_1$MP)
  }
  nMP <- unique(proj_1$MP) |> length()
  proj_1$Year <- proj_1$Year-1
  proj_1$Value <- rep(hist_val$Value, each=nMP)
  if (!is.null(proj_1$Landings)) {
    proj_1$Landings <- rep(hist_val$Landings, each=nMP)
    proj_1$Discards <- rep(hist_val$Discards, each=nMP)
  }

  bind_rows(proj_1, proj)
}


#' @export
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' @export
make_DFs <- function(om, mp_code) {
  om_mse_files <- mse_files[grepl(om, mse_files)]

  if (om=='Rec1') {
    mp_code_mse_files <- om_mse_files[grepl(paste0(mp_code, '_REC1', '\\.'), om_mse_files)]
  } else {
    mp_code_mse_files <- om_mse_files[grepl(paste0(mp_code, '\\.'), om_mse_files)]
  }



  stock_codes <- unlist(lapply(strsplit(mp_code_mse_files, '_'), '[[',2))
  F_list <- list()
  SSB_list <- list()
  Landings_list <- Removals_list <- list()
  CAA_list <- list()
  for (i in seq_along(stock_codes)) {
    MSE <- readRDS(file.path('MSE_Objects', mp_code_mse_files[i]))
    MPs <- MSE@MPs[[1]]
    Stock <- firstup(tolower(names(MSE@Snames)))

    this_stock <- tolower(Stock)

    if (om %in% c('BaseCase', 'Rec1')) {
      this_ref <- Ref_DF |> dplyr::filter(OM=='BaseCase', stock==this_stock)
      this_ref$rebuild_year <- get_rebuild_year(stock_codes[i])
      this_ref$OM <- om
    } else {
      this_ref <- Ref_DF |> dplyr::filter(OM==om, stock==this_stock)
      this_ref$rebuild_year <- get_rebuild_year(stock_codes[i])
    }


    # Spawning Biomass
    SSB <- openMSE::get_SSB(MSE)

    SSB_hist <- SSB |> dplyr::filter(Period=='Historical') |>
      dplyr::group_by(Year) |>
      dplyr::summarise(Value=median(Value))

    SSB_proj <- SSB |> dplyr::filter(Period!='Historical')

    rebuild_prob <- SSB_proj |> dplyr::filter(Year==this_ref$rebuild_year) |>
      dplyr::group_by(MP) |>
      dplyr::summarize(pMSST=mean(Value>this_ref$MSST),
                pRebuild=mean(Value>this_ref$Rebuild))

    SSB_proj <- SSB_proj |>
      dplyr::group_by(Year, MP) |>
      dplyr::summarise(Lower=quantile(Value, 0.25),
                       Upper=quantile(Value, 0.75),
                       Value=median(Value)
                       )

    SSB_proj <- dplyr::left_join(SSB_proj, rebuild_prob)
    SSB_proj <- add_last_hist_year(SSB_hist, SSB_proj)

    SSB_proj$rebuild_year <- this_ref$rebuild_year

    SSB_proj$Period <- 'Projection'
    SSB_hist$Period <- 'Historical'
    df <- dplyr::bind_rows(SSB_hist, SSB_proj)
    df$MSST <- this_ref$MSST
    df$Rebuild  <- this_ref$Rebuild
    df$Stock <- Stock
    df$Stock <- factor(df$Stock, levels=Stock_levels())
    df$stock_code <- stock_codes[i]
    SSB_list[[i]] <- df

    # Fishing Mortality
    FM <- openMSE::get_F(MSE)
    F_hist <- FM |> dplyr::filter(Period=='Historical') |>
      dplyr::group_by(Year, Fleet) |>
      dplyr::summarise(Value=median(Value))

    F_proj <- FM |> dplyr::filter(Period!='Historical')
    F_proj <- F_proj |>
      dplyr::group_by(Year, Fleet, MP) |>
      dplyr::summarise(Value=median(Value))

    F_proj <- add_last_hist_year(F_hist, F_proj)
    F_proj$Period <- 'Projection'
    F_hist$Period <- 'Historical'

    df <- dplyr::bind_rows(F_hist, F_proj)
    df$MFMT  <- this_ref$MFMT
    df$Stock <- Stock
    df$stock_code <- stock_codes[i]
    df$Stock <- factor(df$Stock, levels=Stock_levels())
    df$Fleet <- factor(df$Fleet, levels=Fleet_levels())
    F_list[[i]] <- df

    # Landings & Discards
    Removals <- openMSE::get_Removals(MSE)

    Removals_hist <- Removals |> dplyr::filter(Period=='Historical') |>
      dplyr::group_by(Year, Fleet) |>
      dplyr::summarise(Value=median(Value))

    Removals_proj <- Removals |> dplyr::filter(Period!='Historical') |>
      dplyr::group_by(Year, Fleet, MP) |>
      dplyr::summarise(Value=median(Value))


    Removals_proj <- add_last_hist_year(Removals_hist, Removals_proj)
    Removals_proj$Period <- 'Projection'
    Removals_hist$Period <- 'Historical'


    removal_DF <- dplyr::bind_rows(Removals_hist, Removals_proj)


    Landings <- openMSE::get_Landings(MSE)

    Landings_hist <- Landings |> dplyr::filter(Period=='Historical') |>
      dplyr::group_by(Year, Fleet) |>
      dplyr::summarise(Lower=quantile(Value, 0.25),
                       Upper=quantile(Value, 0.75),
                       Value=median(Value))

    Landings_proj <- Landings |> dplyr::filter(Period!='Historical') |>
      dplyr::group_by(Year, Fleet, MP) |>
      dplyr::summarise(Lower=quantile(Value, 0.25),
                       Upper=quantile(Value, 0.75),
                       Value=median(Value))


    Landings_proj <- add_last_hist_year(Landings_hist, Landings_proj)
    Landings_proj$Period <- 'Projection'
    Landings_hist$Period <- 'Historical'


    df <- dplyr::bind_rows(Landings_hist, Landings_proj)
    df$Landings <- df$Value
    df$Discards <- removal_DF$Value - df$Landings

    df$Stock <- Stock
    df$stock_code <- stock_codes[i]
    df$Stock <- factor(df$Stock, levels=Stock_levels())
    df$Fleet <- factor(df$Fleet, levels=Fleet_levels())

    maxY <- df |> dplyr::ungroup() |>
      dplyr::group_by(Stock) |>
      dplyr::filter(Period=='Historical') |>
      dplyr::mutate(MaxYear=max(Year)) |>
      dplyr::filter(Year > MaxYear-3) |>
      dplyr::group_by(Stock, Fleet) |>
      dplyr::summarise(Ref_Landing=mean(Landings),
                    Ref_Discard=mean(Discards))


    df <- left_join(df, maxY)
    table_vals <- df |> dplyr::group_by(Stock, Fleet, MP) |>
      dplyr::summarize(STY=mean(Landings[Year%in%2025:2029]/Ref_Landing[Year%in%2025:2029], na.rm=TRUE),
                    LTY=mean(Landings[Year%in%2035:2039]/Ref_Landing[Year%in%2035:2039], na.rm=TRUE),
                    FracDiscard=median(Discards/(Landings+Discards), na.rm=TRUE))


    df <- left_join(df, table_vals)


    rebuild_prob <- SSB_proj |> dplyr::filter(Year==this_ref$rebuild_year) |>
      dplyr::group_by(MP) |>
      dplyr::summarize(pMSST=mean(Value>this_ref$MSST),
                pRebuild=mean(Value>this_ref$Rebuild))


    Landings_list[[i]] <- df


    # CAA

  }

  SSB <- do.call('rbind', SSB_list)
  FM <- do.call('rbind', F_list)
  list(SSB=SSB, FM=FM,
       Landings=do.call('rbind', Landings_list))
}

#' @export
plotTS_FM <- function(DF,
                      stock='RS',
                      RecEff=0,
                      inc.ref=TRUE,
                      inc.guide=TRUE,
                      rel.to=FALSE,
                      col.hist='darkgray',
                      lwd=0.5, incDive=FALSE,
                      col.theme='Dark2',
                      ylab='Fishing Mortality (F)',
                      size.strip.text=8,
                      size.axis.title=8,
                      size.axis.text=6,
                      byFleet=!inc.ref) {

  df <- DF |> dplyr::filter(stock_code%in%stock)
  if (!incDive)
    df <- df |> dplyr::filter(Fleet!='Commercial Dive')

  if (rel.to) {
    df$Value <- df$Value/df$MFMT
    ylab <- 'F/MFMT'
  }

  if (byFleet)
    inc.ref <- FALSE
  if (!byFleet)
    df <- df |> dplyr::group_by(Year, Stock, MP) |>
      dplyr::mutate(Value=sum(Value))


  mps <- unique(df$MP)
  mps <- mps[!is.na(mps)]
  eff_values <- unlist(lapply(strsplit(mps, '_'), '[[', 2)) |>
    as.numeric()

  MPs <- mps[match(RecEff, eff_values)]
  df_hist <- df |> dplyr::filter(Period=='Historical')
  df_proj <- df |> dplyr::filter(Period!='Historical', MP %in% MPs)

  this_ref <- df_proj |> dplyr::ungroup() |> dplyr::distinct(Stock, MFMT)

  nMP <- length(MPs)
  mp.cols <- RColorBrewer::brewer.pal(max(nMP,3), col.theme)

  p <- ggplot() +
    geom_line(data=df_hist, aes(x=Year, y=Value),
              color=col.hist, linewidth=lwd)

  if (inc.ref) {
    if (rel.to) {
      p <- p + geom_hline(yintercept = 1, linetype=2, linewidth=0.5*lwd)
    } else {
      p <- p + geom_hline(data=this_ref, aes(yintercept = MFMT), linetype=2, linewidth=0.5*lwd)
    }
  }

  if(byFleet) {
    p <- p + facet_grid(Stock~Fleet, scales = 'free_y')
  } else {
    p <- p + facet_wrap(~Stock)
  }
  p <- p + geom_line(data=df_proj, aes(x=Year, y=Value, color=MP),
                linewidth=lwd)

  p <- p +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y=ylab) +
    expand_limits(y=0) +
    scale_color_manual(values=mp.cols) +
    theme_bw()



  if (!inc.guide)
    p <- p + guides(color='none')

  if (nMP==1) {
    p <- p + guides(color='none')
  }
  p + theme(strip.background = element_blank(),
            strip.text=element_text(size=size.strip.text),
            axis.title = element_text(size=size.axis.title),
            axis.text = element_text(size=size.axis.text))
}


#' @export
plotTS_SSB <- function(SSB,
                      stock='RS',
                      mp='SQ_0',
                      ref_mp=NULL,
                      inc.ref=TRUE,
                      inc.guide=FALSE,
                      rel.to=TRUE,
                      col.hist='darkgray',
                      lwd=0.5,
                      col.theme='Dark2',
                      ylab='Spawning Biomass',
                      size.strip.text=10,
                      size.axis.title=8,
                      size.axis.text=6,
                      size.ref.label=2,
                      period=c('Both', 'Historical', 'Projection'),
                      ggtheme=theme_classic,
                      facet_scales='free_y',
                      inc.ref.labels=FALSE,
                      inc.perc=FALSE,
                      alpha=0.5,
                      mp.col='darkgreen') {

  period <- match.arg(period)

  df <- SSB |> dplyr::filter(stock_code%in%stock)
  df$Period[df$Year<2025] <- 'Historical'
  df$Period[df$Year>=2025] <- 'Projection'

  # df <- df |> dplyr::filter(Year<=max(rebuild_year, na.rm=TRUE))


  if (rel.to) {
    df$Value <- df$Value/df$Rebuild
    df$Upper <- df$Upper/df$Rebuild
    df$Lower <- df$Lower/df$Rebuild
    ylab <- "SB/Rebuilding Target"
    df$MSST <- df$MSST/df$Rebuild
    df$Rebuild <- 1
  }

  if (period!='Both') {
    if (period=='Projection')
      df <- df |> dplyr::filter(Period==period)
    if (period=='Historical')
      df <- df |> dplyr::mutate(Value=ifelse(Period=='Projection', NA, Value),
                                Lower=ifelse(Period=='Projection', NA, Lower),
                                Upper=ifelse(Period=='Projection', NA, Upper))
  }


  df_hist <- df |> dplyr::filter(Period=='Historical')

  mp_details <- strsplit(df$MP, '_')
  df$mp_name <- unlist(lapply(mp_details, '[[', 1))

  df$effort_level <- NA
  df$effort_level[!is.na(df$mp_name)] <- unlist(lapply(mp_details[!is.na(df$mp_name)], '[[', 2))

  df$effort_level <- (1-as.numeric(df$effort_level)) * 100
  df$effort_level <- as.character(df$effort_level)


  df_proj <- df |> dplyr::filter(Period!='Historical', MP==mp)
  if (!is.null(ref_mp)) {
    df_proj_ref_mp <- df |> dplyr::filter(Period!='Historical', MP==ref_mp)
  }

  if (!is.null(df_proj$Model)) {
    this_ref <- df_proj |> dplyr::ungroup() |> dplyr::distinct(Stock, MSST, Rebuild, Model)
  } else {
    this_ref <- df_proj |> dplyr::ungroup() |> dplyr::distinct(Stock, MSST, Rebuild)
  }

  this_ref$Year <- max(df$Year)-1

  p <- ggplot() +
    geom_line(data=df_hist, aes(x=Year, y=Value),
              color=col.hist, linewidth=lwd)

  if (inc.ref) {
    p <- p +
      geom_hline(data=this_ref, aes(yintercept = MSST), linetype=3, linewidth=0.5*lwd,
                 color='darkblue') +
      geom_hline(data=this_ref, aes(yintercept = Rebuild), linetype=2, linewidth=0.5*lwd,
                 color='darkblue')

    if (inc.ref.labels) {
      p <- p +
        geom_text(data=this_ref, aes(x=Year, y=MSST, group=Stock),
                  vjust = 1, hjust=1, label = "MSST",  color='darkblue', size=size.ref.label) +
        geom_text(data=this_ref, aes(x=Year, y=Rebuild, group=Stock),
                  vjust = -1, hjust=1, label = "Rebuild Target",  color='darkblue', size=size.ref.label)
    }

  }

  p <- p + facet_wrap(~Stock, scales = facet_scales) +
    geom_vline(data=df_proj, aes(xintercept = rebuild_year), linetype=2, color='darkgray')


  if (inc.perc)  {
    p <- p + geom_ribbon(data=df_proj, aes(x=Year, ymin=Lower , ymax=Upper,
                                           fill=mp_name), alpha=alpha)
  }

  p <- p + geom_line(data=df_proj, aes(x=Year, y=Value, color=mp_name),
                     linetype=1,
                     linewidth=lwd)


  if (!is.null(ref_mp)) {
    p <- p + geom_line(data=df_proj_ref_mp,
                       aes(x=Year, y=Value),
                       color='black',
                       linetype=2,
                       linewidth=lwd)

  }

  # nMP <- length(MPs)
  # mp.cols <- RColorBrewer::brewer.pal(max(nMP,3), col.theme)


  p <- p +
    scale_y_continuous(expand = c(0, 0), label=scales::comma) +
    labs(y=ylab) +
    expand_limits(y=0) +
    scale_color_manual(values=mp.col) +
    scale_fill_manual(values=mp.col) +
    ggtheme() +
    labs(color='Management Option',
         linetype='Gen. Rec. Effort (%)')

  if (!inc.guide)
    p <- p + guides(color='none', linetype='none', fill='none')

  # if (nMP==1) {
  #   p <- p + guides(color='none', linetype='none', fill='none')
  # }


  p + theme(strip.background = element_blank(),
            strip.text=element_text(size=size.strip.text),
            axis.title = element_text(size=size.axis.title),
            axis.text = element_text(size=size.axis.text)) +
    guides(fill='none')

}

plotTS_Landings <- function(Landings,
                            stock='RS',
                            mp='SQ_0',
                            ref_mp=NULL,
                            mp_names=NULL,
                            inc.ref=FALSE,
                            inc.guide=FALSE,
                            rel.to=FALSE,
                            col.hist='darkgray',
                            lwd=0.5,
                            incDive=FALSE,
                            col.theme='Dark2',
                            ylab='1000 lb',
                            size.strip.text=8,
                            size.axis.title=8,
                            size.axis.text=6,
                            byFleet=!inc.ref,
                            y_scales=NULL,
                            period=c('Both', 'Historical', 'Projection'),
                            inc.perc=FALSE,
                            alpha=0.5,
                            mp.col='darkgreen') {

  period <- match.arg(period)

  df  <- Landings |> dplyr::filter(stock_code%in%stock)
  df$Period[df$Year<2025] <- 'Historical'
  df$Period[df$Year>=2025] <- 'Projection'



  if (!incDive)
    df <- df |> dplyr::filter(Fleet!='Commercial Dive')

  df$Landings <-   df$Landings  |>
    openMSE::kg2_1000lb()

  df$Discards <-   df$Discards |>
    openMSE::kg2_1000lb()

  if (rel.to) {
    df$Discards <- df$Discards/(df$Discards+df$Landings)
    df$Landings <- df$Landings/df$Ref_Landing


    ylab <- 'Relative Landings (Discards)'
  }

  mp_details <- strsplit(df$MP, '_')
  df$mp_name <- unlist(lapply(mp_details, '[[', 1))

  df$effort_level <- NA
  df$effort_level[!is.na(df$mp_name)] <- unlist(lapply(mp_details[!is.na(df$mp_name)], '[[', 2))

  df$effort_level <- (1-as.numeric(df$effort_level)) * 100
  df$effort_level <- as.character(df$effort_level)


  if (!byFleet) {
    df <- df |> dplyr::group_by(Year, Stock, MP, effort_level, Period, mp_name) |>
      dplyr::distinct(Year, Stock, MP, effort_level, Period, mp_name, Landings, Discards) |>
      dplyr::summarise(Landings=sum(Landings),
                    Discards=sum(Discards))
  }

  df_hist <- df |> dplyr::filter(Period=='Historical')
  df_proj <- df |> dplyr::filter(Period!='Historical', MP==mp)
  df_proj <- add_last_hist_year(hist=df_hist, proj=df_proj)
  df <- bind_rows(df_hist, df_proj)


  df <- df |> tidyr::pivot_longer(cols=c(Landings, Discards))
  df$name <- factor(df$name, levels=c('Landings', 'Discards'), ordered = TRUE)


  if (period!='Both') {
    if (period=='Projection')
      df <- df |> dplyr::filter(Period==period)
    if (period=='Historical')
      df <- df |> dplyr::mutate(Value=ifelse(Period=='Projection', NA, Value),
                                Lower=ifelse(Period=='Projection', NA, Lower),
                                Upper=ifelse(Period=='Projection', NA, Upper))
  }

  df_hist <- df |> dplyr::filter(Period=='Historical')
  df_proj <- df |> dplyr::filter(Period!='Historical', MP==mp)



  if (!is.null(mp_names)) {
    mp_names <- factor(mp_names, ordered=TRUE, levels=mp_names)
    df_proj$mp_name <- mp_names[1]
  }

  if (!is.null(ref_mp)) {
    df_proj_ref_mp <- df |> dplyr::filter(Period!='Historical', MP==ref_mp)
    df_proj_ref_mp$mp_name <- mp_names[2]
  }


  p <- ggplot() +
    geom_line(data=df_hist, aes(x=Year, y=value, linetype=name),
              color=col.hist, linewidth=lwd)

  if (inc.ref) {
    p <- p + geom_hline(yintercept = 1, linetype=2, linewidth=0.5*lwd)

  }





  p <- p + geom_line(data=df_proj, aes(x=Year,
                                       y=value,
                                       color=mp_name,
                                       linetype=name),
                     linewidth=lwd)

  if (byFleet) {
    p <- p + facet_wrap(~Fleet, scales=y_scales)
  } else {
    p <- p + facet_wrap(~Stock, scales=y_scales)
  }

  if (inc.perc)  {
    p <- p + geom_ribbon(data=df_proj, aes(x=Year, ymin=Lower, ymax=Upper,
                                           fill=mp_name), alpha=alpha)
  }


  if (!is.null(ref_mp)) {
    p <- p + geom_line(data=df_proj_ref_mp,
                       aes(x=Year, y=value, color=mp_name, linetype=name),
                       linewidth=lwd)

  }


  p <- p +
    labs(y=ylab, linetype='') +
    expand_limits(y=0) +
    scale_color_manual(values=mp.col) +
    scale_fill_manual(values=mp.col) +
    coord_cartesian(clip = 'off') +
    theme_classic()


  if (!rel.to)
    p <- p +  scale_y_continuous(expand = c(0, 0))



  if (!inc.guide)
    p <- p + guides(fill='none')

  p <- p + labs(linetype='')

  if (!is.null(mp_names)) {
    p <- p + labs(color='MP')
  }

  if (is.null(ref_mp))
    p <- p + guides(color='none')


  p + theme(strip.background = element_blank(),
            strip.text=element_text(size=size.strip.text),
            axis.title = element_text(size=size.axis.title),
            axis.text = element_text(size=size.axis.text),
            legend.position = 'bottom')
}

calc_rebuild_table <- function(SSB, mp='SQ_0', stock='RS') {
  SSB <- SSB |> dplyr::filter(stock_code %in% stock)
  rebuild_DF <- SSB |> dplyr::group_by(Stock) |>
    dplyr::filter(Period=='Projection') |>
    dplyr::distinct(MP, pMSST, pRebuild) |>
    dplyr::arrange(Stock)

  vals <- rebuild_DF |> dplyr::group_by(Stock) |>
    dplyr::filter(MP==mp)

  stocks <- unique(SSB$Stock)

  table_list <- list()
  for (i in seq_along(stocks)) {
    table_list[[i]] <- tibble::tibble(Metric=c('Prob. > Rebuild', 'Prob. > MSST'),
                                      Value=as.numeric(c(vals[i,4], vals[i,3])))

  }


  tableDF <- tibble::tibble(x=-Inf, y=Inf,
                            Stock=unique(SSB$Stock)[order(levels(SSB$Stock))],
                            tbl=table_list)
  tableDF$Stock <- factor(tableDF$Stock, levels=levels(rebuild_DF$Stock), ordered = TRUE)
  tableDF |> dplyr::arrange(Stock)
}


plot_rebuild_matrix <- function(stock='RS', om='BaseCase',
                                results_dir='Results_Objects',
                                size.strip.text=8,
                                size.axis.title=8,
                                size.axis.text=6,
                                col1='#D41159',
                                col2='#1A85FF',
                                selectMPs=NULL) {
  Results_files <- list.files(file.path(results_dir, 'SSB'))

  L <- strsplit(Results_files, paste0(om, '_'))
  ind <- which(unlist(lapply(L, length)) == 2)

  OM_fls <- lapply(strsplit(Results_files[ind], paste0(om, '_')), '[[', 2) |> unlist()
  stocks <- lapply(strsplit(OM_fls, '_'), '[[', 1) |> unlist() |> unique()
  stocks <- stocks[stocks%in%stock]

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
    df$Rebuilt <- df$SSB>df$Rebuild
    st_list[[st]] <- df
  }
  DF <- do.call('rbind', st_list)

  DF <- DF |> dplyr::group_by(Stock, MP_Name, Rec_Reduction) |>
    dplyr::summarise(Prob=mean(Rebuilt), .groups='drop')

  DF$Rec_Reduction <- 1-DF$Rec_Reduction

  DF$Rec_Reduction <- factor(DF$Rec_Reduction, ordered = TRUE,
                             levels=unique(DF$Rec_Reduction))
  DF$MP_Name <- factor(DF$MP_Name, levels=rev(MP_levels()), ordered = TRUE)
  DF$Stock <- factor(DF$Stock, levels=Stock_levels(), ordered = TRUE)


  incSelectMP <- FALSE
  if (!is.null(selectMPs)) {
    incSelectMP <- TRUE

    select_list <- list()
    for (i in 1:nrow(selectMPs)) {
      select_list[[i]] <- DF |> dplyr::filter(MP_Name%in% selectMPs$MP_Name[i],
                          Rec_Reduction%in% selectMPs$Rec_Reduction[i])
      select_list[[i]]$mp <- paste(select_list[[i]]$MP_Name, select_list[[i]]$Rec_Reduction, sep='_')
    }
    df2 <- do.call('rbind', select_list)
  }

  p <- ggplot2::ggplot(DF, aes(x=Rec_Reduction, y=MP_Name)) +
    facet_grid(~Stock)

  if (incSelectMP) {
    p <- p + geom_tile(fill='#faf9f7')  +
      geom_tile(data=df2, aes(fill=mp)) +
      geom_text(aes(label = round(Prob, 2)), size=2, color='lightgray') +
      geom_text(data=df2, aes(label = round(Prob, 2)), size=2) +
      guides(fill='none')

  } else {
    p <- p + geom_tile(aes(fill=Prob)) +
      scale_fill_gradient2(low =col1, mid='white', high = col2, midpoint=0.5) +
      geom_text(aes(label = round(Prob, 2)), size=2)
  }
  p <- p +
    theme_bw() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0)) +
    labs(x='Relative Effort General Recreational Fleet',
         y='Management Actions',
         fill='Probability') +
    theme(strip.background = element_blank(),
          strip.text=element_text(size=size.strip.text),
          axis.title = element_text(size=size.axis.title),
          axis.text = element_text(size=size.axis.text)) +
    theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.5, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=6)) #change legend text font size

  p

}
