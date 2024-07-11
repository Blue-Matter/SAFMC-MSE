
MP_levels <- function() {
  c('SQ', 'SQ_FR', 'SQ_MLL', 'SQ_FR_MLL',
    'SQ_NS', 'SQ_OS', 'SQ_MLL_NS', 'SQ_MLL_OS',
    'SQ_FR_NS', 'SQ_FR_OS',
    'SQ_FR_MLL_NS', 'SQ_FR_MLL_OS')
}

Stock_levels <- function() {
  c('Red snapper', 'Gag grouper', 'Black sea bass')
}


#' Create data.frames of MSE results
#'
#' @param om OM name
#' @param MSE_dir
#'
#' @return Nothing. Saves data.frames to 'Results_Objects'
#' @export
make_DFs <- function(om='BaseCase', MSE_dir='MSE_Objects') {

  MSE_files <- list.files(MSE_dir)
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
    MP_text <- lapply(strsplit(stock_mse_files, paste0(paste(om, stock_code, sep='_'), '_')), '[[', 2) |> unlist()
    MP_names <- unlist(strsplit(MP_text, '.mmse'))

    Ref_DF <- readRDS('Misc_Objects/ref_df.rda')
    Ref_DF$OM <- gsub(' ', '',Ref_DF$OM)
    Ref_DF <- Ref_DF|> dplyr::filter(Stock==stock, OM==om)

    mp_list <- list()
    for (i in seq_along(stock_mse_files)) {
      MSEtool:::message_info(stock_mse_files[i], paste0(i, '/', length(stock_mse_files)))
      MSE <- readRDS(file.path(MSE_dir, stock_mse_files[i]))
      project_yrs <- seq(MSE@multiHist[[1]][[1]]@OMPars$CurrentYr[1]+1, by=1, length.out=MSE@proyears)

      MP_name <- MP_names[i]
      rec_reduction <- lapply(strsplit(MSE@MPs[[1]], paste0(MP_name,'_')), '[[', 2) |> unlist() |> as.numeric()

      mps <- MSE@MPs[[1]]
      nMPs <- length(mps)

      # SSB
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

      saveRDS(df, file.path('Results_Objects/SSB', nm))

      # Apical F,  Landings and Discards
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
      saveRDS(df, file.path('Results_Objects/F_Landing_Discards', nm))


    } # end MP loop
  } # end stock loop

}

make_DFs(om='BaseCase')

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

Plot_Prob_Rebuild <- function(om='BaseCase', results_dir='Results_Objects') {

  Results_files <- list.files(file.path(results_dir, 'SSB'))

  OM_fls <- lapply(strsplit(Results_files, paste0(om, '_')), '[[', 2) |> unlist()
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
  DF$MP_Name <- factor(DF$MP_Name, levels=MP_levels())
  DF$Stock <- factor(DF$Stock, levels=Stock_levels())

  ggplot2::ggplot(DF, aes(x=Rec_Reduction, y=MP_Name)) +
    facet_grid(~Stock) +
    geom_tile(aes(fill=Prob)) +
    geom_text(aes(label = round(Prob, 2))) +
    scale_fill_gradient2(low = "red", mid='white', high = "green", midpoint=0.5) +
    theme_bw() +
    scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0)) +
    labs(x='Relative Effort General Recreational Fleet',
         y='Management Actions',
         fill='Probability')

}


rebuild_target <- 2044

targ_yr_ind <- match(rebuild_target, project_yrs)
ref_targets <- Ref_DF |> dplyr::filter(Stock=='Red snapper', OM=='Base Case')

ssb <- MSE@SSB[,1,,]
prob_rebuild <- apply(ssb>=array(ref_targets$MSST, dim=dim(ssb)), 2:3, mean)


# TODO:
# 2. why does GG NS perform better than OS at lower effort
MSE_1 <- readRDS('MSE_Objects/BaseCase_GG_SQ_OS.mmse')
MSE_2 <- readRDS('MSE_Objects/BaseCase_GG_SQ_NS.mmse')

sim<- 1
mm <- 1
data.frame(OS=MSE_1@SB_SBMSY[sim,1,mm,],
           NS=MSE_2@SB_SBMSY[sim,1,mm,])

RS_multiHist <- readRDS('Hist_Objects/BaseCase_RS.hist')
GG_multiHist <- readRDS('Hist_Objects/BaseCase_GG.hist')
BS_multiHist <- readRDS('Hist_Objects/BaseCase_BS.hist')

b <- RS_multiHist[[1]][[1]]@TSdata$Biomass[1,70, ]
plot(b/sum(b)*100, c(3,1,70,19,3,4))
abline(a=0, b=1, lty=3)

b <- GG_multiHist[[1]][[1]]@TSdata$Biomass[1,58, ]
b/sum(b)
plot(b/sum(b)*100, c(34, 21, 21, 18, 3, 3))
abline(a=0, b=1, lty=3)

b <- BS_multiHist[[1]][[1]]@TSdata$Biomass[1,44, ]
b/sum(b)
plot(b/sum(b)*100, c(14,2,72,6,4,2))
abline(a=0, b=1, lty=3)


## SB/SBMSST

## Relative STY

## Relative LTY

## Relative Discards

##

# Gag Grouper


# Black Sea Bass
