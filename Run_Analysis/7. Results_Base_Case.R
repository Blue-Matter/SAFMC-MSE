

make_DFs <- function(stock='Red snapper', om='BaseCase', MSE_dir='MSE_objects') {

  stock_code <- switch(stock,
                       "Red snapper"='RS',
                       "Gag grouper" = "GG",
                       "Black sea bass" ='BS'
  )

  MSE_files <- list.files(MSE_dir)
  stock_mse_files <- MSE_files[grepl(paste(om, stock_code, sep='_'), MSE_files)]
  MP_text <- lapply(strsplit(stock_mse_files, paste0(paste(om, stock_code, sep='_'), '_')), '[[', 2) |> unlist()
  MP_names <- unlist(strsplit(MP_text, '.mmse'))

  Ref_DF <- readRDS('Misc_Objects/ref_df.rda')
  Ref_DF$OM <- gsub(' ', '',Ref_DF$OM)
  Ref_DF <- Ref_DF|> dplyr::filter(Stock==stock, OM==om)

  for (i in seq_along(stock_mse_files)) {
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

    nm <- paste0(paste(om, stock_code, MP_name, sep='_'), '.rda')

    saveRDS(df, file.path('Results_Objects/F_Landing_Discards', nm))


    # # Mean Age
    # MSE@N |> dim()
    # MSE@
    # MSE@PPD[[1]][[1]][[mm]]@CAA[1,1,]
    # MSE@PPD[[1]][[1]][[mm]]@CAL[1,1,]
    #
    #
    # # Prob Trophy Fish
    #
    #
    # yr <- 2032
    # yr_ind <- match(yr, project_yrs)
    # sim <- 50
    # mm <- 1
    # mp <- mps[mm]
    #
    # MSE@FM[sim,1,,mm,yr_ind]
    # df |> dplyr::filter(Sim==sim, Year==yr, MP==mp)
    #
    # MSE@Catch[1,1,3,1,]
    # MSE@Catch[1,1,3,6,]
    #




  }


}

make_DFs()


Plot_Prob_Rebuild <- function(stock='Red snapper', om='BaseCase', rebuild_target=2044) {

  stock_code <- switch(stock,
                       "Red snapper"='RS',
                       "Gag grouper" = "GG",
                       "Black sea bass" ='BS'
  )

  fls <- list.files("Results_Objects/SSB")
  fls <- fls[grepl(paste0(paste(om, stock_code, sep='_'), "_"), fls)]

  df_list <- list()
  for (i in seq_along(fls)) {
    df_list[[i]] <- readRDS(file.path("Results_Objects/SSB", fls[i]))
  }
  df <- do.call('rbind', df_list) |> dplyr::filter(Year==rebuild_target)
  df$Rebuilt <- df$SSB>df$MSST

  df2 <- df |> dplyr::group_by(MP_Name, Rec_Reduction) |>
    dplyr::summarise(Prob=mean(Rebuilt), .groups='drop')

  df2$Rec_Reduction <- factor(df2$Rec_Reduction)
  df2$MP_Name <- factor(df2$MP_Name, levels=c('SQ', 'SQ_FR', 'SQ_MLL', 'SQ_NS', 'SQ_OS',
                                              'SQ_FR_MLL', 'SQ_FR_NS', 'SQ_FR_OS',
                                              'SQ_MLL_NS', 'SQ_MLL_OS',
                                              'SQ_FR_MLL_NS', 'SQ_FR_MLL_OS'))
  ggplot(df2, aes(x=Rec_Reduction, y=MP_Name)) +
    geom_tile(aes(fill=Prob)) +
    geom_text(aes(label = round(Prob, 2))) +
    scale_fill_gradient2(low = "red", mid='white', high = "green", midpoint=0.5)


}

rebuild_target <- 2044

targ_yr_ind <- match(rebuild_target, project_yrs)
ref_targets <- Ref_DF |> dplyr::filter(Stock=='Red snapper', OM=='Base Case')

ssb <- MSE@SSB[,1,,]
prob_rebuild <- apply(ssb>=array(ref_targets$MSST, dim=dim(ssb)), 2:3, mean)





## SB/SBMSST

## Relative STY

## Relative LTY

## Relative Discards

##

# Gag Grouper


# Black Sea Bass
