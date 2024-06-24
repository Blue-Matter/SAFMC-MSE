convert_units <- function(bam_units, value) {
  if (grepl('1000 lb', bam_units))
    value <- openMSE::kg2_1000lb(value)
  value
}


run_simulations <- function(Hist) {
  if (inherits(Hist, 'MOM')) {
    Hist@nsim <- 2
    message('Simulating Fishery')
    Hist <- SimulateMOM(Hist, parallel = FALSE, silent=TRUE)
  } else if (!inherits(Hist, 'multiHist')) {
    stop('`MOM` must be object class `MOM` or `multiHist`')
  }
  Hist
}


#' Compare the biomass trends from BAM and openMSE
#'
#' @param Hist An object of class `multiHist`
#' @param rdat A list object from bamExtras
#'
#' @return a ggplot object
#' @export
Compare_Biomass <- function(Hist, rdat) {
  Hist <- run_simulations(Hist)

  Current_Yr <- Hist[[1]][[1]]@OMPars$CurrentYr[1]
  nyears <- dim(Hist[[1]][[1]]@TSdata$Number)[2]
  nage <- dim(Hist[[1]][[1]]@AtAge$Number)[2]
  hist_years <- rev(seq(Current_Yr, by=-1, length.out=nyears))
  bam_units <- rdat$info$units.biomass
  om_biomass <- rowSums(Hist[[1]][[1]]@TSdata$Biomass[1,,]) / 1000 # in kg
  om_biomass <- convert_units(bam_units, om_biomass)

  bam <- data.frame(Year=hist_years, value=rdat$t.series$B[1:nyears], Model='BAM')
  om <- data.frame(Year=hist_years, value=om_biomass, Model='OM')
  df <- dplyr::bind_rows(bam, om)

  ggplot2::ggplot(df, ggplot2::aes(x=Year, y=value, color=Model, linetype=Model)) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Biomass') +
    ggplot2::theme_bw()

}

#' Compare the F trends from BAM and openMSE
#'
#' @param Hist An object of class `multiHist`
#' @param rdat A list object from bamExtras
#'
#' @return a ggplot object
#' @export
Compare_F <- function(Hist, rdat) {
  Hist <- run_simulations(Hist)

  Current_Yr <- Hist[[1]][[1]]@OMPars$CurrentYr[1]
  nyears <- dim(Hist[[1]][[1]]@TSdata$Number)[2]
  nage <- dim(Hist[[1]][[1]]@AtAge$Number)[2]
  hist_years <- rev(seq(Current_Yr, by=-1, length.out=nyears))

  om <- matrix(0, nage, nyears)
  nfleet <- length(Hist[[1]])
  for (fl in 1:nfleet) {
    om <- om + Hist[[1]][[fl]]@AtAge$F.Mortality[1,,,1]
  }
  om <- apply(om, 2, max, na.rm=TRUE)

  bam <- data.frame(Year=hist_years, value=rdat$t.series$F.full[1:nyears], Model='BAM')
  om <- data.frame(Year=hist_years, value=om, Model='OM')
  df <- dplyr::bind_rows(bam, om)

  ggplot2::ggplot(df, ggplot2::aes(x=Year, y=value, color=Model, linetype=Model)) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Fishing Mortality') +
    ggplot2::theme_bw()

}

get_Landings <- function(rdat) {
  tt <- grepl('klb', names(rdat$LD.pr.tseries))
  val <- apply(rdat$LD.pr.tseries[tt][grepl('^L.', names(rdat$LD.pr.tseries)[tt])], 1, sum, na.rm=TRUE)
  as.numeric(val)
}

get_Discards <- function(rdat) {
  tt <- grepl('klb', names(rdat$LD.pr.tseries))
  val <- apply(rdat$LD.pr.tseries[tt][grepl('^D.', names(rdat$LD.pr.tseries)[tt])], 1, sum, na.rm=TRUE)
  as.numeric(val)
}

get_Removals <- function(rdat) {
  landings <- get_Landings(rdat)
  discards <- get_Discards(rdat)
  landings + discards
}

#' Compare the landings & discards from BAM and openMSE
#'
#' @param Hist An object of class `multiHist`
#' @param rdat A list object from bamExtras
#'
#' @return a ggplot object
#' @export
#'
Compare_Catch <- function(Hist, rdat, divide=FALSE) {
  Hist <- run_simulations(Hist)

  Current_Yr <- Hist[[1]][[1]]@OMPars$CurrentYr[1]
  nyears <- dim(Hist[[1]][[1]]@TSdata$Number)[2]
  nage <- dim(Hist[[1]][[1]]@AtAge$Number)[2]
  hist_years <- rev(seq(Current_Yr, by=-1, length.out=nyears))

  bam_units <- rdat$info$units.landings
  om_landings <- rep(0, nyears)
  om_discards <- rep(0, nyears)
  nfleet <- length(Hist[[1]])
  for (fl in 1:nfleet) {
    om_landings <- om_landings + rowSums(Hist[[1]][[fl]]@TSdata$Landings[1,,])
    om_discards <- om_discards + rowSums(Hist[[1]][[fl]]@TSdata$Discards[1,,])
  }

  om_landings <- convert_units(bam_units, om_landings)
  om_discards <- convert_units(bam_units, om_discards)

  if (divide) {
    om_landings <- om_landings/1000
    om_discards <- om_discards/1000
  }

  bam_landings <- get_Landings(rdat)[1:nyears]
  bam_discards <- get_Discards(rdat)[1:nyears]

  bam <- dplyr::bind_rows(
    data.frame(Year=hist_years, value=bam_landings, type='Landings', Model='BAM'),
    data.frame(Year=hist_years, value=bam_discards, type='Discards', Model='BAM')
  )

  om <-  dplyr::bind_rows(
    data.frame(Year=hist_years, value=om_landings, type='Landings', Model='OM'),
    data.frame(Year=hist_years, value=om_discards, type='Discards', Model='OM')
  )

  df <- dplyr::bind_rows(bam, om)
  df$type <- factor(df$type, ordered=TRUE, levels=c('Landings', 'Discards'))

  ggplot2::ggplot(df, ggplot2::aes(x=Year, y=value, color=Model, linetype=Model)) +
    ggplot2::facet_wrap(~type) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y=bam_units) +
    ggplot2::theme_bw()
}




Combine_Landing_Fleets <- function(MOM, fleet_df) {

  fleet_names <- names(MOM@Fleets[[1]])
  n_fleets_out <- unique(fleet_df$Mapping)
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  proyears <- MOM@proyears
  nage <- MOM@Stocks[[1]]@maxage + 1
  nsim <- MOM@nsim

  fleet_names <- names(MOM@Fleets[[1]])
  for (i in unique(fleet_df$Mapping)) {
    fleet_df_i <- fleet_df |> dplyr::filter(Mapping==i, Code %in% fleet_names)
    fleet_df_i_landings <- fleet_df_i |> dplyr::filter(Type=='Landing')
    fleet_df_i_discards <- fleet_df_i |> dplyr::filter(Type=='Discard')
    if (nrow(fleet_df_i_landings)>1) {
      if (nrow(fleet_df_i_landings)>2)
        stop('Aggregating more than two landing fleets currently not supported')
      fleet_ind <- match(fleet_df_i_landings$Code, fleet_names)

      l.fleet <- fleet_ind[1] # keep
      d.fleet <- fleet_ind[2] # combine with keep and drop

      fleet_out <- MOM@Fleets[[1]][[l.fleet]]
      fleet_out_cpars <- MOM@cpars[[1]][[l.fleet]]

      # F-at-Age
      F_at_age <- array(0, c(nage, nyears))
      for (fl in 1:2) {
        F_apical <- replicate(nage, MOM@cpars[[1]][[fleet_ind[fl]]]$Find[1,]) |> t()
        F_at_age <- F_at_age+ F_apical * MOM@cpars[[1]][[fleet_ind[fl]]]$V[1,,1:nyears]
      }
      Find <- apply(F_at_age, 2, max)

      # Selectivity
      FMax <- replicate(nage, Find) |> t()
      histV <- F_at_age/FMax

      projV <- replicate(proyears, histV[,nyears])
      V <- abind::abind(histV, projV, along=2)
      V <- replicate(nsim, V) |> aperm(c(3,1,2))

      fleet_out_cpars$V <- V

      # Fishing effort / mortality
      fleet_out_cpars$Find <- replicate(nsim, Find) |> t()

      MOM@Fleets[[1]][[l.fleet]] <- fleet_out
      MOM@cpars[[1]][[l.fleet]] <- fleet_out_cpars
      MOM@Fleets[[1]][[d.fleet]] <- NULL
      MOM@cpars[[1]][[d.fleet]] <- NULL

      names(MOM@Fleets[[1]])[l.fleet] <- fleet_out@Name
      names(MOM@cpars[[1]])[l.fleet] <- fleet_out@Name

      fleet_names <- names(MOM@Fleets[[1]])
      disc_ind <- match(fleet_df_i_discards$Code, fleet_names)
      MOM@Fleets[[1]][[disc_ind]]@Name <- paste0(fleet_out@Name, '.D')
      names(MOM@Fleets[[1]])[disc_ind] <- paste0(fleet_out@Name, '.D')
      names(MOM@cpars[[1]])[disc_ind] <- paste0(fleet_out@Name, '.D')
    }
  }
  MOM
}

Combine_Discard_Fleets <- function(MOM, fleet_df) {

  fleet_names <- names(MOM@Fleets[[1]])
  n_fleets_out <- unique(fleet_df$Mapping)
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  proyears <- MOM@proyears
  nage <- MOM@Stocks[[1]]@maxage + 1
  nsim <- MOM@nsim

  fleet_df_new <- fleet_df |> dplyr::filter(Code %in% fleet_names)

  for (i in unique(fleet_df_new$Mapping)) {
    fleet_df_i <- fleet_df_new |> dplyr::filter(Mapping==i)

    fleet_df_i_landings <- fleet_df_i |> dplyr::filter(Type=='Landing')
    fleet_df_i_discards <- fleet_df_i |> dplyr::filter(Type=='Discard')

    if (nrow(fleet_df_i)>1) {
      if (nrow(fleet_df_i)>2)
        stop('Aggregating more than two fleets currently not supported')

      l.fleet <- match(fleet_df_i_landings$Code, fleet_names)
      d.fleet <- match(fleet_df_i_discards$Code, fleet_names)
      fleet_ind <- c(l.fleet, d.fleet)

      fleet_out <- MOM@Fleets[[1]][[l.fleet]]
      fleet_out_cpars <- MOM@cpars[[1]][[l.fleet]]

      # F-Dead-at-Age
      F_Dead_at_age <- array(0, c(2, nage, nyears))
      F_apical <- array(0, dim=c(2, nage, nyears))
      for (fl in 1:2) {
        F_apical[fl,,] <- replicate(nage, MOM@cpars[[1]][[fleet_ind[fl]]]$Find[1,]) |> t()
        F_Dead_at_age[fl,,] <- F_apical[fl,,] * MOM@cpars[[1]][[fleet_ind[fl]]]$V[1,,1:nyears]
      }
      F_Dead_at_age_total <- apply(F_Dead_at_age, 2:3, sum)
      Find <- apply(F_Dead_at_age_total, 2, max)

      # Fishing effort / mortality
      fleet_out_cpars$Find <- replicate(nsim, Find) |> t()

      # Discard mortality
      fleet_out_cpars$Fdisc_array1 <- MOM@cpars[[1]][[d.fleet]]$Fdisc_array1

      # Selectivity-at-Age
      # Selectivity
      FMax <- replicate(nage, Find) |> t()
      histV <- F_Dead_at_age_total/FMax
      histV[!is.finite(histV)] <- 1E-10

      projV <- replicate(proyears, histV[,nyears])
      V <- abind::abind(histV, projV, along=2)
      V <- replicate(nsim, V) |> aperm(c(3,1,2))
      fleet_out_cpars$V <- V

      # Landings Retention-at-Age
      apicalF <- replicate(nage, MOM@cpars[[1]][[l.fleet]]$Find[1,]) |> t()
      F_Land_at_Age <- apicalF * MOM@cpars[[1]][[l.fleet]]$V[1,,1:nyears]
      F_land <- apply(F_Land_at_Age, 2, max)

      retA <- F_Land_at_Age/F_Dead_at_age_total
      retA[!is.finite(retA)] <- 1E-6
      projretA <- replicate(proyears, retA[,nyears])
      retA <- abind::abind(retA, projretA, along=2)
      retA <- replicate(nsim, retA) |> aperm(c(3,1,2))

      fleet_out_cpars$retA <- retA

      MOM@Fleets[[1]][[l.fleet]] <- fleet_out
      MOM@cpars[[1]][[l.fleet]] <- fleet_out_cpars
    }

  }

  # drop discard fleets
  disc_fleets <- fleet_df_new |> dplyr::filter(Type=='Discard')
  for (i in 1:nrow(disc_fleets)) {
    fleet_names <- names(MOM@Fleets[[1]])
    fleet_ind <- match(disc_fleets$Code[i], fleet_names)
    MOM@Fleets[[1]][[fleet_ind]] <- NULL
    MOM@cpars[[1]][[fleet_ind]] <- NULL
  }
  MOM
}

Add_Dummy_Fleets <- function(MOM, fleet_df) {
  fleet_names <- names(MOM@Fleets[[1]])
  fleet_df_new <- fleet_df |>
    dplyr::filter(Type == 'Landing')

  i_rows <- fleet_df_new |> dplyr::distinct(Mapping)
  fleet_df_new <- fleet_df_new[i_rows$Mapping,]

  dummy_fleets <- fleet_df_new$Code[!fleet_df_new$Code %in% fleet_names]

  if (length(dummy_fleets)>0) {

    for (fl in seq_along(dummy_fleets)) {
      dummy_fleet <- MOM@Fleets[[1]][[1]]
      dummy_fleet_cpars <-  MOM@cpars[[1]][[1]]
      dummy_fleet@Name <- dummy_fleets[fl]
      dummy_fleet_cpars$Find[] <- 0
      dummy_fleet_cpars$qs[] <- 0

      MOM@Fleets[[1]] <- append(MOM@Fleets[[1]], dummy_fleet)
      nms <- names(MOM@Fleets[[1]])
      nms[length(nms)] <- dummy_fleets[fl]
      names(MOM@Fleets[[1]]) <- nms

      MOM@cpars[[1]] <- append(MOM@cpars[[1]], list(dummy_fleet_cpars))
      names(MOM@cpars[[1]]) <- nms
    }
  }
  MOM
}

Order_Fleets <- function(MOM, fleet_df) {
  fleet_names <- names(MOM@Fleets[[1]])
  fleet_df_new <- fleet_df |>
    dplyr::filter(Type == 'Landing')
  i_rows <- fleet_df_new |> dplyr::distinct(Mapping)
  fleet_df_new <- fleet_df_new[i_rows$Mapping,]
  fleet_names <- names(MOM@Fleets[[1]])
  ord <- order(match(fleet_df_new$Code, fleet_names))
  ordered_MOM <- MOM
  for (fl in seq_along(fleet_names)) {
    ordered_MOM@Fleets[[1]][[fl]] <- MOM@Fleets[[1]][[ord[fl]]]
    ordered_MOM@cpars[[1]][[fl]] <- MOM@cpars[[1]][[ord[fl]]]
  }
  names(ordered_MOM@Fleets[[1]]) <- fleet_df_new$Code
  names(ordered_MOM@cpars[[1]]) <- fleet_df_new$Code
  ordered_MOM
}

Add_Discard_Mortality <- function(MOM, discard_mortality) {

  fleet_names <- names(MOM@Fleets[[1]])
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  proyears <- MOM@proyears
  nage <- MOM@Stocks[[1]]@maxage + 1
  nsim <- MOM@nsim
  current_year <- MOM@Fleets[[1]][[1]]@CurrentYr
  years <- rev(seq(current_year, by=-1, length.out=nyears))

  discard_mortality_stock <- discard_mortality |>
    dplyr::filter(Stock==names(MOM@Stocks)[1])

  fleet_names <- names(MOM@Fleets[[1]])
  disc_fleets <- unique(discard_mortality_stock$Code)
  chk <- disc_fleets %in% fleet_names
  if (any(!chk))
    stop('discard_mortality$Code does not match MOM fleet names')

  for (fl in seq_along(disc_fleets)) {
    disc_fleet <- disc_fleets[fl]
    disc_fleet_ind <- match(paste0(disc_fleet,'.D'), fleet_names)

    # discard mortality probability by year
    disc_m_df <- discard_mortality_stock |> dplyr::filter(Code==disc_fleet)

    disc_m_DF <- data.frame(Year=years, Prob_Dead=disc_m_df$Prob_Dead[1])
    for (i in 2:nrow(disc_m_df)) {
      disc_m_DF <- disc_m_DF |> dplyr::mutate(Prob_Dead=ifelse(Year>=disc_m_df$Year[i],
                                                               disc_m_df$Prob_Dead[i], Prob_Dead))
    }

    # extend for projections
    Prob_Dead <- c(disc_m_DF$Prob_Dead,rep(disc_m_DF$Prob_Dead[length(disc_m_DF$Prob_Dead)],proyears))
    # make discard-M-at-age array
    Prob_Dead <- replicate(nsim, Prob_Dead)
    Prob_Dead <- replicate(nage, Prob_Dead)
    Prob_Dead <- aperm(Prob_Dead, c(2,3,1))
    MOM@cpars[[1]][[disc_fleet_ind]]$Fdisc_array1 <- Prob_Dead

    # inflate hist F
    Find <- MOM@cpars[[1]][[disc_fleet_ind]]$Find[1,]
    Find <- Find/disc_m_DF$Prob_Dead
    MOM@cpars[[1]][[disc_fleet_ind]]$Find<- replicate(nsim,Find) |> t()
  }
  MOM
}

Add_Fleet_Names <- function(MOM, fleet_df) {
  map <- unique(fleet_df$Mapping)
  df <- fleet_df |> dplyr::filter(Type=='Landing')
  nstocks <- length(MOM@Fleets)
  nfleets <- length(MOM@Fleets[[1]])
  for (s in 1:nstocks) {
    names(MOM@Fleets[[s]]) <- df$Name[map]
    names(MOM@cpars[[s]]) <- df$Name[map]

    for (f in 1:nfleets) {
      MOM@Fleets[[s]][[f]]@Name <- df$Name[map][f]
    }
  }
  MOM
}

#' Compare the F trends from BAM and openMSE
#'
#' @param MOM An object of class `MOM`
#' @param fleet_df A dataframe. See 'Operating Models' article
#' @param discard_mortality A dataframe. See 'Operating Models' article
#' @return An updated MOM
#' @export
Aggregate_Fleets <- function(MOM, fleet_df, discard_mortality) {

  nyears <- MOM@Fleets[[1]][[1]]@nyears
  proyears <- MOM@proyears
  nage <- MOM@Stocks[[1]]@maxage + 1
  nsim <- MOM@nsim

  MOM1 <- Combine_Landing_Fleets(MOM, fleet_df)

  MOM2 <- Add_Discard_Mortality(MOM1, discard_mortality)

  MOM3 <- Combine_Discard_Fleets(MOM2, fleet_df)

  MOM4 <- Add_Dummy_Fleets(MOM3, fleet_df)

  MOM5 <- Order_Fleets(MOM4, fleet_df)

  Add_Fleet_Names(MOM5, fleet_df)

}


