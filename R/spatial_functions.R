
get_coords <- function(shp, areas, region='region') {
  tt <- shp[shp$AreaName%in% areas,]
  e <- sf::st_union(tt)
  df <- sf::st_coordinates(e)
  df <- data.frame(df[,1:2])
  df$Region <- region
  df
}

#' Extract Lat Longs for Spatial Areass
#'
#' @return
#' @export
#'
#' @examples
Spatial_Area_Definition <- function() {
  invisible(capture.output(shp <- sf::st_read(dsn = file.path(system.file(package='SAMSE'), "SA_EEZ_off_states.shp"), stringsAsFactors = F)))

  # North and South Carolina
  NC_SC <- get_coords(shp, areas=c('Off NC', 'Off SC'), region='North and South Carolina')

  # Georgia - Cape Canaveral
  GA_FL <- get_coords(shp, c('Off GA', 'Off FL'), 'Georgia - Cape Canaveral')
  GA_CpC <- GA_FL %>% dplyr::filter(Y>=28.470)

  # Cape Canaveral to Florida
  CpC_FL <- GA_FL %>% dplyr::filter(Y<28.470)
  CpC_FL$Region <- 'Cape Canaveral - Florida'

  DF <- dplyr::bind_rows(NC_SC, GA_CpC, CpC_FL)
  DF$Region <- factor(DF$Region, levels=unique(DF$Region), ordered = TRUE)

  DF
}




#' Add Fishing Effort Spatial Distribution to MOM
#'
#' @param MOM An object of class `MOM`
#' @param Spatial_Effort A data.frame with Spatial Effort by Area and fleet.
#' If multiple stocks, `Spatial_Effort` should be a list length `nstocks`
#'
#' @return An updated MOM object
#' @export
Add_Spatial_Effort <- function(MOM, Spatial_Effort) {
  nstock <- length(MOM@Stocks)
  nfleet <- length(MOM@Fleets[[1]])
  for (st in 1:nstock) {
    if (inherits(Spatial_Effort, 'list')) {
      spat_effort <- Spatial_Effort[[st]]
    } else {
      spat_effort <- Spatial_Effort
    }
    for (fl in 1:nfleet) {
      MOM@cpars[[1]][[fl]]$Fdist <- spat_effort[,fl+1]
    }
  }

  MOM
}

calulate_age_depth <- function(log_mean_nearshore, MOM, Rel_Abun_Region, average_prob, opt=TRUE) {

  Ages <- 0:MOM@Stocks[[1]]@maxage
  nage <- length(Ages)
  Prob_Nearshore <- rep(0.999, length(Ages))
  Prob_Nearshore[2] <- 0.95
  Prob_Nearshore[3] <- 0.8
  Prob_Nearshore[4:nage] <- exp(log_mean_nearshore)

  stock_depth <- dplyr::bind_rows(data.frame(Age=Ages,Proportion=Prob_Nearshore, Area='Nearshore'),
                                  data.frame(Age=Ages,Proportion=1-Prob_Nearshore, Area='Offshore'))

  Rel_Abun_Region$Region <- factor(Rel_Abun_Region$Region,
                                   levels=unique(Rel_Abun_Region$Region),
                                   ordered = TRUE)
  stock_region <- Rel_Abun_Region |> dplyr::group_by(Region) |>
    dplyr::summarise(Proportion=sum(Proportion))

  age_list <- list()
  for (age in Ages) {
    prop_depth <- stock_depth |> dplyr::filter(Age==age)

    age_list[[age+1]] <- data.frame(Area=c(1,3,5,2,4,6),
                                    Region=stock_region$Region,
                                    Depth=rep(prop_depth$Area,each=3),
                                    Proportion=c(stock_region$Proportion * prop_depth$Proportion[1],
                                                 stock_region$Proportion * prop_depth$Proportion[2]),
                                    Age=age)

  }
  Age_Depth_Region_Dist <- do.call('rbind', age_list) |> dplyr::arrange(Area, Age)

  nareas <- length(Rel_Abun_Region$Area)

  mov <- array(NA, dim=c(nage, nareas, nareas))
  initdist <- array(0, c(MOM@nsim,nage,nareas))

  frac_other <- matrix(NA, nrow=nareas, ncol=nareas)
  frac_other[1,] <- c(NA, 1, 0.1, 0.1, 0.01, 0.01)
  frac_other[2,] <- c(1, NA, 0.1, 0.1, 0.01, 0.01)
  frac_other[3,] <- c(0.1, 0.01, NA, 1, 0.1, 0.01)
  frac_other[4,] <- c(0.01, 0.1, 1, NA, 0.1, 0.01)
  frac_other[5,] <- c(0.01, 0.01, 0.1, 0.01, NA, 1)
  frac_other[6,] <- c(0.01, 0.01, 0.1, 0.1, 1, NA)


  for (a in seq_along(Ages)) {
    temp <- Age_Depth_Region_Dist %>% filter(Age==Ages[a]) |>
      dplyr::arrange(Area)
    dist <- temp$Proportion/sum(temp$Proportion)

    dist <- dist/sum(dist)
    mov_age <- makemov2(dist,
                        prob=average_prob,
                        probE=1,
                        frac_other=frac_other, plot=FALSE)

    mov[a,,] <- mov_age
    initdist[, a,] <- t(replicate(MOM@nsim,
                                  CalcAsymptoticDist(mov_age, dist))[1,,])
  }

  for (st in seq_along(MOM@Stocks)) {
    mov <- replicate(MOM@nsim, mov) %>% aperm(., c(4,1,2,3))
    MOM@cpars[[st]][[1]]$mov <- mov
    MOM@cpars[[st]][[1]]$initdist <- initdist
    MOM@cpars[[st]][[1]]$Spat_targ <- rep(0, MOM@nsim)
  }

  if (opt) {
    Hist <- Simulate(MOM, nsim=2, silent=TRUE)

    nyears <- MOM@Fleets[[1]][[1]]@nyears
    b <- Hist[[1]][[1]]@TSdata$Biomass[1,nyears,]
    b <- b/sum(b)
    ss <- sum((b-Rel_Abun_Region$Proportion)^2)
    return(ss)
  }

  MOM
}

#' Add Spatial Distribution to an Operating Model
#'
#' @param MOM An object of class `MOM`
#' @param Rel_Abun_Region A data.frame with the biomass distribution by area in the terminal year
#' @param average_prob The mean probability of staying within an area in a given year
#'
#' @return An object of class `MOM` with the movement matrix and fishing effort
#' distribution provided in custom parameters
#' @export
#'
#' @examples
Add_Spatial_to_OM <- function(MOM, Rel_Abun_Region, average_prob=0.05) {

  # MOM <- Add_Spatial_Effort(MOM, Spatial_Effort)

  # optimize for age-depth distribution
  opt <- optimize(calulate_age_depth, interval=log(c(0.01,0.9)),
                  MOM=MOM, Rel_Abun_Region=Rel_Abun_Region,
                  average_prob)

  MOM <- calulate_age_depth(opt$minimum, MOM,
                     Rel_Abun_Region=Rel_Abun_Region,
                     average_prob, opt=FALSE)

  MOM
}











