source('Build_Package/1b. Set OM Parameters.R')

library(bamExtras)

# --- OM_02 - Lower M ----

## Red Snapper ----

# reduce M at max age to 0.07 instead of 0.107
bam <- bam2r("RedSnapper")
init <- bam$init
init$set_M_constant[1] <- '0.07'
set_M <- init$set_M
nms <- names(set_M)
set_M <- as.numeric(set_M)
set_M <- set_M * 0.07/0.107
set_M <- as.character(set_M)
names(set_M) <- nms
init$set_M <- set_M
bam <- bam2r("RedSnapper", init=init)

BAM <- run_bam(bam=bam)



RSMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                             stock_name='Red Snapper',
                             nsim=nsim,
                             pyears,
                             CAL_bins,
                             Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/RS_Fleet_Structure.rds'),
                             seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/RS_seasonalF.rds'))

## Gag ----
# reduce M at max age to 0.10 instead of 0.172
bam <- bam2r("GagGrouper")
init <- bam$init
init$set_M_constant[1] <- '0.10'
set_M <- init$set_M
nms <- names(set_M)
set_M <- as.numeric(set_M)
set_M <- set_M * 0.10/0.172
set_M <- as.character(set_M)
names(set_M) <- nms
init$set_M <- set_M
bam <- bam2r("GagGrouper", init=init)

BAM <- run_bam(bam=bam)

GGMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                      stock_name='Gag Grouper',
                      nsim=nsim,
                      pyears,
                      CAL_bins,
                      Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/GG_Fleet_Structure.rds'),
                      seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/GG_seasonalF.rds'))


## Combine Stocks into Multi-Stock OM ----
MOM <- Combine_OMs(list(RSMOM, GGMOM), Name='Red Snapper & Gag Grouper MOM')

# Add spatial
frac_other <- readRDS('Build_Package/Objects/BaseCase/frac_other.rds')
Frac_Age_Region <- readRDS('Build_Package/Objects/BaseCase/Frac_Age_Region.rds')

OM_02 <- add_spatial_to_OM(MOM,
                         average_prob=list(0.95,0.95),
                         frac_other=list(frac_other,frac_other),
                         Frac_Age_Region)

usethis::use_data(OM_02, overwrite = TRUE)


# --- OM_03 - Higher M ----

## Red Snapper ----

# increase M at max age to 0.15 instead of 0.107
bam <- bam2r("RedSnapper")
init <- bam$init
init$set_M_constant[1] <- '0.15'
set_M <- init$set_M
nms <- names(set_M)
set_M <- as.numeric(set_M)
set_M <- set_M * 0.15/0.107
set_M <- as.character(set_M)
names(set_M) <- nms
init$set_M <- set_M
bam <- bam2r("RedSnapper", init=init)

BAM <- run_bam(bam=bam)



RSMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                      stock_name='Red Snapper',
                      nsim=nsim,
                      pyears,
                      CAL_bins,
                      Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/RS_Fleet_Structure.rds'),
                      seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/RS_seasonalF.rds'))

## Gag ----
# increase M at max age to 0.25 instead of 0.172
bam <- bam2r("GagGrouper")
init <- bam$init
init$set_M_constant[1] <- '0.25'
set_M <- init$set_M
nms <- names(set_M)
set_M <- as.numeric(set_M)
set_M <- set_M * 0.25/0.172
set_M <- as.character(set_M)
names(set_M) <- nms
init$set_M <- set_M
bam <- bam2r("GagGrouper", init=init)

BAM <- run_bam(bam=bam)

GGMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                      stock_name='Gag Grouper',
                      nsim=nsim,
                      pyears,
                      CAL_bins,
                      Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/GG_Fleet_Structure.rds'),
                      seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/GG_seasonalF.rds'))


## Combine Stocks into Multi-Stock OM ----
MOM <- Combine_OMs(list(RSMOM, GGMOM), Name='Red Snapper & Gag Grouper MOM')

# Add spatial
frac_other <- readRDS('Build_Package/Objects/BaseCase/frac_other.rds')
Frac_Age_Region <- readRDS('Build_Package/Objects/BaseCase/Frac_Age_Region.rds')

OM_03 <- add_spatial_to_OM(MOM,
                           average_prob=list(0.95,0.95),
                           frac_other=list(frac_other,frac_other),
                           Frac_Age_Region)

usethis::use_data(OM_03, overwrite = TRUE)
#
# # ---- OM_04 - Possible over-estimation of general recreational catch ----


## Red Snapper ----

bam <- bam2r("RedSnapper")

# reduced General recreational Landings and Discards by 40% of the values used in Base Case
bam$init$obs_L_rGN <- as.character(0.6 * as.numeric(bam$init$obs_L_rGN))
bam$init$obs_released_rGN <- as.character(0.6 * as.numeric(bam$init$obs_released_rGN))

bam <- bam2r("RedSnapper", init=bam$init)
BAM <- run_bam(bam=bam)

RSMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                      stock_name='Red Snapper',
                      nsim=nsim,
                      pyears,
                      CAL_bins,
                      Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/RS_Fleet_Structure.rds'),
                      seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/RS_seasonalF.rds'))

## Gag ----

bam <- bam2r("GagGrouper")

# reduced General recreational Landings and Discards by 40% of the values used in Base Case
bam$init$obs_L_rGN <- as.character(0.6 * as.numeric(bam$init$obs_L_rGN))
bam$init$obs_released_rGN <- as.character(0.6 * as.numeric(bam$init$obs_released_rGN))

bam <- bam2r("GagGrouper", init=bam$init)
BAM <- run_bam(bam=bam)

GGMOM <- structure_OM(rdat=standardize_rdat(BAM$rdat),
                      stock_name='Gag Grouper',
                      nsim=nsim,
                      pyears,
                      CAL_bins,
                      Fleet_Structure=readRDS('Build_Package/Objects/Stock_data/GG_Fleet_Structure.rds'),
                      seasonal_F_List= readRDS('Build_Package/Objects/Stock_data/GG_seasonalF.rds'))


## Combine Stocks into Multi-Stock OM ----
MOM <- Combine_OMs(list(RSMOM, GGMOM), Name='Red Snapper & Gag Grouper MOM')

# Add spatial
frac_other <- readRDS('Build_Package/Objects/BaseCase/frac_other.rds')
Frac_Age_Region <- readRDS('Build_Package/Objects/BaseCase/Frac_Age_Region.rds')

OM_04 <- add_spatial_to_OM(MOM,
                           average_prob=list(0.95,0.95),
                           frac_other=list(frac_other,frac_other),
                           Frac_Age_Region)

usethis::use_data(OM_04, overwrite = TRUE)


# ---- OM_05 - Increased Variability in Recruitment Process Error ----
OM_05 <- Generate_Future_Rec_Devs(OM_01, sd_multi=c(1.5,1.5))

# compare
get_proj_PE <- function(OM) {
  pyears <- OM@proyears
  tyears <- ncol(OM@cpars[[1]][[1]]$Perr_y)
  nstock <- length(OM@Stocks)
  stocks <- names(OM@Stocks)
  current_yr <- OM@Fleets[[1]][[1]]@CurrentYr
  df_list <- list()
  for (i in 1:nstock) {
    pe <- OM@cpars[[i]][[1]]$Perr_y[,(tyears-pyears+1):tyears]
    dd <- dim(pe)
    df <-data.frame(Stock=stocks[i], Sim=1:dd[1], Year=rep(current_yr+(1:dd[2]), each=dd[1]), PE=as.vector(pe))
    df_list[[i]] <- df
  }
  df <- do.call('rbind', df_list)

  pe1<- df %>% filter(Stock=='Red Snapper', Year==2021)
  pe2<- df %>% filter(Stock=='Red Snapper', Year==2021)

  plot(pe1$PE, pe2$PE)


  df$Sim <- factor(df$Sim)
  df$Stock <- factor(df$Stock, levels=stocks, ordered = TRUE)
  df
}

OM_01_PE <- get_proj_PE(OM_01) %>% filter(Sim%in% c(1,6))
OM_05_PE <- get_proj_PE(OM_05) %>% filter(Sim%in%c(1,6))

OM_01_PE$OM <- 'OM_01'
OM_05_PE$OM <- 'OM_05'

PE_DF <- rbind(OM_01_PE, OM_05_PE)

ggplot(PE_DF, aes(x=Year, y=PE, group_by(Sim), linetype=Sim)) +
  facet_grid(Stock~OM, scales='free') +
  geom_line() +
  expand_limits(y=0) +
  guides(linetype='none') +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=2, color='darkgrey') +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Projection Year', y='Recruitment Process Error')

ggsave(filename='img/OM_Construction/OM_05/RecDevs.png')

usethis::use_data(OM_05, overwrite = TRUE)

# ---- OM_06 - Increased Rec Effort ----
# to be done within the MP
OM_06 <- OM_01
usethis::use_data(OM_06, overwrite = TRUE)


# ---- OM_07 - Lower FL Biomass ----


# Relative Unfished biomass in 'Cape Canaveral - Florida' region is half  the Base Case

# base case assumptions
frac_region_DF <- readRDS('Build_Package/Objects/BaseCase/Spatial_Regional_Dist.rds')
frac_region_DF$Region <- factor(frac_region_DF$Region, ordered=TRUE, levels=unique(frac_region_DF$Region))
frac_region_DF$Stock <- factor(frac_region_DF$Stock, ordered=TRUE, levels=unique(frac_region_DF$Stock))

modify_FL_dist <- function(df, FL_multi, ...) {
  df$Frac_Area <- df$Frac_Area/sum(df$Frac_Area)
  df$Frac_Area[3] <- df$Frac_Area[3] * FL_multi
  df$Frac_Area <- df$Frac_Area/sum(df$Frac_Area)
  df
}

stop('function incorrect. Need to finalize specifications of these OMs')

frac_region_DF2 <- frac_region_DF %>%
  group_by(Stock) %>%
  group_modify(., modify_FL_dist, FL_multi=0.5)%>%
  dplyr::relocate(Stock, .after=Region) %>%
  dplyr::arrange(Region, Stock)




df <- frac_region_DF %>% filter(Stock=='Red Snapper')
df$Frac_Area[3]/df$Frac_Area[2]
df$Frac_Area <- df$Frac_Area/sum(df$Frac_Area)
df$Frac_Area[3] <- df$Frac_Area[3] * FL_multi
df$Frac_Area[1:2] <- df$Frac_Area[1:2] *(1+FL_multi)


df_age_RS <- readRDS('Build_Package/Objects/BaseCase/RS_Age_Depth_Dist.rds')
df_age_GG <- readRDS('Build_Package/Objects/BaseCase/GG_Age_Depth_Dist.rds')
df_age_dist <- dplyr::bind_rows(df_age_RS, df_age_GG)


Frac_Age_Region <- calc_region_depth_dist(df_age_dist, frac_region_DF2)

frac_other <- readRDS('Build_Package/Objects/BaseCase/frac_other.rds')

OM_07 <- add_spatial_to_OM(OM_01,
                         average_prob=list(0.95,0.95),
                         frac_other=list(frac_other,frac_other),
                         Frac_Age_Region)

usethis::use_data(OM_07, overwrite = TRUE)

# ---- OM_08 - Higher FL Biomass ----





# Relative Unfished biomass in 'Cape Canaveral - Florida' region is twice the Base Case

# base case assumptions
frac_region_DF <- readRDS('Build_Package/Objects/BaseCase/Spatial_Regional_Dist.rds')
frac_region_DF$Region <- factor(frac_region_DF$Region, ordered=TRUE, levels=unique(frac_region_DF$Region))
frac_region_DF$Stock <- factor(frac_region_DF$Stock, ordered=TRUE, levels=unique(frac_region_DF$Stock))

frac_region_DF2 <- frac_region_DF %>%
  group_by(Stock) %>%
  group_modify(., modify_FL_dist, FL_multi=2)%>%
  dplyr::relocate(Stock, .after=Region) %>%
  dplyr::arrange(Region, Stock)

frac_region_DF2 <- frac_region_DF2 %>%
  group_by(Stock) %>%
  group_modify(., standardize_areas)%>%
  dplyr::relocate(Stock, .after=Region) %>%
  dplyr::arrange(Region, Stock)


df_age_RS <- readRDS('Build_Package/Objects/BaseCase/RS_Age_Depth_Dist.rds')
df_age_GG <- readRDS('Build_Package/Objects/BaseCase/GG_Age_Depth_Dist.rds')
df_age_dist <- dplyr::bind_rows(df_age_RS, df_age_GG)

Frac_Age_Region <- calc_region_depth_dist(df_age_dist, frac_region_DF2)

frac_other <- readRDS('Build_Package/Objects/BaseCase/frac_other.rds')

OM_08 <- add_spatial_to_OM(OM_01,
                           average_prob=list(0.95,0.95),
                           frac_other=list(frac_other,frac_other),
                           Frac_Age_Region)

usethis::use_data(OM_08, overwrite = TRUE)


