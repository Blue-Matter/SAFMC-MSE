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

# ---- OM_04 - Sporadic Failures in Future Recruitment ----


## Red Snapper ----

OM <- OM_01

sim <- 1
cyear <- OM@Fleets$`Red Snapper`$`Commercial Handline: On-Season`@CurrentYr
nyears <- OM@Stocks$`Red Snapper`@maxage+OM@Fleets$`Red Snapper`$`Commercial Handline: On-Season`@nyears

hstyrs <- rev(seq(cyear, by=-1, length.out=nyears))
proyrs <- seq(cyear+1, by=1, length.out=OM@proyears)
yrs <- c(hstyrs, proyrs)
pe_ind <- (nyears+1):(hist_pe+OM@proyears)
PE_hist <- OM@cpars$`Red Snapper`$`Commercial Handline: On-Season`$Perr_y[sim,1:nyears]
PE_proj <- OM@cpars$`Red Snapper`$`Commercial Handline: On-Season`$Perr_y[sim,pe_ind]


df <- data.frame(Year=yrs,
                 PE=c(PE_hist, PE_proj),
                 Period=c(rep('Historical',nyears), rep('Projection', length(pe_ind))))



ggplot(df, aes(x=yrs, y=PE, color=Period)) +
  geom_line()



nfails <- sample(1:5,1)

failyrs <- sort(sample(1:20, nfails))
PE_proj2 <- PE_proj
PE_proj2[failyrs] <- PE_proj2[failyrs] *0.5* min(PE_hist)

df$PE_fail <- c(PE_hist, PE_proj2)

df <- df %>% tidyr::pivot_longer(., cols=c('PE', 'PE_fail'))
ggplot(df, aes(x=Year, y=value, color=Period)) +
  facet_grid(~name) +
  geom_line()


## Gag Grouper ----










# ---- OM_05 - Sporadic Failures in Future Recruitment ----


# ---- OM_06 - Decline in Growth in Projection Years ----

# ---- OM_07 - Historical Recreational Landings Decreased by 40% ----


# ---- OM_08 - Recreational Effort in Projection years increase by 2% per year ----


# ---- OM_09 - Lower assumed fraction of unfished biomass in southernmost region ----


# ---- OM_10 - Higher assumed fraction of unfished biomass in southernmost region ----

