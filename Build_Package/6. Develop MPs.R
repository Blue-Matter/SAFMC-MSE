library(SAMSE)
source('1a. Initialize.R')

# this script defines some example MMPs


# ---- Define Fleet Management Object ----
Fleet_Management_RS <- Fleet_Details
Fleet_Management_RS$Stock <- 'Red Snapper'

Fleet_Management_GG <- Fleet_Details
Fleet_Management_GG$Stock <- 'Gag Grouper'


Fleet_Management <- bind_rows(Fleet_Management_RS, Fleet_Management_GG)
Fleet_Management$Stock <- factor(Fleet_Management$Stock, levels=c('Red Snapper', 'Gag Grouper', ordered=TRUE))
Fleet_Management$Catch <- NA
Fleet_Management$F <- NA
Fleet_Management$MLL <- NA

usethis::use_data(Fleet_Management, overwrite = TRUE)


# ACL
ACL <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                  ACL_lb=NA,
                  ACL_kg=NA)


usethis::use_data(ACL, overwrite = TRUE)

Allocation_RS <- data.frame(Stock='Red Snapper',
                            Category=c('Comm', 'Rec'),
                            Allocation=NA,
                            MLL=NA)

Allocation_GG <- data.frame(Stock='Gag Grouper',
                            Category=c('Comm', 'Rec'),
                            Allocation=NA,
                            MLL=NA)

Allocation_Sector <- bind_rows(Allocation_RS, Allocation_GG)

usethis::use_data(Allocation_Sector, overwrite = TRUE)


temp_DF <- Fleet_Details %>% filter(Season=='On')
temp_DF$Allocation <- NA
temp_DF_RS <- temp_DF_GG <- temp_DF

temp_DF_RS$Stock <- 'Red Snapper'
temp_DF_GG$Stock <- 'Red Snapper'



Allocation_Fleet <- bind_rows(temp_DF_RS, temp_DF_GG) %>%
  select(Stock, Fleet, Fleets, Category, Season, Allocation)

usethis::use_data(Allocation_Fleet, overwrite = TRUE)

# move this stuff to data-raw

ACL$ACL_lb <- c(444655, 734350)
ACL$ACL_kg <- lb2kg(ACL$ACL_lb)

Allocation_Sector$Allocation <- c(0.2807, 0.7191, 0.51, 0.49)


Allocation_Fleet <- get_Landings(MOM_multiHist) %>%
  dplyr::filter(Sim==1, Year%in%2017:2019) %>%
  dplyr::left_join(., Fleet_Details, by = join_by(Fleet)) %>%
  dplyr::filter(Season=='On') %>%
  dplyr::group_by(Stock, Fleet) %>%
  dplyr::summarise(Catch=mean(Value), .groups='keep') %>%
  dplyr::left_join(., Fleet_Details, by = join_by(Fleet)) %>%
  dplyr::group_by(Stock, Category) %>%
  dplyr::mutate(TotalC=sum(Catch)) %>%
  dplyr::group_by(Stock, Fleet) %>%
  dplyr::mutate(Allocation=Catch/TotalC) %>%
  dplyr::select(-Catch, -TotalC) %>%
  dplyr::group_by(Stock, Category) %>%
  mutate(Allocation=Allocation/sum(Allocation))


# ---- Generic MMP ---

ACL_MMP <- function(x, DataList,  ACL, Allocation_Sector, Allocation_Fleet, ... ) {

  stocks <- ACL$Stock
  nstocks <- length(stocks)

  # Calculate ACL for each On-Season Fleet
  ACL_fleet <- calculate_allocation(ACL, Allocation_Sector, Allocation_Fleet)

  fleets <- unique(ACL_fleet$Fleet)
  nfleets <- length(fleets) # On-Season Fleets

  # Calculate F to catch ACL by Fleet
  F_DF <- Calculate_F(x, DataList,ACL_fleet)

  # Calculate Season Length corresponding with F
  F_DF

  # Set F for Off-Season Fleets

  # maximum change in F (effort)

  Fleet_MMP(x, DataList, F_DF)
}


# F as a function of season length
Fleets <- Fleet_Details %>% filter(Season=='On')

Season_RS <- data.frame(Stock='Red Snapper',
                        Fleet=rep(Fleets$Fleets, each=366),
                        Season_Length=0:365,
                        On_Season_F=NA,
                        Frac_Total_F=NA)



Season_GG <- data.frame(Stock='Gag Grouper',
                        Fleet=rep(Fleets$Fleets, each=366),
                        Season_Length=0:365,
                        On_Season_F=NA,
                        Frac_Total_F=NA)


Season_F <- bind_rows(Season_RS, Season_GG)



# ---- Make Data-Frames ----
F_apical <- get_F(MOM_multiHist) %>% filter(Sim==1, Year>=2009) %>%
  left_join(., Fleet_Details)


Season_Length_RS_Comm <- data.frame(Year=2009:2019,
                                    Stock='Red Snapper',
                                    Category='Comm',
                                    Season_Length=c(365, 3,0,24,44,58,0,0,60,116,53))

Season_Length_RS_Rec <- data.frame(Year=2009:2019,
                                   Stock='Red Snapper',
                                   Category='Rec',
                                   Season_Length=c(c(365, 336, 0,6,3, 8, 0, 0, 9, 6, 5)))

Season_Length_RS <- bind_rows(Season_Length_RS_Comm, Season_Length_RS_Rec)

RS_season_df <- left_join(F_apical, Season_Length_RS, by = join_by(Year, Stock, Category)) %>%
  filter(Stock=='Red Snapper', Fleet!='Commercial Dive') %>%
  dplyr::rename(F=Value) %>%
  mutate(rel_season_length=Season_Length/365)


F_total <- RS_season_df %>% group_by(Year, Fleets) %>%
  summarize(F_total=sum(F), Season_Length=mean(Season_Length))

F_on <- RS_season_df %>%
  filter(Season=='On') %>%
  group_by(Year, Fleets) %>%
  summarize(F_On=sum(F), Season_Length=mean(Season_Length))

DF <- left_join(F_total, F_on) %>%
  mutate(Frac_Total_F=F_On/F_total)


  tidyr::pivot_longer(., cols=c('F_total', 'F_On'))

ggplot(DF, aes(x=Season_Length, y=value, color=name)) +
  facet_grid(name~, scales='free_y') +
  expand_limits(y=0) +
  geom_point() +
  theme_bw()


fit_logistic <- function(df) {
  x <- df$Season_Length
  y <- df$Frac_Total_F
  a <- 1
  fo <- y ~ a / (1 + exp(-b * (x-c)))
  fit <- nls(fo, start = list(b = 0.05, c = 10), control=list(maxiter=500))
  newX <- 0:365
  pred <- predict(fit, newdata = data.frame(x=newX))
  data.frame(Season_Length=newX, Frac_Total_F=pred)
}


DF_Season_list <- DF %>%
  group_by(Fleets) %>%
  group_map(~ fit_logistic(.))


DF_Season_pred <- do.call('rbind', DF_Season_list)
DF_Season_pred$Fleets <- rep(unique(DF$Fleets), each=366)

ggplot(DF) +
  facet_wrap(~Fleets, scales='free_y') +
  geom_point(aes(x=Season_Length, y=Frac_Total_F)) +
  geom_line(data=DF_Season_pred, aes(x=Season_Length, y=Frac_Total_F)) +
  theme_bw() +
  labs(x='Season Length (days)', y='Fraction of Total F in On-Season')





fit_logistic <- function(df) {
  df <- df %>% arrange(x)
  x <- df$F_On
  y <- df$Season_Length/365

  fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=DF,
             control=list(maxiter =500))

  fit <- nls(y ~ SSasympOrig(x, Asym, lrc), data=DF,
             control=list(maxiter =500))

  Asym <- 1
  lrc <- 5.8
  p <- SSasympOrig (x, Asym, lrc)
  plot(x,y)
  lines(x, p)


  fit <- nls(y ~ SSmicmen(x, Vm, K), data=DF,
             control=list(maxiter =500))

  newX <- seq(0,  max(x), by=0.001)
  pred <- predict(fit, newdata = data.frame(x=newX))
  tt <- data.frame(F_On=newX, Season_Length=pred*365)
  lines(tt$F_On, tt$Season_Length)
}


DF_Season_list <- DF %>%
  group_by(Fleets) %>%
  group_map(~ fit_logistic(.))


DF_Season_pred <- do.call('rbind', DF_Season_list)
reps <- unlist(lapply(lapply(DF_Season_list, dim), '[[',1))

fleets <- unique(DF$Fleets)
fleets <- c(rep(fleets[1], reps[1]),
            rep(fleets[2], reps[2]),
            rep(fleets[3], reps[3]))

DF_Season_pred$Fleets <- fleets

ggplot(DF) +
  facet_wrap(~Fleets, scales='free') +
  geom_point(aes(x=F_On, y=Season_Length)) +
  geom_line(data=DF_Season_pred, aes(x=F_On, y=Season_Length)) +
  theme_bw() +
  labs(x='On-Season F', y='Season Length (days)')



# Put these into data.frame

















DF %>% filter(Fleets=='Commercial Handline', Season_Length==365)




  # Predicted On-Season F for each season length (Red Snapper)
  RS_On_Fleets <- c('Commercial Handline: On-Season',
                    'Recreational Headboat: On-Season',
                    'General Recreational: On-Season')
  df <- F_df %>% filter(Stock=='Red Snapper', Fleet %in% RS_On_Fleets)


  df <- left_join(df, RS_Season_LengthDF, by = join_by(Fleet))
  df <- df %>% group_by(Fleet) %>% mutate(val=abs(deltaE-deltaE2))
  pred_Season_Length <- df %>%  group_by(Fleet) %>% filter(val==min(val)) %>%
    distinct(Fleet, Season_Length)

  # Predicted Off-Season F for a given On-Season F
  temp_list <- list()
  for (i in seq_along(RS_On_Fleets)) {
    tempdf <- pred_Season_Length %>% filter(Fleet==RS_On_Fleets[i])
    tempdf2 <- DF_Season_pred  %>% filter(Fleet==RS_On_Fleets[i]) %>%
      dplyr::mutate(val=abs(Season_Length-tempdf$Season_Length)) %>%
      dplyr::filter(val==min(val))

    tempdf3 <- F_df %>% filter(Fleet==RS_On_Fleets[i], Stock=='Red Snapper')
    newF <- tempdf3$newF/tempdf2$relF - tempdf3$newF

    this_fleet <- strsplit(RS_On_Fleets[i], ':')[[1]]
    this_fleet[2] <- 'Off-Season'
    this_fleet <- paste(this_fleet, collapse=": ")

    fl_ind <- match(this_fleet, fleet_names$Fleet)
    histF <- MOM_multiHist[[1]][[fl_ind]]@SampPars$Fleet$FinF[x]

    temp_list[[i]] <- data.frame(Stock='Red Snapper', Fleet=this_fleet, newF=newF, histF=histF)
  }
  F_df_Off_RS <- do.call('rbind', temp_list)
  F_df_Off_RS$deltaE <- F_df_Off_RS$newF/F_df_Off_RS$histF

  F_df_Off_RS <- bind_rows(F_df_Off_RS, data.frame(Stock='Red Snapper',
                                                   Fleet='Commercial Dive',
                                                   deltaE=0))

  # Calculate Off-Season Effort for Gag Grouper
  F_df_Off_GG <- F_df %>% filter(Stock=='Gag Grouper')
  F_df_Off_GG <- left_join(F_df_Off_GG, GG_on_season_relF, by = join_by(Fleet))
  F_df_Off_GG$newF <- F_df_Off_GG$newF/F_df_Off_GG$relF - F_df_Off_GG$newF

  for (fl in 1:nrow(F_df_Off_GG)) {
    this_fleet <- strsplit(F_df_Off_GG$Fleet[fl], ':')[[1]]
    if (length(this_fleet)>1) {
      this_fleet[2] <- 'Off-Season'
      this_fleet <- paste(this_fleet, collapse=": ")
    }

    fl_ind <- match(this_fleet, fleet_names$Fleet)
    histF <- MOM_multiHist[[2]][[fl_ind]]@SampPars$Fleet$FinF[x]
    F_df_Off_GG$histF[fl] <- histF
    F_df_Off_GG$Fleet[fl] <- this_fleet
  }

  F_df_Off_GG$deltaE <- F_df_Off_GG$newF/F_df_Off_GG$histF


  F_df_Off <- bind_rows(F_df_Off_RS, F_df_Off_GG)


  # Gag Grouper
  rec_out <- list()
  # Red Snapper
  rec_out[[1]] <- list()
  # Gag Grouper
  rec_out[[2]] <- list()


  for (s in 1:2) {
    # Commercial Handline - On-Season
    rec_out[[s]][[1]] <- new('Rec')
    newE <-  F_df %>% filter(Stock==stocks[s], Fleet=='Commercial Handline: On-Season')
    rec_out[[s]][[1]]@Effort <- newE$deltaE

    # Recreational Headboat - On-Season
    rec_out[[s]][[2]] <- new('Rec')
    newE <-  F_df %>% filter(Stock==stocks[s], Fleet=='Recreational Headboat: On-Season')
    rec_out[[s]][[2]]@Effort <- newE$deltaE

    # General Recreational - On-Season
    rec_out[[s]][[3]] <- new('Rec')
    newE <-  F_df %>% filter(Stock==stocks[s], Fleet=='General Recreational: On-Season')
    rec_out[[s]][[3]]@Effort <-  newE$deltaE

    # Commercial Handline - Off-Season
    rec_out[[s]][[4]] <- new('Rec')
    newE <-  F_df_Off %>% filter(Stock==stocks[s], Fleet=='Commercial Handline: Off-Season')
    rec_out[[s]][[4]]@Effort <- newE$deltaE

    # Recreational Headboat - Off-Season
    rec_out[[s]][[5]] <- new('Rec')
    newE <-  F_df_Off %>% filter(Stock==stocks[s], Fleet=='Recreational Headboat: Off-Season')
    rec_out[[s]][[5]]@Effort <- newE$deltaE

    # General Recreational - Off-Season
    rec_out[[s]][[6]] <- new('Rec')
    newE <-  F_df_Off %>% filter(Stock==stocks[s], Fleet=='General Recreational: Off-Season')
    rec_out[[s]][[6]]@Effort <- newE$deltaE

    # Commercial Dive
    rec_out[[s]][[7]] <- new('Rec')
    newE <-  F_df_Off %>% filter(Stock==stocks[s], Fleet=='Commercial Dive')
    rec_out[[s]][[7]]@Effort <- newE$deltaE


  }
  rec_out


}
class(Fixed_ACL) <- 'MMP'


# test MP
MOM_multiHist <- readRDS('Hist_Objects/MOM_BaseCase.hist')

MMSE <- ProjectMOM(MOM_multiHist, MPs=c('fixedF_mean3'), dropHist = FALSE)

DataList <- out



# Calculate Distribution of On-Season Catches for Comm and Rec Fleets
Allocation_Fleet <- get_Landings(MOM_multiHist) %>%
  dplyr::filter(Sim==1, Year%in%2017:2019) %>%
  dplyr::left_join(., Fleet_Details, by = join_by(Fleet)) %>%
  dplyr::filter(Season=='On') %>%
  dplyr::group_by(Stock, Fleet) %>%
  dplyr::summarise(Catch=mean(Value), .groups='keep') %>%
  dplyr::left_join(., Fleet_Details, by = join_by(Fleet)) %>%
  dplyr::group_by(Stock, Category) %>%
  dplyr::mutate(TotalC=sum(Catch)) %>%
  dplyr::group_by(Stock, Fleet) %>%
  dplyr::mutate(Allocation=Catch/TotalC) %>%
  dplyr::select(-Catch, -TotalC)


Allocation_RS <- data.frame(Stock='Red Snapper',
                            Category=c('Comm', 'Rec'),
                            Allocation=c(0.2807, 0.7193))

Allocation_GG <- data.frame(Stock='Gag Grouper',
                            Category=c('Comm', 'Rec'),
                            Allocation=c(0.51, 0.49))

Allocation_Sector <- bind_rows(Allocation_RS, Allocation_GG)


# --- Functions ----







# ---- On- and Off-Season Dynamics ----


# Assume General Recreational Effort has increased 30% since 2009 (others remain same)
nyears <- length(2009:2019)
Rel_E <- data.frame(Year=2009:2019,
                    Fleet=rep(c('Commercial Handline: On-Season',
                                'Recreational Headboat: On-Season',
                                'General Recreational: On-Season'), each=nyears),
                    RelE=c(rep(1,nyears),rep(1,nyears), seq(0.7, 1, length.out=nyears)))

RS_season_df <- left_join(RS_season_df, Rel_E, by = join_by(Year, Fleet))
RS_season_df <- RS_season_df %>% mutate(F=F/RelE)

# Assumes Effort (n boats) by fleet has remained relatively constant from 2009:2019

# Estimate Season Length for a given F
RS_On_Fleets <- c('Commercial Handline: On-Season',
                  'Recreational Headboat: On-Season',
                  'General Recreational: On-Season')

Pred_Season_length <- list()
for (i in seq_along(RS_On_Fleets)) {
  DF <- RS_season_df  %>% filter(Fleet==RS_On_Fleets[i])
  if (i>1)
    DF <- RS_season_df  %>% filter(Fleet==RS_On_Fleets[i]) %>% filter(Year!=2010)

  fit <- nls(rel_season_length ~ SSlogis(F, Asym, xmid, scal), data=DF,
             control=list(maxiter =500))

  maxF <- max(DF$F)
  newF <- seq(0, maxF, by=0.0001)

  pred <- predict(fit, newdata = data.frame(F=newF))

  Enow <- RS_season_df  %>% filter(Fleet==RS_On_Fleets[i]) %>% filter(Year==2019)

  df <- data.frame(Fleet=RS_On_Fleets[i],
             F=newF,
             deltaF2=newF/Enow$F,
             Season_Length=pred*365
             )
  Pred_Season_length[[i]] <- df

}

RS_Season_LengthDF <- do.call('rbind', Pred_Season_length)

ggplot(RS_Season_LengthDF, aes(x=F, y=Season_Length)) +
  facet_wrap(~Fleet, scales='free') +
  geom_line() +
  theme_bw() +
  geom_point(data=RS_season_df %>% filter(Season=='On'))


ggplot(RS_Season_LengthDF, aes(x=deltaF2, y=Season_Length)) +
  facet_wrap(~Fleet, scales='free') +
  geom_line() +
  theme_bw()


# Gag Grouper
F_apical <- get_F(MOM_multiHist) %>% filter(Sim==1) %>%
  left_join(., fleet_names)

GG_season <- F_apical %>% filter(Stock=='Gag Grouper', Year>2010) %>%
  group_by(Fleets, Year) %>%
  dplyr::mutate(TotalF=sum(Value, na.rm=TRUE)) %>%
  dplyr::mutate(relF = Value[Season=="On"]/TotalF) %>%
  dplyr::group_by(Fleet, Year) %>%
  dplyr::filter(Season=='On') %>%
  distinct(Year, Fleet, relF)

# Assumed % of Total F taken during fishing season
GG_on_season_relF <- GG_season %>% group_by(Fleet) %>%
  dplyr::summarize(relF=mean(relF))


F_apical %>% filter(Stock=='Gag Grouper', Year==2019)

0.645150612 / (0.645150612+0.002267575)

ggplot(GG_season, aes(x=Year, y=relF)) +
  facet_grid(~Fleet) +
  expand_limits(y=0) +
  geom_line()


# Predict fraction of total effort in On-Season - Red Snapper
DF_Season <- RS_season_df %>%
  group_by(Fleets, Year) %>%
  dplyr::mutate(TotalF=sum(F)) %>%
  dplyr::mutate(relF = F[Season=="On"]/TotalF) %>%
  dplyr::group_by(Fleet, Year) %>%
  dplyr::filter(Season=='On') %>%
  distinct(Year, Season_Length, Fleet, relF)

DF_Season %>% filter(Fleet=='Commercial Handline: On-Season')


fit_logistic <- function(df) {
  x <- df$Season_Length
  y <- df$relF
  a <- 1
  fo <- y ~ a / (1 + exp(-b * (x-c)))
  fit <- nls(fo, start = list(b = 0.05, c = 10), control=list(maxiter=500))
  newX <- 0:365
  pred <- predict(fit, newdata = data.frame(x=newX))
  data.frame(Season_Length=newX, relF=pred)
}


DF_Season_list <- DF_Season %>%
  group_by(Fleet) %>%
  group_map(~ fit_logistic(.))

DF_Season_pred <- do.call('rbind', DF_Season_list)
DF_Season_pred$Fleet <- rep(unique(DF_Season$Fleet), each=366)

ggplot(DF_Season, ) +
  facet_wrap(~Fleet, scales='free_y') +
  geom_point(aes(x=Season_Length, y=relF)) +
  geom_line(data=DF_Season_pred, aes(x=Season_Length, y=relF)) +
  theme_bw()


Effort_RS_On <- data.frame(Stock='Red Snapper',
                           Fleet=c('Commercial Handline: On-Season',
                                   'Recreational Headboat: On-Season',
                                   'General Recreational: On-Season',
                                   'Commercial Dive'),
                           deltaE=1)

Effort_GG_On <- data.frame(Stock='Gag Grouper',
                           Fleet=c('Commercial Handline: On-Season',
                                   'Recreational Headboat: On-Season',
                                   'General Recreational: On-Season',
                                   'Commercial Dive'),
                           deltaE=1)



Effort_On <- bind_rows(Effort_RS_On, Effort_GG_On)



Effort_RS_Off <- data.frame(Stock='Red Snapper',
                           Fleet=c('Commercial Handline: Off-Season',
                                   'Recreational Headboat: Off-Season',
                                   'General Recreational: Off-Season',
                                   'Commercial Dive'),
                           deltaE=1)

Effort_GG_Off <- data.frame(Stock='Gag Grouper',
                           Fleet=c('Commercial Handline: Off-Season',
                                   'Recreational Headboat: Off-Season',
                                   'General Recreational: Off-Season',
                                   'Commercial Dive'),
                           deltaE=1)


Effort_Off <- bind_rows(Effort_RS_Off, Effort_GG_Off)


SizeLimit_RS  <- data.frame(Stock='Red Snapper', Category=c('Comm', 'Rec'), SizeLimit=inch2mm(20))
SizeLimit_GG <- data.frame(Stock='Gag Grouper', Category=c('Comm', 'Rec'), SizeLimit=inch2mm(24))

SizeLimit <- bind_rows(SizeLimit_RS, SizeLimit_GG)

# Max change in effort
# Effort

Season_Length_RS <- data.frame(Stock='Red Snapper',
                               Category=c('Comm', 'Rec'),
                               Season_Length=c(53, 5))
Season_Length_GG <- data.frame(Stock='Gag Grouper',
                               Category=c('Comm', 'Rec'),
                               Season_Length=c(245, 245))

Season_Length <- bind_rows(Season_Length_RS, Season_Length_GG)





Predict_Off_Season_RS <- function(x, DataList, On_Season, DF_Season_pred) {
  RS_season <- Season_Length %>% filter(Stock=='Red Snapper')
  fleets <- as.character(unique(RS_season$Fleet))
  temp_list <- list()
  for (fl in seq_along(fleets)) {
    this_fleet_season <- RS_season %>% filter(Fleet==fleets[fl])
    pred_OS_relF <- DF_Season_pred  %>% filter(Fleet==fleets[fl])

    if (nrow(pred_OS_relF)<1) {
      pred_OffSeason_F <- 0
    } else {
      # fraction of total F in On-Season
      predict_rel_F <- pred_OS_relF %>%
        dplyr::mutate(val=abs(Season_Length-this_fleet_season$Season_Length)) %>%
        dplyr::filter(val==min(val))

      newF <- On_Season %>% filter(Stock=='Red Snapper', Fleet==fleets[fl])
      pred_OffSeason_F <- newF$newF/predict_rel_F$relF - newF$newF
    }

    this_fleet <- strsplit(fleets[fl], ':')[[1]]
    if (length(this_fleet)>1) {
      this_fleet[2] <- 'Off-Season'
      this_fleet <- paste(this_fleet, collapse=": ")
    }

    fl_ind <- match(this_fleet, fleet_names$Fleet)
    histF <- DataList[[1]][[fl_ind]]@Misc$FleetPars$FinF[x]

    temp_list[[fl]] <- data.frame(Stock='Red Snapper',
                                  Fleet=this_fleet,
                                  histF=histF,
                                  newF=pred_OffSeason_F,
                                  deltaE=pred_OffSeason_F/histF)

  }
  do.call('rbind', temp_list)
}

Predict_Off_Season_GG <- function(x, DataList, On_Season, GG_on_season_relF) {
  F_df_Off_GG <- On_Season %>% filter(Stock=='Gag Grouper')
  F_df_Off_GG <- left_join(F_df_Off_GG, GG_on_season_relF, by = join_by(Fleet))
  F_df_Off_GG$newF <- F_df_Off_GG$newF/F_df_Off_GG$relF - F_df_Off_GG$newF
  for (fl in 1:nrow(F_df_Off_GG)) {
    this_fleet <- strsplit(F_df_Off_GG$Fleet[fl], ':')[[1]]
    if (length(this_fleet)>1) {
      this_fleet[2] <- 'Off-Season'
      this_fleet <- paste(this_fleet, collapse=": ")
    }

    fl_ind <- match(this_fleet, fleet_names$Fleet)
    histF <- MOM_multiHist[[2]][[fl_ind]]@SampPars$Fleet$FinF[x]
    F_df_Off_GG$histF[fl] <- histF
    F_df_Off_GG$Fleet[fl] <- this_fleet
  }

  F_df_Off_GG$deltaE <- F_df_Off_GG$newF/F_df_Off_GG$histF

  F_df_Off_GG %>% dplyr::select(Stock, Fleet, histF, newF, deltaE=deltaE)

}


Set_Season_Effort <- function(x, DataList, Effort_On) {
  On_Season <- Effort_On
  stocks <- unique(On_Season$Stock)
  nstocks <- length(stocks)
  fleets <- unique(On_Season$Fleet)
  nfleets <- length(fleets) # On-Season Fleets
  F_list <- list()
  for (st in 1:nstocks) {
    temp_list <- list()
    for (fl in 1:nfleets) {
      this_stock <- stocks[st]
      this_fleet <- fleets[fl]
      fl_ind <- match(this_fleet, fleet_names$Fleet)
      histF <- DataList[[st]][[fl_ind]]@Misc$FleetPars$FinF[x]
      temp_list[[fl]] <- data.frame(Stock=this_stock, Fleet=this_fleet,histF=histF)
    }
    F_list[[st]] <- do.call('rbind', temp_list)
  }
  F_df <- do.call('rbind', F_list)
  On_Season <- left_join(On_Season, F_df, by = join_by(Stock, Fleet))
  On_Season$newF <- On_Season$histF * On_Season$deltaE
  On_Season %>% dplyr::select(Stock, Fleet, histF, deltaE, newF)
}

Calculate_On_Season_Effort <- function(x, DataList, y, ACL, Allocation_Sector, Allocation_Fleet, SizeLimit) {
  # Set an ACL for each On-Season Fleet
  stocks <- ACL$Stock
  nstocks <- length(stocks)
  ACL_fleet <- calculate_allocation(ACL, Allocation_Sector, Allocation_Fleet)
  fleets <- unique(ACL_fleet$Fleet)
  nfleets <- length(fleets) # On-Season Fleets


  # Calculate F for the On-Season Fleets to catch ACL_fleet
  F_list <- list()
  for (st in 1:nstocks) {
    temp_list <- list()
    for(fl in 1:nfleets) {
      newF <- Calculate_F(x,st,fl,stocks, fleets, y, ACL_fleet, DataList, SizeLimit)
      this_stock <- newF$Stock
      this_fleet <- newF$Fleet
      fl_ind <- match(this_fleet, fleet_names$Fleet)
      histF <- DataList[[st]][[fl_ind]]@Misc$FleetPars$FinF[x]
      temp_list[[fl]] <- data.frame(Stock=this_stock, Fleet=this_fleet, newF=newF$F, histF=histF)
    }
    F_list[[st]] <- do.call('rbind', temp_list)
  }
  F_df <- do.call('rbind', F_list)

  # Required in Effort from last historical year to catch ACL
  F_df$deltaE <- F_df$newF/F_df$histF
  F_df$deltaE[!is.finite(F_df$deltaE)] <- 0
  F_df %>% dplyr::select(Stock, Fleet, histF, deltaE, newF)
}




current_E <- function(x, DataList, ... ) {
  rec_out <- list()
  # Red Snapper
  rec_out[[1]] <- list()
  # Gag Grouper
  rec_out[[2]] <- list()


  for (s in 1:2) {
    # Commercial Handline - On-Season
    rec_out[[s]][[1]] <- new('Rec')
    rec_out[[s]][[1]]@Effort <- 1

    # Recreational Headboat - On-Season
    rec_out[[s]][[2]] <- new('Rec')
    rec_out[[s]][[2]]@Effort <- 1

    # General Recreational - On-Season
    rec_out[[s]][[3]] <- new('Rec')
    rec_out[[s]][[3]]@Effort <- 1

    # Commercial Handline - Off-Season
    rec_out[[s]][[4]] <- new('Rec')
    rec_out[[s]][[4]]@Effort <- 1

    # Recreational Headboat - Off-Season
    rec_out[[s]][[5]] <- new('Rec')
    rec_out[[s]][[5]]@Effort <- 1

    # General Recreational - Off-Season
    rec_out[[s]][[6]] <- new('Rec')
    rec_out[[s]][[6]]@Effort <- 1

    # Commercial Dive
    rec_out[[s]][[7]] <- new('Rec')
    rec_out[[s]][[7]]@Effort <- 1
  }
  rec_out

}
class(current_E) <- 'MMP'

# ---- Testing ----

MMSE <- ProjectMOM(MOM_multiHist, MPs=c('Fixed_ACL', 'current_E'), dropHist = FALSE)

Removals <- get_Removals(MMSE) %>% filter(Sim==1, Stock=='Gag Grouper')

Removals <- left_join(Removals, fleet_names)

ggplot(Removals, aes(x=Year, y=Value, color=Fleet)) +
  facet_grid(~MP, scales='free_y') +
  geom_line()

Removals <- get_Removals(MMSE) %>% filter(Sim==1, Stock=='Red Snapper')

Removals <- left_join(Removals, fleet_names)

ggplot(Removals %>% filter(Year>=2010), aes(x=Year, y=Value, color=Fleets, linetype=MP)) +
  facet_grid(Season~Category, scales='free_y') +
  geom_line()


Biomass <- get_Biomass(MMSE) %>% filter(Sim==1)

ggplot(Biomass, aes(x=Year, y=Value)) +
  facet_grid(MP~Stock) +
  geom_line()



fl <- tempfile()
fl
saveRDS(MMSE, fl)

Landings <- get_Landings(MMSE) %>% filter(Sim==1, Stock=='Red Snapper', Year%in%2009:2025)
Removals <- get_Removals(MMSE) %>% filter(Sim==1, Stock=='Red Snapper', Year%in%2009:2025)

t1 <- Landings %>%  filter(Fleet=='Commercial Handline: On-Season')
tt <- Removals %>%  filter(Fleet=='Commercial Handline: Off-Season')

par(mfrow=c(1,2))
plot(t1$Year, t1$Value, type='b')
lines(tt$Year, tt$Value, type='b', col='blue')



left_join(Landings, ACL_fleet %>% filter(Stock=='Red Snapper')) %>%
  dplyr::select(Fleet, Value, ACL_fleet)

Landings <- get_Landings(MMSE) %>% filter(Sim==1, Stock=='Gag Grouper', Year%in%2020)
left_join(Landings, ACL_fleet %>% filter(Stock=='Gag Grouper')) %>%
  dplyr::select(Fleet, Value, ACL_fleet)


Removals <- get_Removals(MMSE) %>% filter(Sim==1, Stock=='Red Snapper', Year%in% 2013:2020)

Removals %>% filter(Fleet=='Commercial Handline: On-Season')
Removals %>% filter(Fleet=='Commercial Handline: Off-Season')

get_F(MOM_multiHist) %>% filter(Sim==1, Stock=='Red Snapper', Year %in% 2013)

0.01423750 / (0.01423750+0.039390941)



left_join(Landings, ACL_fleet %>% filter(Stock=='Red Snapper')) %>%
  dplyr::select(Fleet, Value, ACL_fleet)



ACL_fleet <- calculate_allocation(ACL, Allocation_Sector, Allocation_Fleet)
ACL_fleet$Fleet <- factor(ACL_fleet$Fleet, levels=fleet_names$Fleet, ordered = TRUE)


removals_project <- get_Removals(MMSE)

removals_project %>% filter(Stock=='Red Snapper', Sim==1, Year%in%2020)
ACL_fleet


tt <- removals_project %>% filter(Stock=='Red Snapper', Sim==1, Year%in%2017:2020)
tt$Fleet <- factor(tt$Fleet, levels=fleet_names$Fleet, ordered = TRUE)

ggplot(tt %>% filter(Fleet!='Commercial Dive'), aes(x=Year, y=Value)) +
  facet_wrap(~Fleet, scales='free_y') +
  geom_line()



ACL_fleet %>% filter(Stock=='Red Snapper')


data.frame(Catch=MMSE@Catch[1,1,c(1,2,3,7),1,1] %>% round(2), ACL_fleet %>% filter(Stock=='Red Snapper')) %>%
 dplyr::select(Catch, ACL_fleet)

data.frame(Catch=MMSE@Catch[1,2,c(1,2,3,7),1,1] %>% round(2), ACL_fleet %>% filter(Stock1='Red Snapper'))

fleet_names

MOM_multiHist[[2]][[4]]@SampPars$Fleet$FinF


# Plot past and projected removals



# DataList <- readRDS("C:/Users/User/Documents/GitHub/SAFMC-MSE/Hist_Objects/DataList.rda")

x <- 1



# Next:
# - fix catches for Gag - should match TAC
# - seasonal dynamics
# - effort dynamics
# - changes to effort
# - size limits


# MOM_RS_GG <- readRDS('Hist_Objects/BaseCaseMOM.mom')
# MOM_RS_GG@nsim <- 3
# MOM_multiHist <- SimulateMOM(MOM_RS_GG)







MMSE@Catch[1,1,c(1,2,3,7),1,1]

ACL_fleet$Red_Snapper

MMSE@Catch[1,1,c(1,2,3,7),1,1]
ACL_fleet$Red_Snapper

data.frame(Catch=MMSE@Catch[1,2,c(1,2,3,7),1,1] %>% round(2), ACL_fleet$Gag )



Landings <- get_Landings(MMSE) %>% filter(Sim==1, Stock=='Gag Grouper', Year%in% 2019:2020)


Removals <- get_Removals(MMSE) %>% filter(Sim==1, Stock=='Red Snapper', Year%in% 2019:2020)

catchdf<- Landings %>% filter(Year==2019, Fleet %in% c('Commercial Handline: On-Season',
                                             'Recreational Headboat: On-Season',
                                             'General Recreational: On-Season',
                                             'cDV'))


catchdf <- catchdf %>% mutate(Landings_LB=kg2lb(Value))


sum(catchdf$Landings_LB[c(1,4)])/347301

sum(catchdf$Landings_LB[c(2,3)])/sum(catchdf$Landings_LB)

sum(catchdf$Landings_LB[c(1,4)])/sum(catchdf$Landings_LB)


catchdf$Landings_LB
347301

kg2lb(ACL_fleet$Red_Snapper)[1:3]

ACL_lb$Red_Snapper



detach(package:SAMSE,unload=TRUE)
detach(package:openMSE,unload=TRUE)


Removals <- openMSE::get_Removals(MOM_multiHist)
Landings <- openMSE::get_Landings(MOM_multiHist)

Removals <- Removals %>% filter(Sim==1) %>% group_by(Stock, Year, Fleet) %>% summarize(Catch=sum(Value))
Removals$On.Season <- grepl('On-Season', Removals$Fleet)
Removals$On.Season[ grepl('cDV', Removals$Fleet)==TRUE] <- TRUE

df <- Removals %>% dplyr::filter(Year==2019, On.Season==TRUE) %>%
  group_by(Stock) %>%
  mutate(Catch_frac=Catch/sum(Catch))

df



















st <- 1
y <- 70

B_at_Age <- rowSums(DataList[[1]][[1]]@Misc$StockPars$Biomass[x,st,,y,] )
Sel_at_Age <- DataList[[1]][[1]]@Misc$FleetPars$retA_real

V <- DataList[[1]][[1]]@Misc$FleetPars$V_real[x,,y]
retA <- DataList[[1]][[1]]@Misc$FleetPars$retA_real[x,,y]

DataList[[1]][[2]]@Misc$FleetPars$V_real[x,,y]
DataList[[1]][[3]]@Misc$FleetPars$V_real[x,,y]
DataList[[1]][[7]]@Misc$FleetPars$V_real[x,,y]


F1 <- Calculate_F(1,1,1,70, ACL_fleet, DataList)
F2 <- Calculate_F(1,1,2,70, ACL_fleet, DataList)
F3 <- Calculate_F(1,1,3,70, ACL_fleet, DataList)
F4 <- Calculate_F(1,1,4,70, ACL_fleet, DataList)

Flist <- c(F1, F2, F3, F4)
Flist/sum(Flist)
ACL_fleet$Red_Snapper/sum(ACL_fleet$Red_Snapper)

Fmat <- matrix(0, 21, 4)
Fret <- Fmat
for (fl in 1:4) {
  this_fleet <- names(ACL_fleet[[st]][fl])

  fl_ind <- match(this_fleet, fleet_names$Fleet)
  V_real <- DataList[[st]][[fl_ind]]@Misc$FleetPars$V_real[x,,y]
  RetA_real <- DataList[[st]][[fl_ind]]@Misc$FleetPars$retA_real[x,,y]

  Fmat[,fl] <- Flist[[fl]] *V_real
  Fret[,fl] <- Flist[[fl]] * RetA_real

}


Zmat <- rowSums(Fmat) + M_at_Age
predC <- Fret/Zmat * (1-exp(-Zmat)) * B_at_Age # predicted retained catch

apply(predC, 2, sum)
ACL_fleet$Red_Snapper

catch <- get_Landings(MOM_multiHist) %>% filter(Year==2019, Stock=='Red Snapper', Sim==1)
catch

kg2lb(63632.417)
kg2lb(11945.750)

fl <- 1
ACL_sector$Red_Snapper
ACL_fleet$Red_Snapper




# General Recreational - On-Season
rec_out[[s]][[5]] <- new('Rec')
predE <- pred_DF %>% filter(Day==new_season)


e1 <- GNrec_current$Rel_E_On_Season * cureE_genR
e1a <- GNrec_current$Rel_E_Off_Season * cureE_genR
e2 <- predE$Pred * cureE_genR
e3 <- (1-predE$Pred) * cureE_genR


rec_out[[s]][[5]]@Effort <-  e2 / e1

# General Recreational - Off-Season
rec_out[[s]][[6]] <- new('Rec')
rec_out[[s]][[6]]@Effort <- e3 / e1a


# ---- Fleet Dynamics ----
RS_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_RS.csv'))
GG_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_GG.csv'))

RS_logbook_SA <- RS_logbook_all %>% filter(Trip.Region == 'SA')
GG_logbook_SA <- GG_logbook_all %>% filter(Trip.Region == 'SA')


RS_logbook_SA <- RS_logbook_SA %>% filter(LandingYear %in% 2009:2019) %>%
  dplyr::select(LandingYear, Vessel.Name, Total.HrsFished) %>%
  dplyr::mutate(Species='Red Snapper')

GG_logbook_SA <- GG_logbook_SA %>% filter(LandingYear %in% 2009:2019) %>%
  dplyr::select(LandingYear, Vessel.Name, Total.HrsFished) %>%
  dplyr::mutate(Species='Gag Grouper')


RS_logbook_SA %>% filter(LandingYear==2019) %>% dplyr::select(TotalSoakTime)

df <- bind_rows(RS_logbook_SA, GG_logbook_SA)
tt <- df %>% group_by(LandingYear, Species) %>%
  dplyr::summarize(E=sum(Total.HrsFished,  na.rm=TRUE)) %>%
  dplyr::group_by(LandingYear) %>%
  dplyr::mutate(E2=E/max(E, na.rm=TRUE)) %>%
  dplyr::rename(Year=LandingYear, Stock=Species)
tt

tt %>% filter(Year==2019)

ggplot(tt, aes(x=Year, y=E, color=Stock)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


ggplot(tt, aes(x=Year, y=E2, color=Stock)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


Biomass <- get_Biomass(MOM_multiHist) %>% filter(Sim==1, Year %in% 2009:2019) %>%
  rename(Biomass=Value) %>%
  group_by(Year) %>%
  mutate(Rel_Biomass=Biomass[Stock=='Red Snapper']/Biomass[Stock!='Red Snapper'])

Biomass <- left_join(Biomass, tt)


ggplot(Biomass, aes(x=Year, y=Biomass)) +
  facet_wrap(~Stock) +
  geom_line()


ggplot(Biomass %>% filter(Stock=='Red Snapper'),
       aes(x=Biomass, y=E)) +
  geom_point() +
  geom_line() +
  theme_bw()

redf <- Biomass %>% filter(Stock=='Red Snapper')
redf <- left_join(redf, DF_Season)
redf <- redf %>% mutate(Biomass2=Biomass*relF)

ggplot(redf, aes(x=Biomass, y=E2)) +
  facet_wrap(~Stock) +
  expand_limits(y=0) +
  geom_line()




redf <- Biomass %>% filter(Stock=='Gag Grouper')
redf <- left_join(redf, DF_Season)
redf <- redf %>% mutate(Biomass2=Biomass)

ggplot(redf, aes(x=Biomass2, y=E2)) +
  facet_wrap(~Stock) +
  expand_limits(y=0) +
  geom_line()

RS_logbook_SA %>% filter(LandingYear==2009) %>% distinct(VesselOfficialNumber, Vessel.Name) %>%
  arrange(Vessel.Name)







ggplot(RS_season_df,
       aes(x=Season_Length, y=F)) +
  facet_grid(Season~Fleets, scales='free_y') +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Year)) +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw()


ggplot(RS_season_df %>% filter(Season=='On'),
       aes(x=Season_Length, y=F)) +
  facet_wrap(~Fleet, scales='free_y') +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Year)) +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw()


ggplot(RS_season_df %>% filter(Season=='On', Fleet=='Recreational Headboat: On-Season'),
       aes(x=Season_Length, y=F)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Year)) +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw()


ggplot(RS_season_df %>% filter(Season=='On', Fleet=='General Recreational: On-Season'),
       aes(x=Season_Length, y=F)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=Year)) +
  geom_smooth(method='lm', formula= y~x) +
  theme_bw()


RS_season_df <- RS_season_df %>% mutate(logF=log(F))

ggplot(RS_season_df %>%
         filter(Season=='On', Fleet=='General Recreational: On-Season'),
       aes(x=Season_Length, y=logF)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)


# Fit LM
LMs <- RS_season_df  %>%
  filter(Season=='On') %>%
  group_by(Fleet) %>%
  group_map(~ (lm(F ~ Season_Length, data=.x)))


tt= predict(LMs[[2]], newdata = data.frame(Season_Length=0:365), se.fit = TRUE)
plot(0:365, tt$fit)


which.min(abs(tt$fit-0.07066176))
which.min(abs(tt$fit-0.09086075))






# use this for predicting days
#


tt <- DF_Season_pred %>% filter(Fleet=='Commercial Handline: On-Season')
nrow(tt)
plot(tt$Season_Length, tt$relF)


plot(tt$Season_Length, tt$relF)


df <- tt %>% filter(Fleet=='Commercial Handline: On-Season')
df <- tt %>% filter(Fleet=='Recreational Headboat: On-Season')
df <- tt %>% filter(Fleet=='General Recreational: On-Season')

plot(t1[[3]])
fit_logistic(df)

fit_logistic <- function(df) {
  x <- df$Season_Length
  y <- df$relF
  a <- 1
  fo <- y ~ a / (1 + exp(-b * (x-c)))
  fit <- nls(fo, start = list(b = 0.05, c = 10))
  newX <- 0:365
  pred <- predict(fit, newdata = data.frame(x=newX))
  data.frame(Season_Length=newX, relF=pred)
}

df1_F <- F_apical %>% filter(Fleet=="Commercial Handline: On-Season", Stock=='Red Snapper')
df2_F <- F_apical %>% filter(Fleet=="Commercial Handline: Off-Season", Stock=='Red Snapper')
df1_F$relF <- df1_F$Value/(df1_F$Value+df2_F$Value)

df <- data.frame(x=Season_Length_RS_Comm$Season_Length, y=df1_F$relF) %>% arrange(x)
plot(df$x, df$y)
x <- df$x
y <- df$y



lines(newX, pred, col='blue')


fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=df)
summary(fit)

newX <- 0:365
pred <- predict(fit, newdata = data.frame(x=newX))
lines(newX, pred, col='blue')


df <- data.frame(x=Season_Length_RS_Comm$Season_Length, y=df1_F$Value) %>% arrange(x)

plot(df$x,df$y)
mod <- lm(y~x, data=df)

lines(newX, predict(mod, newdata=data.frame(x=newX)))


Removals <- get_Removals(MOM_multiHist) %>% filter(Sim==1, Year>=2009)


df1_C <- Removals %>% filter(Fleet=="Commercial Handline: On-Season", Stock=='Red Snapper')
df2_C <- Removals %>% filter(Fleet=="Commercial Handline: Off-Season", Stock=='Red Snapper')

df1_C$relC <- df1_C$Value/(df1_C$Value+df1_C$Value)











head(Com_On_RS)
fl <- 1


Com_On_RS <- data.frame(Year=1950:2019,
                        Stock='Red Snapper',
                        Fleet=fleets[fl],
                        C=rowSums(MOM_multiHist[[1]][[fl]]@TSdata$Removals[1,,]),
                        F=MOM_multiHist[[1]][[fl]]@SampPars$Fleet$Find[1,],
                        B=rowSums(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,,]),
                        Season='On-Season')

Com_Off_RS <- data.frame(Year=1950:2019,
                         Stock='Red Snapper',
                         Fleet=fleets[fl+3],
                         C=rowSums(MOM_multiHist[[1]][[fl+3]]@TSdata$Removals[1,,]),
                         F=MOM_multiHist[[1]][[fl+3]]@SampPars$Fleet$Find[1,],
                         B=rowSums(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,,]),
                         Season='Off-Season')


Com_On_RS <- Com_On_RS %>% filter(Year>=2009)
Com_Off_RS <- Com_Off_RS %>% filter(Year>=2009)

Com_On_RS$season_length <- c(365, 3,0,24,44,58,0,0,60,116,53)

Com_On_RS$relC <- Com_On_RS$C/(Com_On_RS$C+Com_Off_RS$C)
Com_On_RS$relF <- Com_On_RS$F/(Com_On_RS$F+Com_Off_RS$F)


x <- Com_On_RS$season_length
y <- Com_On_RS$relF
df <- data.frame(x=x, y=y) %>% arrange(x)
df <- df[3:10,]
plot(df$x, df$y)

fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=df)
summary(fit)

newX <- 0:365
pred <- predict(fit, newdata = data.frame(x=newX))
lines(newX, pred, col='blue')




MOM_multiHist <- readRDS('Hist_Objects/MOM_BaseCase.hist')

MOM_RS_GG <- readRDS('Hist_Objects/BaseCaseMOM.mom')
MOM_RS_GG@nsim <- 3
MOM_multiHist <- SimulateMOM(MOM_RS_GG)



names(MOM_multiHist[[1]])





Removals <- openMSE::get_Removals(MOM_multiHist)
Landings <- openMSE::get_Landings(MOM_multiHist)

head(Landings)

Catch <- Removals %>% filter(Sim==1) %>% group_by(Stock, Year, Fleet) %>% summarize(Catch=sum(Value))
Catch$On.Season <- grepl('On-Season', Catch$Fleet)

Catch$Fleet <- factor(Catch$Fleet)
df <- Catch %>% dplyr::filter(Stock=='Red Snapper', Year%in%2017:2019, On.Season==TRUE) %>%
  dplyr::group_by(Fleet) %>%
  mutate(Catch_frac=Catch/sum(Catch))

  dplyr::summarize(Catch_frac=Catch/sum(Catch))

df

df[which(grepl('On-Season', df$Fleet )),] %>% mutate(Catch_frac=Catch_lb/sum(Catch_lb))


df <- data.frame(Var=c(rep('a',3), rep('b',3),rep('c',3)),
                 Val=c(rnorm(3), rnorm(3), rnorm(3)))

df %>% dplyr::group_by(Var) %>% dplyr::summarize(Val=Val/sum(Val))
df

df$Var <- factor(df$Var)


sum(df$Catch_frac[1:2])
sum(df$Catch_frac[1:2])

# TAC allocation

928924/(928924+148993)

148993/124815
140286/124815


ggplot(Catch, aes(x=Year, y=Catch)) +
  facet_grid(~Model) +
  geom_line()


Removals <- Removals %>% filter(Sim==1) %>%  group_by(Model, Year) %>% summarize(Catch=sum(Value))
ggplot(Removals, aes(x=Year, y=Catch/1000)) +
  facet_grid(~Model) +
  geom_line()




# TO DO:
# - calculate relative targeting of RS for Commercial and Recreational Fleets
# - calculate impact of season length
# - develop MMP
# - run projections
# - make plots



# ---- Commercial Handline ----

# RS price: 6.75 per lb # South Atlantic https://www.nationalfisherman.com/gulf-south-atlantic/gulf-south-atlantic-market-report-2019-year-in-review#:~:text=Red%20snapper%3A%20The%20Gulf%20of,of%20nearly%207%20million%20pounds.
# GG price: 6.10 per lb # Gulf of Mexico https://media.fisheries.noaa.gov/2023-01/Gag%20Interim%20Rule_EA%20_final508_01182023.pdf



# ---- target by relative price ---

RS_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_RS.csv'))
GG_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_GG.csv'))

RS_logbook_SA <- RS_logbook_all %>% filter(Trip.Region == 'SA')
GG_logbook_SA <- GG_logbook_all %>% filter(Trip.Region == 'SA')

RS_logbook_SA <- RS_logbook_SA %>% mutate(ReportedDead=as.numeric(ReportedDead),
                                          ReportedDiscardDead=as.numeric(ReportedDiscardDead),
                                          ReportedDiscard.Alive=as.numeric(ReportedDiscard.Alive),
                                          N=as.numeric(WholePounds)/9.71,
                                          Species='Red Snapper') %>%
  select(Year=LandingYear, Species,ReportedDead, ReportedDiscardDead, ReportedDiscard.Alive, N)

GG_logbook_SA <- GG_logbook_SA %>%  mutate(ReportedDead=as.numeric(ReportedDead),
                                           ReportedDiscardDead=as.numeric(ReportedDiscardDead),
                                           ReportedDiscard.Alive=as.numeric(ReportedDiscard.Alive),
                                           N=as.numeric(WholePounds)/20,
                                           Species='Gag') %>%
  select(Year=LandingYear, Species,ReportedDead, ReportedDiscardDead, ReportedDiscard.Alive, N)

df <- bind_rows(RS_logbook_SA, GG_logbook_SA) %>% filter(Year>=1992, Year<=2019) %>%
  group_by(Year, Species) %>%
  summarize(N=sum(ReportedDiscardDead, na.rm=TRUE)+sum(ReportedDiscard.Alive, na.rm=TRUE)+
              sum(N, na.rm = T))

season_length_RS <- data.frame(Year=1992:2019, Species='Red Snapper',
                               Season_Length=c(rep(366,18), 3,0,24,44,53,0,0,60,116, 53))

season_length_GG <- data.frame(Year=1992:2019, Species='Gag',
                               Season_Length=c(rep(366,7), rep(304,11), 245, 245,180,
                                               196, 204, 170, 245, 245, 245, 245))

season_length <- bind_rows(season_length_RS, season_length_GG)

df <- left_join(df, season_length)
df$N <- df$N/df$Season_Length


ggplot(df, aes(x=Year, y=N, color=Species)) +
  geom_line()

ind <- which(Years$Year %in% 1992:2019)
t1 <- df %>% filter(Species=='Red Snapper')
relE_RS <- t1$N/ apply(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,ind,], 1, sum)

t2 <- df %>% filter(Species=='Gag')
relE_GG <- t2$N/ apply(MOM_multiHist[[2]][[1]]@TSdata$Biomass[1,ind,], 1, sum)


b1 <- apply(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,ind,], 1, sum)/MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$B0[1]
b2 <- apply(MOM_multiHist[[2]][[1]]@TSdata$Biomass[1,ind,], 1, sum)/MOM_multiHist[[2]][[1]]@Ref$ReferencePoints$B0[1]


relB <- b1/b2
relE <- relE_RS / (relE_RS+relE_GG)

plot(relB, relE, type='p', ylim=c(0,1), xlim=c(0,3))

df <- data.frame(x=relB, y=relE) %>% arrange(x)
plot(df$x, df$y,  xlim=c(0,3))
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=df)
summary(fit)
newX <- 0:3
pred <- predict(fit, newdata = data.frame(x=newX))
lines(newX, pred, col='blue')




# ---- Gen Rec ----
logbook_all <- read.csv(file.path(data.dir, 'SE Headboat trip data 12 species.csv'))
logbook_RS_GG <- logbook_all %>% filter(Species.Name %in% c('RED SNAPPER', 'GAG'))


df <- logbook_RS_GG %>% filter(Year>=1990, Year<=2019) %>% group_by(Year, Species.Name) %>%
  mutate(Numkept=as.numeric(Numkept)) %>%
  summarize(N=sum(Numkept+ReleasedDead+ReleasedLive, na.rm=TRUE))

ggplot(df, aes(x=Year, y=N, color=Species.Name)) +
  geom_line()


t1 <- df %>% filter(Species.Name=='RED SNAPPER')
relE_RS <- t1$N/ apply(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,ind,], 1, sum)

t2 <- df %>% filter(Species.Name=='GAG')
relE_GG <- t2$N/ apply(MOM_multiHist[[2]][[1]]@TSdata$Biomass[1,ind,], 1, sum)


b1 <- apply(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,ind,], 1, sum)/MOM_multiHist[[1]][[1]]@Ref$ReferencePoints$B0[1]
b2 <- apply(MOM_multiHist[[2]][[1]]@TSdata$Biomass[1,ind,], 1, sum)/MOM_multiHist[[2]][[1]]@Ref$ReferencePoints$B0[1]

relB <- b1/b2
relE <- relE_RS / (relE_RS+relE_GG)

plot(relB, relE, type='b', ylim=c(0,1), xlim=c(0,3))

# ---- Calculate Relative Effort by Season Length ----
# Red Snapper

years <- unique(RS_Rec_Season$Year)
GNrec <- data.frame(Year=years, Rel_E_On_Season=NA, Season_Length=c(336, 0,6,3, 8, 0, 0, 9, 6, 5))
for (i in seq_along(years)) {
  GNrec$Rel_E_On_Season[i] <- calc_relEfforts_RS_HB(years[i], logbook=RS_logbook_SA, season_df=RS_Rec_Season)
}

GNrec$Rel_E_Off_Season <- 1- GNrec$Rel_E_On_Season
GNrec_current <- GNrec %>% tail(3) %>% summarise(Rel_E_On_Season=mean(Rel_E_On_Season), Season_Length=mean(Season_Length))
GNrec_current$Rel_E_Off_Season <- 1- GNrec_current$Rel_E_On_Season


df <- data.frame(x=GNrec$Season_Length, y=GNrec$Rel_E_On_Season) %>% arrange(x)
plot(df$x, df$y)
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=df)
summary(fit)
newX <- 0:365
pred <- predict(fit, newdata = data.frame(x=newX))
lines(newX, pred, col='blue')

pred_DF <- data.frame(Day=newX, Pred=as.vector(pred))

new_season <- 2

calc_relEfforts_RS_HB <- function(year, logbook, season_df) {

  df <- logbook %>% filter(Year ==year)
  df$Month <- df$Month
  df$Day <- df$Day
  df$Date <- lubridate::dmy(paste(df$Day, df$Month, df$Year, sep='-'))
  df$open <- FALSE


  # Assign open/closed season
  open_dates <- season_df %>% filter(Year ==year)
  for (i in 1:nrow(open_dates)) {
    if (!is.na(open_dates$Date_Open[i])) {
      int <- lubridate::interval(open_dates$Date_Open[i], open_dates$Date_Closed[i]-1)
      df$open[df$Date %within% int] <- TRUE
    }
  }


  # Dead discards
  disc_mort <- unique(open_dates$Disc_M)

  df <- df %>% mutate(Numkept=as.numeric(Numkept),
                      Discard_Alive=as.numeric(ReleasedLive),
                      Discard_Dead=as.numeric(ReleasedDead),
                      Discard_D_Total=(Discard_Alive*disc_mort)+Discard_Dead)



  on_season <- df %>% filter(open==TRUE) %>%
    summarize(Interaction=sum(Numkept, Discard_Alive, ReleasedDead, na.rm=T))
  off_season <- df %>% filter(open==FALSE) %>%
    summarize(Interaction=sum(Numkept, Discard_Alive, ReleasedDead, na.rm=T))

  on_season$Interaction / (on_season$Interaction + off_season$Interaction)

}

cureE_genR <- 1000

GNrec_current$Rel_E_On_Season * cureE_genR
GNrec_current$Rel_E_Off_Season * cureE_genR


(rec_out[[s]][[5]]@Effort * GNrec_current$Rel_E_On_Season * cureE_genR )
(rec_out[[s]][[6]]@Effort * GNrec_current$Rel_E_Off_Season * cureE_genR)


# open Red Snapper all year

MMP <- function(x, DataList, ...) {
  rec_out <- list()
  # Red Snapper
  rec_out[[1]] <- list()
  # Gag Grouper
  rec_out[[2]] <- list()


  for (s in 1:2) {
    # Commercial Handline - On-Season
    rec_out[[s]][[1]] <- new('Rec')
    rec_out[[s]][[1]]@Effort <- 1

    # Commercial Handline - Off-Season
    rec_out[[s]][[2]] <- new('Rec')
    rec_out[[s]][[2]]@Effort <- 1

    # Recreational Headboat - On-Season
    rec_out[[s]][[3]] <- new('Rec')
    rec_out[[s]][[3]]@Effort <- 1

    # Recreational Headboat - Off-Season
    rec_out[[s]][[4]] <- new('Rec')
    rec_out[[s]][[4]]@Effort <- 1

    # General Recreational - On-Season
    rec_out[[s]][[5]] <- new('Rec')
    predE <- pred_DF %>% filter(Day==new_season)


    e1 <- GNrec_current$Rel_E_On_Season * cureE_genR
    e1a <- GNrec_current$Rel_E_Off_Season * cureE_genR
    e2 <- predE$Pred * cureE_genR
    e3 <- (1-predE$Pred) * cureE_genR


    rec_out[[s]][[5]]@Effort <-  e2 / e1

    # General Recreational - Off-Season
    rec_out[[s]][[6]] <- new('Rec')
    rec_out[[s]][[6]]@Effort <- e3 / e1a

    # Commercial Dive
    rec_out[[s]][[7]] <- new('Rec')
    rec_out[[s]][[7]]@Effort <- 1


  }
  rec_out


}
class(MMP) <- 'MMP'


MMSE <- ProjectMOM(MOM_multiHist, MPs=c('MMP', 'curE'), dropHist = FALSE, parallel = FALSE)

par(mfrow=c(1,2))
plot(MMSE@Effort[1,1,5,1,], type='l', ylim=c(0,5))
lines(MMSE@Effort[1,1,5,2,], col='blue')

plot(MMSE@Effort[1,2,6,1,], type='l', ylim=c(0,5))
lines(MMSE@Effort[1,2,6,2,], col='blue')





MMSE@SB_SBMSY[1,1,1,]
MMSE@SB_SBMSY[1,1,2,]





C <- apply(MMSE@Catch[,2,,1,], c(1,3), sum)
matplot(t(C), type='l')


DataList <- list(list(MOM_multiHist[[1]][[1]]@Data,
                      MOM_multiHist[[1]][[2]]@Data,
                      MOM_multiHist[[1]][[3]]@Data),
                 list(MOM_multiHist[[2]][[1]]@Data,
                      MOM_multiHist[[2]][[2]]@Data,
                      MOM_multiHist[[2]][[3]]@Data))



Effort_DF <- data.frame(CHL=1, RHB=1, GNR=1)
Season_Length







# ---- Properties of Recruitment Deviations ----
plot_rec_devs <- function(MOM_multiHist, st) {
  maxage <- MOM_multiHist[[st]][[1]]@SampPars$Stock$maxage
  rec_devs <- MOM_multiHist[[st]][[1]]@SampPars$Stock$Perr_y
  rec_devs <- rec_devs[,(maxage+1):ncol(rec_devs)]

  Years <- get_Years(MOM_multiHist)

  rec_df <- left_join(data.frame(Sim=1:nsim, RecDev=as.vector(rec_devs),
                                 Year=rep(Years$Year, each=nsim)),
                      Years, by='Year')

  ggplot(rec_df %>% filter(Sim %in% 1:9), aes(x=Year, y=RecDev)) +
    facet_wrap(~Sim) +
    geom_line() +
    geom_line(data=rec_df %>% filter(Sim %in% 1:9, Period=='Projection'), color='blue') +
    expand_limits(y=0) +
    guides(color='none') +
    labs(y='Recruitment Deviations') +
    geom_hline(yintercept = 1, linetype=3) +
    theme_bw()
}

report_dev_properties <- function(rdat) {
  rdat <- bamExtras::standardize_rdat(rdat)
  parms <- rdat[["parms"]]
  t.series <- rdat[["t.series"]] # time data frame
  Perr <- parms[["R.sigma.logdevs"]]
  AC <- acf(t.series$logR.dev, lag.max = 1, plot = FALSE)$acf[2]
  data.frame(Perr, AC)
}

# Red Snapper

plot_rec_devs(MOM_multiHist, 1)
ggsave('img/RS_rec_devs.png')

RS_rec_devs <- report_dev_properties(rdat_RedSnapper)
saveRDS(RS_rec_devs, 'Objects/RS_rec_devs.rda')

# Gag Grouper

plot_rec_devs(MOM_multiHist, 2)
ggsave('img/GG_rec_devs.png')

GG_rec_devs <- report_dev_properties(rdat_GagGrouper)
saveRDS(GG_rec_devs, 'Objects/GG_rec_devs.rda')




# General Rec
fleets <- names(MOM_multiHist[[1]])
fl <- 3
GN_On_RS <- data.frame(Year=1950:2019,
                        Stock='Red Snapper',
                        Fleet=fleets[fl],
                        C=rowSums(MOM_multiHist[[1]][[fl]]@TSdata$Removals[1,,]),
                        F=MOM_multiHist[[1]][[fl]]@SampPars$Fleet$Find[1,],
                        B=rowSums(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,,]),
                        Season='On-Season')

GN_Off_RS <- data.frame(Year=1950:2019,
                         Stock='Red Snapper',
                         Fleet=fleets[fl+3],
                         C=rowSums(MOM_multiHist[[1]][[fl+3]]@TSdata$Removals[1,,]),
                         F=MOM_multiHist[[1]][[fl+3]]@SampPars$Fleet$Find[1,],
                         B=rowSums(MOM_multiHist[[1]][[1]]@TSdata$Biomass[1,,]),
                         Season='Off-Season')


GN_On_RS <- GN_On_RS %>% filter(Year>=2010)
GN_Off_RS <- GN_Off_RS %>% filter(Year>=2010)

GN_On_RS$season_length <- c(336, 0,6,3, 8, 0, 0, 9, 6, 5)

GN_On_RS$relC <- GN_On_RS$C/(GN_On_RS$C+GN_Off_RS$C)


x <- GN_On_RS$season_length
y <- GN_On_RS$relC
df <- data.frame(x=x, y=y) %>% arrange(x)

plot(df$x, df$y)

fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data=df)
summary(fit)

newX <- 0:365
pred <- predict(fit, newdata = data.frame(x=newX))
lines(newX, pred, col='blue')


GN_On_RS %>% filter(Year==2010)
GN_Off_RS %>% filter(Year==2010)






# ---- Species Targeting ----

fl <- 2

fleets <- names(MOM_multiHist[[1]])
names(MOM_multiHist[[2]])[fl]


Com_On_RS <- data.frame(Year=1950:2019,
                      Stock='Red Snapper',
                      Fleet=fleets[fl],
                      C=rowSums(MOM_multiHist[[1]][[fl]]@TSdata$Landings[1,,]),
                      F=MOM_multiHist[[1]][[fl]]@SampPars$Fleet$Find[1,],
                      B=rowSums(MOM_multiHist[[1]][[fl]]@TSdata$VBiomass[1,,]),
                      Season='On-Season')

Com_On_RS$CPUE <- Com_On_RS$C/Com_On_RS$F

Com_On_GG <- data.frame(Year=1950:2019,
                        Stock='Gag Grouper',
                        Fleet=fleets[fl],
                        C=rowSums(MOM_multiHist[[2]][[fl]]@TSdata$Landings[1,,]),
                        F=MOM_multiHist[[2]][[fl]]@SampPars$Fleet$Find[1,],
                        B=rowSums(MOM_multiHist[[2]][[fl]]@TSdata$VBiomass[1,,]),
                        Season='On-Season')

Com_On_GG$CPUE <- Com_On_GG$C/Com_On_GG$F

rs_year <- Com_On_RS %>% filter(C>0) %>% select(Year)
gg_year <- Com_On_GG %>% filter(C>0) %>% select(Year)

years <- intersect(rs_year$Year, gg_year$Year)

Com_On_RS <- Com_On_RS %>% filter(Year %in% years)
Com_On_GG <- Com_On_GG %>% filter(Year%in% years)


rel_biomass <- Com_On_RS$B/Com_On_GG$B
rel_CPUE <- Com_On_RS$CPUE/Com_On_GG$CPUE
rel_F <- Com_On_RS$F/(Com_On_GG$F+Com_On_RS$F)

par(mfrow=c(2,2))

plot(rel_CPUE, rel_F)
plot(log(rel_CPUE), rel_F)
plot(years, rel_F)
plot(years, rel_CPUE)
























Com_Off_RS <- data.frame(Year=1950:2019,
                     Stock='Red Snapper',
                     Fleet='Commercial Handline',
                     F=MOM_multiHist$`Red Snapper`$`Commercial Handline: Off-Season`@SampPars$Fleet$Find[1,],
                     B=rowSums(MOM_multiHist$`Red Snapper`[[1]]@TSdata$Biomass[1,,]),
                     Season='Off-Season')

head(Com_On_RS)
head(Com_Off_RS)


                          Commercial=rowSums(MOM_multiHist$`Red Snapper`$`Commercial Handline: On-Season`@TSdata$Landings[1,,])/1000,
                          Rec_HB=rowSums(MOM_multiHist$`Red Snapper`$`Recreational Headboat: On-Season`@TSdata$Landings[1,,])/1000,
                          Rec_GN=rowSums(MOM_multiHist$`Red Snapper`$`General Recreational: On-Season`@TSdata$Landings[1,,])/1000)


names(MOM_multiHist$`Red Snapper`)

df_landings <- data.frame(Year=1950:2019,
                 Commercial=rowSums(MOM_multiHist$`Red Snapper`$`Commercial Handline: On-Season`@TSdata$Landings[1,,])/1000,
                 Rec_HB=rowSums(MOM_multiHist$`Red Snapper`$`Recreational Headboat: On-Season`@TSdata$Landings[1,,])/1000,
                 Rec_GN=rowSums(MOM_multiHist$`Red Snapper`$`General Recreational: On-Season`@TSdata$Landings[1,,])/1000)

df_landings$rat <- df_landings$Rec_HB/df_landings$Rec_GN
df_landings %>% filter(Year==1990)


plot(1950:2019,df_landings$rat, type='l')

kg2lb(df[70,2])/1000
kg2lb(df[70,3])/1000
kg2lb(df[70,4])/1000


