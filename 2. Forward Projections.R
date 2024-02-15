library(SAMSE)
MPs <- c('StatusQuo', 'RecEff20', 'RecEff40', 'Ftarget', 'MLL20_25', 'MLL25_25')

incRecEMPs <- paste0(MPs, '_IncRecEff')

# ---- Loop through OMs and Run Projections ----

OM_hists <- list.files('Hist_Objects', pattern='.hist')

for (i in seq_along(OM_hists)) {
  multiHist <- readRDS(file.path('Hist_Objects', OM_hists[i]))

  if (i == 6) {
    MSE <- ProjectMOM(multiHist, MPs=incRecEMPs,
                      dropHist = FALSE)
  } else {
    MSE <- ProjectMOM(multiHist, MPs=MPs,
                      dropHist = FALSE)
  }


  rm(multiHist)

  fl <- paste0(tools::file_path_sans_ext(OM_hists[i]), '.mmse')
  saveRDS(MSE, file.path('MSE_Objects', fl))
  rm(MSE)

}


# ---- Calculate Summary Statistics & Performance Metrics ----
library(openMSE)
OMs <- paste0("OM_", unlist(strsplit(OM_hists, '.hist')))

MSE_info <- vector('list', length(OMs))
names(MSE_info) <- OMs
for (i in seq_along(OMs)) {

  fl <- paste0(unlist(strsplit(OM_hists[i], '.hist')), '.mmse')
  MSE <- readRDS(file.path('MSE_Objects', fl))

  structure_fleets <- function(df) {
    fleet_names <- as.character(unique(df$Fleet))
    fleet_names2 <- lapply(strsplit(fleet_names, ':'), '[', 1) %>% unlist()

    for (i in seq_along(fleet_names)) {
      df <- df %>% mutate(Fleet=dplyr::case_match(Fleet, fleet_names[i]~fleet_names2[i], .default=Fleet))
    }

    if (!is.null(df$MP)) {
      df <- df %>% group_by(Year, Sim, Stock, Fleet, MP, Variable) %>% summarise(Value=sum(Value),
                                                                             .groups='drop')
    } else {
      df <- df %>% group_by(Year, Sim, Stock, Fleet, Variable) %>% summarise(Value=sum(Value),
                                                                             .groups='drop')
    }



    df$Fleet <- factor(df$Fleet, levels=unique(fleet_names2), ordered = TRUE)
    df
  }

  add_factor <- function(df) {
    df$Stock <- factor(df$Stock, levels=unique(df$Stock), ordered = TRUE)
    if (!is.null(df[['MP']])) {
      ind <- grepl('_IncRecEff', unique(df$MP))

      if (any(ind)) {
        df$MP <- strsplit(df$MP, '_IncRecEff') %>% unlist()
      }

      df$MP <- factor(df$MP, levels=unique(df$MP), ordered = TRUE)
    }
    if (!is.null(df$Fleet)) {
      df <- structure_fleets(df)
    }


    df
  }

  # Historical
  Ref_Points <- Calculate_Ref_Points(MSE@multiHist) %>% add_factor()

  MSE_info[[i]]$Ref_Points <- Ref_Points

  SB <- get_SSB(MSE@multiHist) %>% filter(Sim==1) %>% add_factor()

  Landings <- get_Landings(MSE@multiHist) %>% filter(Sim==1) %>% add_factor()


  Removals <- get_Removals(MSE@multiHist) %>% filter(Sim==1) %>% add_factor()
  Fishing_Mortality <- get_F(MSE@multiHist) %>% filter(Sim==1) %>% add_factor()

  Discards <- Removals
  Discards$Variable <- 'Discards'
  Discards$Value <- Removals$Value - Landings$Value
  Discards$Value[ Discards$Value<0] <- 0

  Fishing_Mortality <- get_F(MSE@multiHist) %>% filter(Sim==1) %>% add_factor()

  MSE_info[[i]]$Historical <- dplyr::bind_rows(Landings, Discards, SB, Fishing_Mortality)

  # Projection
  SB <- get_SSB(MSE)  %>% add_factor()

  Landings <- get_Landings(MSE)  %>% add_factor()
  Removals <- get_Removals(MSE)  %>% add_factor()
  Discards <- Removals
  Discards$Variable <- 'Discards'
  Discards$Value <- Removals$Value - Landings$Value
  Discards$Value[ Discards$Value<0] <- 0

  Fishing_Mortality <- get_F(MSE) %>% add_factor()
  MSE_info[[i]]$Projection <- dplyr::bind_rows(Landings, Discards, SB, Fishing_Mortality)

}

saveRDS(MSE_info, 'inst/shiny_app/Data/MSE_info.rda')


tt = MSE_info$OM_01$Projection %>% filter(Stock=='Gag Grouper', MP=='Ftarget', Variable=='Landings',
                                      Year==2025)

tt  = tt %>% group_by(Year, Sim) %>% summarise(Value=sum(Value))

openMSE::kg2_1000lb(median(tt$Value))

tt =MSE_info$OM_01$Projection %>% filter(Stock=='Gag Grouper', MP=='Ftarget', Variable=='Landings',
                                     Fleet=='Recreational Headboat') %>%
  group_by(Year) %>%
  summarise(median=openMSE::kg2_1000lb(median(Value)))

plot(tt$Year, tt$median, type='l')


Ref_Points <- MSE_info[[1]]$Ref_Points
SSBhist <- MSE_info[[1]]$Historical$SSB
Catch <- MSE_info[[1]]$Historical$Catch


plot_SB_hist <- function(SSBhist, Ref_Points, inc_ref_point=TRUE, rel_to=NA, ymax=NULL) {

  ref_point <- Ref_Points %>%
    tidyr::pivot_longer(., cols=1:4, names_to = 'Reference Point') %>%
    filter(`Reference Point` %in% c("SBtarg", 'MSST'))

  SSBhist <- left_join(SSBhist, Ref_Points, by = join_by(Stock)) %>%
    tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                        names_to = 'Reference Point') %>%
    filter(`Reference Point` %in% c("SBtarg", 'MSST'))


  if (!is.na(rel_to)) {
    SSBhist <- SSBhist %>% group_by(Stock) %>% mutate(Value=Value/value[`Reference Point`==rel_to],
                                                      value=value/value[`Reference Point`==rel_to])
  }


  p <- ggplot(SSBhist) +
    facet_wrap(~Stock, scale='free', ncol=1) +
    geom_line( aes(x=Year, y=Value)) +
    expand_limits(y=0) +
    theme_bw() +
    labs(y=unique(SSBhist$Variable)) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          strip.text = element_text(size=16,face="bold"),
          legend.position="bottom")

  if (!is.null(ymax)) {
    dummy <- data.frame(x=rep(range(SSBhist$Year),2),
                        Stock =rep(unique(SSBhist$Stock), each=2),
                        y=c(0, ymax[[1]], 0, ymax[[2]]))
    p <- p + geom_blank(data=dummy, aes(x=x, y=y))
  }


  if (inc_ref_point) {
    p <- p + geom_hline(aes(yintercept=value, linetype=`Reference Point`))
  }

  p
}





p + geom_hline(aes(yintercept=MSST), linetype=2) +
  geom_hline(aes(yintercept=SBtarg), linetype=3)

# Make list of MSE results to load into Shiny

# Historical OM
## SSB
## Catch
## Fishing Mortality

# Projections
## SSB
## Catch
## Fishing Mortality

# Performance Metrics



F_DF <- get_F(MSE)


plot_Fmort(MSE)
plot_Catch(MSE)
plot_SB(MSE)

Landings_10(MSE)
Landings_20(MSE)

avail('PM', 'SAMSE')






