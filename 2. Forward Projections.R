library(SAMSE)
MPs <- c('StatusQuo','StatusQuo_MLL', 'SQRecEffort20', 'Ftarget')

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
OM_hists <- OM_hists[!OM_hists %in% c("07.hist", "08.hist")]
OMs <- paste0("OM_", unlist(strsplit(OM_hists, '.hist')))

MSE_info <- vector('list', length(OMs))
names(MSE_info) <- OMs
for (i in seq_along(OMs)) {

  fl <- paste0(unlist(strsplit(OM_hists[i], '.hist')), '.mmse')
  MSE <- readRDS(file.path('MSE_Objects', fl))

  add_stock_factor <- function(df) {
    df$Stock <- factor(df$Stock, levels=unique(df$Stock), ordered = TRUE)
    df
  }

  # Historical
  Ref_Points <- Calculate_Ref_Points(MSE@multiHist) %>% add_stock_factor()

  MSE_info[[i]]$Ref_Points <- Ref_Points

  SB <- get_SSB(MSE@multiHist) %>% filter(Sim==1) %>% add_stock_factor()


  Landings <- get_Landings(MSE@multiHist) %>% filter(Sim==1) %>% add_stock_factor()
  Removals <- get_Removals(MSE@multiHist) %>% filter(Sim==1) %>% add_stock_factor()
  Fishing_Mortality <- get_F(MSE@multiHist) %>% filter(Sim==1) %>% add_stock_factor()

  Discards <- Removals
  Discards$Variable <- 'Discards'
  Discards$Value <- Removals$Value - Landings$Value
  Discards$Value[ Discards$Value<0] <- 0

  Fishing_Mortality <- get_F(MSE@multiHist) %>% filter(Sim==1) %>%
    group_by(Stock, Year) %>%
    dplyr::summarise(Mean=mean(Value),
                     Lower=quantile(Value,0.25),
                     Upper=quantile(Value, 0.75),
                     .groups = 'drop')

  MSE_info[[i]]$Historical$SSB <- SB
  MSE_info[[i]]$Historical$Landings <- Landings
  MSE_info[[i]]$Historical$Removals <- Removals
  MSE_info[[i]]$Historical$Discards <- Discards
  MSE_info[[i]]$Historical$Fishing_Mortality <- Fishing_Mortality

  # Projection


  # Performance Metrics




}


saveRDS(MSE_info, 'inst/shiny_app/Data/MSE_info.rda')

Ref_Points <- MSE_info[[1]]$Ref_Points
SSBhist <- MSE_info[[1]]$Historical$SSB

plot_SB_hist(MSE_info[[1]]$Historical$SSB, MSE_info[[1]]$Ref_Points, rel_to='MSST')

plot_SB_hist <- function(SSBhist, Ref_Points, inc_ref_point=TRUE, rel_to=NULL) {

  ref_point <- Ref_Points %>%
    tidyr::pivot_longer(., cols=1:4, names_to = 'Reference Point') %>%
    filter(`Reference Point` %in% c("SBtarg", 'MSST'))

  SSBhist <- left_join(SSBhist, Ref_Points, by = join_by(Stock)) %>%
    tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                        names_to = 'Reference Point') %>%
    filter(`Reference Point` %in% c("SBtarg", 'MSST'))


  if (!is.null(rel_to)) {
    SSBhist <- SSBhist %>% group_by(Stock) %>% mutate(Value=Value/value[`Reference Point`==rel_to],
                                                      value=value/value[`Reference Point`==rel_to])
  }

  p <- ggplot(SSBhist, aes(x=Year, y=Value)) +
    facet_wrap(~Stock, scale='free') +
    geom_line() +
    expand_limits(y=0) +
    theme_bw() +
    labs(y=unique(SSBhist$Variable))


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






