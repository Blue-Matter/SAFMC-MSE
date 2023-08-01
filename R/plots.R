

#' Plot Landings and Discards
#'
#' @param multiHist
#'
#' @return A ggplot object
#' @export
#'
plot_Catch_Discards <- function(multiHist) {
  fleets <- names(multiHist[[1]])
  hist.years <- MSEgraph::get_Years(multiHist) %>% filter(Period=='Historical')

  out_list <- list()
  for (i in 1:length(fleets)) {
    fl <- fleets[i]
    fl_name <-  strsplit(fl, '\\:')[[1]][1]
    Landings <- rowSums(multiHist[[1]][[fl]]@TSdata$Landings[1,,])
    Discards <- rowSums(multiHist[[1]][[fl]]@TSdata$Discards[1,,])
    season <- 'On-Season'
    if (grepl('Off', fl)) season <- 'Off-Season'
    out_list[[i]] <- data.frame(Year=hist.years$Year, Fleet=fl_name, Landings=Landings, Discards=Discards, Season=season)
  }
  df <- do.call('rbind', out_list)
  df <- df %>% tidyr::pivot_longer(., cols=c(Landings, Discards))
  df$Season <- factor(df$Season, levels=unique(df$Season), ordered = TRUE)
  df$name <- factor(df$name, levels=unique(df$name), ordered = TRUE)
  df$Fleet <- factor(df$Fleet, levels=unique(df$Fleet), ordered = TRUE)
  df$value <- df$value / 1000
  ggplot(df, aes(x=Year, y=value, color=name)) +
    facet_grid(Season~Fleet, scales='free') +
    geom_line() +
    scale_color_manual(values=c('blue', 'orange')) +
    theme_bw() +
    labs(x='Year', y='Landings/Discards (t)',
         color='')
}


#' Plot Spawning Stock and Reference Pints
#'
#' @param MMSE
#'
#' @return
#' @export
plot_SB <- function(MMSE) {
  Ref_Points <- Calculate_Ref_Points(MMSE@multiHist)

  SSB <- get_SSB(MMSE) %>% filter(Period=='Projection')

  SSB_df <- SSB %>% group_by(Year, Stock, MP) %>%
    dplyr::summarise(Mean=mean(Value),
                     Lower=quantile(Value,0.25),
                     Upper=quantile(Value, 0.75),
                     .groups = 'drop')
  SSB_df$MP <- factor(SSB_df$MP, levels=MMSE@MPs[[1]], ordered = TRUE)


  SSB_df <- left_join(SSB_df, Ref_Points, by = join_by(Stock))
  SSB_df$Stock <- factor(SSB_df$Stock, levels=names(MMSE@Stocks), ordered = TRUE)

  ggplot(SSB_df, aes(x=Year)) +
    facet_grid(Stock~MP, scales='free_y') +
    geom_ribbon(aes(ymin=Lower, ymax=Upper), fill='lightgray') +
    geom_line(aes(x=Year, y=Mean)) +
    expand_limits(y=0) +
    theme_bw() +
    labs(y='Spawning Stock') +
    geom_hline(aes(yintercept=MSST), linetype=2) +
    geom_hline(aes(yintercept=SBtarg), linetype=3)

}

#' Plot Landings and Discards from Projection Period
#'
#' @param MMSE
#' @param colors
#'
#' @return
#' @export
plot_Catch <- function(MMSE, colors=c('darkblue', 'darkred')) {

  Landings <- get_Landings(MMSE)
  Removals <- get_Removals(MMSE)

  Discards <- Landings
  Discards$Value <- Removals$Value - Landings$Value
  Discards <- Discards %>% filter(Period=='Projection')
  Landings <- Landings %>% filter(Period=='Projection')

  Landings_df <- Landings %>%
    group_by(Year, Stock, MP, Sim) %>%
    dplyr::summarise(Value=sum(Value)/1000, .groups='drop') %>%
    group_by(Year, Stock, MP) %>%
    dplyr::summarise(Mean=mean(Value),
                     Lower=quantile(Value,0.25),
                     Upper=quantile(Value, 0.75),
                     .groups = 'drop')
  Landings_df$Variable <- 'Landings'

  Discards_df <- Discards %>%
    group_by(Year, Stock, MP, Sim) %>%
    dplyr::summarise(Value=sum(Value)/1000, .groups='drop') %>%
    group_by(Year, Stock, MP) %>%
    dplyr::summarise(Mean=mean(Value),
                     Lower=quantile(Value,0.25),
                     Upper=quantile(Value, 0.75),
                     .groups = 'drop')

  Discards_df$Variable <- 'Discards'

  df <- bind_rows(Landings_df, Discards_df)

  df$MP <- factor(df$MP, levels=MMSE@MPs[[1]], ordered = TRUE)
  df$Stock <- factor(df$Stock, levels=names(MMSE@Stocks), ordered = TRUE)
  df$Variable <- factor(df$Variable, levels=c('Landings', 'Discards'), ordered = TRUE)

  ggplot(df, aes(x=Year)) +
    facet_grid(Stock~MP, scales='free_y') +
    geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=Variable), alpha=0.3) +
    geom_line(aes(x=Year, y=Mean, color=Variable)) +
    expand_limits(y=0) +
    theme_bw() +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors) +
    labs(y='Landings (1000 t)')

}

#' Plot Total Fishing Mortality
#'
#' @param MMSE
#'
#' @return
#' @export
plot_F <- function(MMSE) {

  F_DF <- get_F(MMSE) %>%  filter(Period=='Projection') %>%
    group_by(Year, Stock, MP, Sim) %>%
    summarize(Value=sum(Value), .groups='drop')

  F_DF <- F_DF %>%
    group_by(Year, Stock, MP) %>%
    dplyr::summarise(Mean=mean(Value),
                     Lower=quantile(Value,0.25),
                     Upper=quantile(Value, 0.75),
                     .groups = 'drop')

  F_DF$MP <- factor(F_DF$MP, levels=MMSE@MPs[[1]], ordered = TRUE)
  F_DF$Stock <- factor(F_DF$Stock, levels=names(MMSE@Stocks), ordered = TRUE)

  ggplot(F_DF, aes(x=Year)) +
    facet_grid(Stock~MP, scales='free_y') +
    geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3) +
    geom_line(aes(x=Year, y=Mean)) +
    expand_limits(y=0) +
    theme_bw() +
    scale_fill_manual(values=colors) +
    scale_color_manual(values=colors) +
    labs(y='Landings (1000 t)')

}


