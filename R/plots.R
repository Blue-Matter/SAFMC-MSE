

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
