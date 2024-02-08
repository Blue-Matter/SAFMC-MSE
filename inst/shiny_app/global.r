# OMdat <- read.csv('OM_details/OM_descriptions.csv')

library(shiny)
library(SAMSE)
library(shinyWidgets)

OMdat <- read.csv('../../OM_details/OM_descriptions.csv')
OMdat <- OMdat %>% dplyr::rename(., 'Key Uncertainty'=Key.Uncertainty)

MSE_info <- readRDS('Data/MSE_info.rda')

OM_names <- names(MSE_info)
hist_OMs <- OM_names[1:4]
hist_OMs <- hist_OMs[!is.na(hist_OMs)]
ref_points <- names(MSE_info$OM_01$Ref_Points)

SB_ref_points <- ref_points[ref_points%in%c('SBtarg', 'MSST')]
### Plotting Functions ####

plot_SB_hist <- function(SSBhist, Ref_Points, inc_ref_point=TRUE, rel_to=NA) {

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

  p <- ggplot(SSBhist, aes(x=Year, y=Value)) +
    facet_wrap(~Stock, scale='free', ncol=1) +
    geom_line() +
    expand_limits(y=0) +
    theme_bw() +
    labs(y=unique(SSBhist$Variable)) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          strip.text = element_text(size=16,face="bold"),
          legend.position="bottom")


  if (inc_ref_point) {
    p <- p + geom_hline(aes(yintercept=value, linetype=`Reference Point`))
  }

  p
}
