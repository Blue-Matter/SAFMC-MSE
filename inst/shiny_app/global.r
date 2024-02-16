# OMdat <- read.csv('OM_details/OM_descriptions.csv')

library(shiny)
library(shinyWidgets)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(DT)

OMdat <- read.csv('Data/OM_descriptions.csv')
OMdat <- OMdat %>% dplyr::rename(., 'Key Uncertainty'=Key.Uncertainty)

MSE_info <- readRDS('Data/MSE_info.rda')

OM_numbers <- names(MSE_info)
OM_names <- OMdat$Name[1:length(OM_numbers)]

MPs <- unique(MSE_info[[1]]$Projection$MP)
MPs <- MPs[!is.na(MPs)]

ref_points <- names(MSE_info$OM_01$Ref_Points)

SB_ref_points <- ref_points[ref_points%in%c('SBtarg', 'MSST')]

stocks <- unique(MSE_info[[1]]$Historical$Stock)

# plot_choices_hist <- c('Spawning Biomass', 'Catch', 'Fishing Mortality')
plot_choices_hist <- c('Spawning Biomass', 'Fishing Mortality')
plot_choices_proj <- c('Spawning Biomass',  'Catch','Fishing Mortality')

# plot code


hist_plot <- function(DF, byfleet=FALSE, ymax=NULL) {

  var <- unique(DF$Variable)
  DF$Variable <- as.character(DF$Variable)

  if ('Discards' %in% var)
    var <- 'Discards'

  ylab <- switch(var,
                 `Apical Fishing Mortality`='Fishing Mortality',
                 `Discards` = 'Landings/Discards',
                 "Spawning Biomass"="Spawning Production")



  p <- ggplot(DF, aes(x=Year, y=Value, color=Variable))

  if (!byfleet & var!='Discards') {
    p <- p + geom_hline(aes(yintercept=value, linetype=`Reference Point`))
  }

  if (!is.null(DF[["Upper"]])) {
    p <- p + geom_ribbon(aes(ymin=Lower, ymax=Upper), fill='lightgray', alpha=0.4)
  }

  p <- p +
    geom_line(linewidth=1.5) +
    theme_bw() +
    expand_limits(y=0)

  if (!is.null(ymax)) {
    p <- p +  expand_limits(y=c(0, ymax))
  }

  p <- p + labs(y=ylab) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          strip.text = element_text(size=16,face="bold"),
          legend.position="bottom")

  if (ylab !='Landings/Discards') {
    p <- p + guides(color='none')
  }
  if(byfleet)
    p <- p + facet_wrap(~Fleet, scales="free_y")

  p +geom_vline(xintercept = 2019, linetype=2, color='darkgray')
}




