library(openMSE)
library(dplyr)
library(ggplot2)

# ---- Load Base Case MOM ----
# created in 1_Generate_MOM.R
RS_GG_MOM <- readRDS('OMs/BaseCaseMOM.rda')


# ---- Simulate Historical Fishery ----

RS_GG_hist <- SimulateMOM(RS_GG_MOM)


# ---- Plot Historical Simulated Fishery ----

multiHist <- RS_GG_hist

names(multiHist)
names(multiHist[[1]])


RS_GG_MOM@Fleets[[1]] %>% names()

get_info <- function(multiHist) {
  out <- list()
  out$nsim <- tt[1]
  out$stocks <- names(multiHist)
  out$fleets <- names(multiHist[[1]])
  out$n.stocks <- length(multiHist)
  out$n.fleets <- length(multiHist[[1]])
  out$hist.yrs <- multiHist[[1]][[1]]@Data@Year
  tt <- dim(multiHist[[1]][[1]]@AtAge$Length)

  out$nage <- tt[2]
  out$maxage <- nage - 1
  out$nyears <- length(hist.yrs)
  out$proyears <- tt[3]-nyears
  out
}

get_ts <- function(multiHist) {
  info <- get_info(multiHist)
  outlist <- list()
  for (p in 1:info$n.stocks) {
    outlist[[p]] <- list()
    for (fl in 1:info$n.fleets) {
      nms <- c('Biomass', 'Removals', 'Landings', 'Discards')
      dflist <- list()
      for (nm_i in seq_along(nms)) {
        vals <- multiHist[[p]][[fl]]@TSdata[[nms[nm_i]]][1,,]
        if (!is.null(vals)) {
          vals <- rowSums(vals)
        } else {
          vals <- NA
        }
        dflist[[nm_i]] <- data.frame(Stock=info$stocks[p],
                                     Fleet=info$fleets[fl],
                                     Year=info$hist.yrs,
                                     Value=vals,
                                     Name=nms[nm_i])

      }

      outlist[[p]][[fl]] <- do.call('rbind', dflist)

    }
    outlist[[p]] <- do.call('rbind', outlist[[p]])
  }
  df <-  do.call('rbind', outlist)
  df
}


plot_biomass <- function(multiHist, units=c('1000 lb', 'mt')) {
  units <- match.arg(units)
  info <- get_info(multiHist)
  df <- get_ts(multiHist)
  bio <- df %>% filter(Name=='Biomass', Fleet==info$fleets[1])
  if (units=='1000 lb') bio$Value <- bio$Value/1000
  if (units=='mt') bio$Value <- lb2mt(bio$Value)
  ggplot(bio, aes(x=Year, y=Value, color=Stock, linetype=Stock)) +
    geom_line() +
    expand_limits(y=0) +
    theme_bw() +
    labs(x='Year', y=paste0('Biomass (', units, ')'))

}

# TODO:
# add reference points
# plot catches and discards
#

plot_biomass(RS_GG_hist, 'mt')


lb2mt(RS_GG_hist$Gag$cHL@Ref$ReferencePoints$BMSY[1])

RS_GG_hist$Gag$cHL@Ref$ReferencePoints$FMSY
RS_GG_hist$`Red Snapper`$cHL@Ref$ReferencePoints$FMSY

