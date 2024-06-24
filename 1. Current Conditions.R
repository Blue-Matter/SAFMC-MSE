
# Plots of B vs Btarget and F vs Ftarget from BAM model #
library(ggplot2)

# ---- Base Case ----
rdat_RS <- bamExtras::rdat_RedSnapper |> bamExtras::standardize_rdat()
rdat_GG <- bamExtras::rdat_GagGrouper |> bamExtras::standardize_rdat()
rdat_BS <- bamExtras::rdat_BlackSeaBass |> bamExtras::standardize_rdat()

dataList <- list('Base Case' = list('Red snapper'=rdat_RS,
                                    'Gag grouper'=rdat_GG,
                                    'Black seabass'= rdat_BS)
)


plot_current_status <- function(dataList) {
  df_om_list <- list()

  for (om in seq_along(dataList)) {
    df_list <- list()
    for (i in seq_along(dataList[[om]])) {
      OM <- names(dataList)[om]
      nm <- names(dataList[[om]])[[i]]
      rdat <- dataList[[om]][[i]]

      if (nm == "Red snapper") {
        MSST <- rdat$t.series$SSB.msstF30 # red snapper
        MFMT <- rdat$t.series$F.F30.ratio
      } else {
        MSST <- rdat$t.series$SSB.msst # gag grouper & black sea bass
        MFMT <- rdat$t.series$F.Fmsy # gag
      }

      temp <- data.frame(Year=rdat$t.series$year,
                         Stock=nm, MSST=MSST, MFMT=MFMT, OM=OM)
      temp <- temp[!is.na(temp$MSST),]
      df_list[[i]] <- temp
    }
    df_om_list[[om]] <- do.call('rbind', df_list)
  }

  df <- do.call('rbind', df_om_list)
  df <- df |> tidyr::pivot_longer(cols=3:4)
  df$name <- dplyr::case_match(df$name,
                               "MSST" ~ 'SSB/MSST',
                               "MFMT" ~ 'F/MFMT')
  df$Stock <- factor(df$Stock, level=names(dataList[[om]]), ordered = TRUE)
  df$name <- factor(df$name, level=unique(df$name), ordered = TRUE)
  df$OM <- factor(df$OM, level=unique(df$OM), ordered = TRUE)

  ggplot(df, aes(x=Year, y=value, colour = OM, linetype=OM)) +
    facet_grid(name~Stock, scales='free_y') +
    geom_line() +
    expand_limits(y=0) +
    geom_hline(yintercept = 1, linetype=2) +
    theme_bw() +
    labs(y='Value')

}

p <- plot_current_status(dataList)

ggsave('img/Stock_Status.png', p)

rdat_RS$t.series$SSB.msstF30
rdat_GG$t.series$SSB.msstF30


library(openMSE)

OM_RS <- readRDS('inst/OM_RS.rda')
Hist_RS <- Simulate(OM_RS)

OM_GG <- readRDS('inst/OM_GG.rda')
Hist_GG <- Simulate(OM_GG)


OM_BS <- readRDS('inst/OM_BS.rda')
Hist_BS <- Simulate(OM_BS)

# Stock Status by stock and fleet

# B/Btarget

# Red Snapper


Calculate_Ref_Points <- function(multiHist, stock='RS') {

  if (stock=='RS') {
    # Red Snapper
    y <- ncol(multiHist[[1]][[1]]@TSdata$Find)
    SP_SPR_df <- Calc_RS_Ref_Points(1, y, multiHist)
    SPR_targ <- 0.3
    ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
    rsdf <- SP_SPR_df[ind,] %>% select(F, SPR, SBtarg=SB)
    rsdf$MSST <- rsdf$SB*0.75
    rsdf$Stock <- 'Red Snapper'
    return(rsdf)
  }

  if (stock=='GG') {
    # Gag Grouper
    y <- ncol(multiHist[[1]][[1]]@TSdata$Find)
    GG_SP_SPR_df <- Calc_GG_Ref_Points(1, y, multiHist)

    ind <- which.max(GG_SP_SPR_df$Removals)
    GG_Ref_df <- GG_SP_SPR_df[ind,]
    ggdf <- GG_Ref_df %>% select(F, SPR, SBtarg=SB)
    ggdf$MSST <- ggdf$SB*0.75
    ggdf$Stock <- "Gag Grouper"
    return(ggdf)
  }

  if (stock=='BS') {
    # BS
    y <- ncol(multiHist[[1]][[1]]@TSdata$Find)
    GG_SP_SPR_df <- Calc_GG_Ref_Points(1, y, multiHist)

    ind <- which.max(GG_SP_SPR_df$Removals)
    GG_Ref_df <- GG_SP_SPR_df[ind,]
    ggdf <- GG_Ref_df %>% select(F, SPR, SBtarg=SB)
    ggdf$MSST <- ggdf$SB*0.75
    ggdf$Stock <- "Gag Grouper"
    return(ggdf)
  }

}




ref_points <- Calculate_Ref_Points(Hist_RS, 'RS')
B <- get_SSB(Hist_RS) |> dplyr::filter(Sim==1)
B$Btarg <- ref_points$MSST
B$Btarg <- ref_points$SBtarg
B$B_Btarg <- B$Value/B$Btarg

ggplot(B, aes(x=Year, y=B_Btarg)) +
  geom_line() +
  expand_limits(y=0) +
  geom_hline(yintercept = 1, lty=2) +
  theme_bw()

plot(B$Year, B$B_Btarg, ylim=c(0,3), type='l')
abline(h=1, lt=2)

# Gag Grouper

ref_points <- Calculate_Ref_Points(Hist_GG, 'GG')

# Black Seabass


Hist <- Hist_RS

plot(rdat_RS$eq.series$F.eq, rdat_RS$eq.series$SSB.eq, type='l')

SP_SPR_df <- Calc_RS_Ref_Points(1, 70, multiHist)
SPR_targ <- 0.3
ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
rsdf <- SP_SPR_df[ind,] %>% select(F, SPR, SBtarg=SB)
rsdf$MSST <- rsdf$SB*0.75
rsdf$Stock <- 'Red Snapper'

lines(SP_SPR_df$F, SP_SPR_df$SB, col='blue')


# Red Snapper




# Landings and Discards by stock and fleet


# Management Options

