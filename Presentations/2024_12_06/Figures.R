
library(SAMSE)
library(patchwork)
library(openMSE)
library(dplyr)
library(ggplot2)
library(ggrepel)


Stock_Names <- c('Red Snapper', 'Gag Grouper', 'Black Sea Bass')

Ref_DF <- readRDS('Misc_Objects/ref_df.rda')
Ref_DF$stock <- tolower(Ref_DF$Stock)
mse_files <- list.files('MSE_Objects')
om <- 'BaseCase'

DF_SQ <- make_DFs(om, mp_code='SQ')
DF_OS <- make_DFs(om, mp_code='SQ_OS')
DF_FR_OS <- make_DFs(om, mp_code='SQ_FR_OS')

SSB <- bind_rows(DF_SQ$SSB, DF_OS$SSB, DF_FR_OS$SSB)
Landings <- bind_rows(DF_SQ$Landings, DF_OS$Landings, DF_FR_OS$Landings)

# Time-Series Plots ----

stocks <- c('RS', 'GG', 'BS')

mp <- 'SQ_0'

SSB$Stock <- forcats::fct_recode(SSB$Stock,
              'Red Snapper'= 'Red snapper',
              'Gag Grouper'='Gag grouper',
              'Black Sea Bass'='Black sea bass')

p1 <- plotTS_SSB(SSB, stock=stocks, mp=mp, inc.perc = TRUE,
                 size.strip.text=14,
                 size.axis.title=12,
                 size.axis.text=10) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=3) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

p2 <- plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = 'darkgreen',
                mp.col=mp_colors,
                y_scales='free_y', byFleet = FALSE,
                inc.ref=FALSE,
                size.strip.text=14,
                size.axis.title=12,
                size.axis.text=10)  +
  guides(linetype='none') +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

cowplot::plot_grid(p1, p2, nrow=2, align='v')

ggsave('img/December2024/TS_SQ.png', width=10, height=6)



mp <- 'SQ_0.65'
ref_mp <- 'SQ_0'

mp_names <- c("SQ 0.35 Gen. Rec. Effort", "Status Quo")


p1 <- plotTS_SSB(SSB, stock=stocks, mp=mp, inc.perc = TRUE,
                 ref_mp=ref_mp,
                 size.strip.text=14,
                 size.axis.title=12,
                 size.axis.text=10) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=3) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

p2 <- plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
                      mp=mp,
                      mp_names = 'darkgreen',
                      mp.col=mp_colors,
                      y_scales='free_y', byFleet = FALSE,
                      inc.ref=FALSE,
                      size.strip.text=14,
                      size.axis.title=12,
                      size.axis.text=10)  +
  guides(linetype='none') +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

cowplot::plot_grid(p1, p2, nrow=2, align='v')

ggsave('img/December2024/TS_SQ_0.35.png', width=10, height=6)






mp <- 'SQ_FR_OS_0'
ref_mp <- 'SQ_0'

mp_names <- ''


p1 <- plotTS_SSB(SSB, stock=stocks, mp=mp, inc.perc = TRUE,
                 ref_mp=ref_mp,
                 size.strip.text=14,
                 size.axis.title=12,
                 size.axis.text=10) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=3) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

p2 <- plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
                      mp=mp,
                      mp_names = 'darkgreen',
                      mp.col=mp_colors,
                      y_scales='free_y', byFleet = FALSE,
                      inc.ref=FALSE,
                      size.strip.text=14,
                      size.axis.title=12,
                      size.axis.text=10)  +
  guides(linetype='none') +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_y_continuous( breaks=scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

cowplot::plot_grid(p1, p2, nrow=2, align='v')

ggsave('img/December2024/TS_OS.png', width=10, height=6)



# Trade-Off Plots ----

RefLandingValues <- DF_SQ$Landings |> ungroup() |> distinct(Stock, Fleet, Ref_Landing, Ref_Discard)
results_dir <- 'Results_Objects'
SSB_Results <- list.files(file.path(results_dir, 'SSB'))
Landings_Results <- list.files(file.path(results_dir, 'F_Landing_Discards'))

L <- strsplit(SSB_Results, paste0(om, '_'))
ind <- which(unlist(lapply(L, length)) == 2)

OM_fls <- lapply(strsplit(SSB_Results[ind], paste0(om, '_')), '[[', 2) |> unlist()
stocks <- lapply(strsplit(OM_fls, '_'), '[[', 1) |> unlist() |> unique()
# stocks <- stocks[stocks%in%stock]

st_list <- list()
for (st in seq_along(stocks)) {
  stock_code <- stocks[st]
  stock <- switch(stock_code,
                  'RS'="Red Snapper",
                  "GG"="Gag Grouper",
                  'BS'="Black Sea Bass"
  )

  SSBfls <- SSB_Results[grepl(paste0(paste(om, stock_code, sep='_'), "_"), SSB_Results)]
  Catchfls <- Landings_Results[grepl(paste0(paste(om, stock_code, sep='_'), "_"), Landings_Results)]

  SSB_list <- list()
  Catch_list <- list()
  for (i in seq_along(SSBfls)) {
    SSB_list[[i]] <- readRDS(file.path(results_dir, 'SSB', SSBfls[i]))
    Catch_list[[i]] <- readRDS(file.path(results_dir, 'F_Landing_Discards', Catchfls[i]))
  }

  # Summarize
  SSBDF <- do.call('rbind', SSB_list)
  SSBDF$RebuildYear <- get_rebuild_year(stock_code)
  CatchDF <- do.call('rbind', Catch_list) |>
    dplyr::group_by(Sim, MP, MP_Name, Rec_Reduction, Year) |>
    summarise(Landings=sum(Landings),
              Discards=sum(Discards))

  SSBDF <- SSBDF |> dplyr::group_by(MP, MP_Name, Rec_Reduction) |>
    filter(Year==RebuildYear) |>
    summarize(RelSSB=median(SSB/Rebuild))
              # Lower=quantile(SSB/Rebuild, 0.25),
              # Upper=quantile(SSB/Rebuild, 0.75))

  Reference_Landings <- RefLandingValues |> filter(tolower(Stock)==tolower(stock)) |>
    summarise(Ref_Landing=sum(Ref_Landing),
              Ref_Discard=sum(Ref_Discard))

  STY <- CatchDF |>
    dplyr::filter(Year %in% 2025:2029) |>
    dplyr::group_by(Sim, MP, MP_Name, Rec_Reduction) |>
    summarise(Landings=median(Landings)) |>
    dplyr::group_by(MP, MP_Name, Rec_Reduction) |>
    summarize(RelSTY=median(Landings /Reference_Landings$Ref_Landing))
              # Lower=quantile(Landings /Reference_Landings$Ref_Landing, 0.25),
              # Upper=quantile(Landings /Reference_Landings$Ref_Landing, 0.75))



  LTY <- CatchDF |>
    dplyr::filter(Year %in% 2030:2034) |>
    dplyr::group_by(Sim, MP, MP_Name, Rec_Reduction) |>
    summarise(Landings=median(Landings)) |>
    dplyr::group_by(MP, MP_Name, Rec_Reduction) |>
    summarize(RelLTY=median(Landings /Reference_Landings$Ref_Landing))

              # Lower=quantile(Landings /Reference_Landings$Ref_Landing, 0.25),
              # Upper=quantile(Landings /Reference_Landings$Ref_Landing, 0.75))
              #


  Discards <- CatchDF |>
    dplyr::group_by(Sim, MP, MP_Name, Rec_Reduction) |>
    mutate(RelDiscards=Discards/(Landings+Discards)) |>
    dplyr::group_by(MP, MP_Name, Rec_Reduction) |>
    summarize(RelDiscards=median(RelDiscards))


  df <-  left_join(SSBDF, STY)
  df <-  left_join(df, LTY)
  df <-  left_join(df, Discards)
  st_list[[st]] <- df

  st_list[[st]]$Stock <- stock
}

DF <- do.call('rbind', st_list)

# row_ind <- strsplit(DF$MP_Name, '_') |> lapply(length)
# DF <- DF[which(unlist(row_ind) < 3),]


make_plot <- function(DF, stock='Red Snapper',
                      mp=NULL, mytheme=NULL, singleMP=NULL) {


  df <- DF |> dplyr::filter(Stock%in%stock)
  df$Stock <- factor(df$Stock, levels=c('Red Snapper',
                                        'Gag Grouper',
                                        'Black Sea Bass'), ordered = TRUE)

  if (!is.null(mp)) {
    df <- df |> dplyr::filter(MP%in% mp)
  }

  ymaxSTY <- ceiling(max(df$RelSTY))
  ymaxLTY <- ceiling(max(df$RelLTY))
  yYield <- max(c(ymaxSTY, ymaxLTY))
  ymaxDiscards <- ceiling(max(df$RelDiscards))



  if (!is.null(singleMP)) {
    df <- df |> dplyr::filter(MP_Name %in%mp)
  }

  nms <- strsplit(df$MP_Name, '_')
  for (i in seq_along(nms)) {
    if (length(nms[[i]]) ==2)
      nms[[i]] <- nms[[i]][2]
    if (length(nms[[i]]) ==3)
      nms[[i]] <- paste(nms[[i]][2:3], collapse='_')
    if (df$Rec_Reduction[i]>0)
      nms[[i]] <- paste(nms[[i]], 1-df$Rec_Reduction[i], sep='_')
  }

  df$MP_Name <- unlist(nms)
  df$MP_Name <- factor(df$MP_Name, levels=unique(unlist(nms)), ordered = TRUE)


  p1 <- ggplot(df, aes(x=RelSSB , y=RelSTY , color=MP_Name)) +
    geom_point() +
    facet_wrap(~Stock) +
    ggrepel::geom_text_repel(aes(label=MP_Name)) +
    expand_limits(x=c(0,1), y=c(0,yYield)) +
    geom_vline(xintercept = 1, linetype=2, color='darkgray') +
    geom_hline(yintercept = 1, linetype=2, color='darkgray') +
    scale_x_continuous( breaks=scales::pretty_breaks()) +
    scale_y_continuous( breaks=scales::pretty_breaks()) +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size=14),
          panel.grid.minor = element_blank()) +
    labs(x='',
         y='Relative STY') +
    guides(color='none') +
    scale_color_brewer(palette='Dark2', type='qual')

  ymax <- ceiling(max(df$RelLTY))

  p2 <- ggplot(df, aes(x=RelSSB, y=RelLTY , color=MP_Name)) +
    geom_point() +
    facet_wrap(~Stock) +
    ggrepel::geom_text_repel(aes(label=MP_Name)) +
    expand_limits(x=c(0,1), y=c(0,yYield)) +
    geom_vline(xintercept = 1, linetype=2, color='darkgray') +
    geom_hline(yintercept = 1, linetype=2, color='darkgray') +
    scale_x_continuous( breaks=scales::pretty_breaks()) +
    scale_y_continuous( breaks=scales::pretty_breaks()) +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x='',
         y='Relative LTY') +
    guides(color='none') +
    scale_color_brewer(palette='Dark2', type='qual')

  ymax <- ceiling(max(df$RelDiscards))

  p3 <- ggplot(df, aes(x=RelSSB , y=RelDiscards  , color=MP_Name)) +
    geom_point() +
    facet_wrap(~Stock) +
    ggrepel::geom_text_repel(aes(label=MP_Name)) +
    expand_limits(x=c(0,1), y=c(0,ymaxDiscards)) +
    geom_vline(xintercept = 1, linetype=2, color='darkgray') +
    scale_x_continuous( breaks=scales::pretty_breaks()) +
    scale_y_continuous( breaks=scales::pretty_breaks()) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x='SB/Rebuild',
         y='Fraction Discarded') +
    guides(color='none') +
    scale_color_brewer(palette='Dark2', type='qual')


  if (!is.null(mytheme)) {
    p1 <- p1 + mytheme
    p2 <- p2 + mytheme
    p3 <- p3 + mytheme
  }

  p1 / p2 / p3

}


make_plot(DF,stock=c('Red Snapper',
                     'Gag Grouper',
                     'Black Sea Bass'),
          mp='SQ_0')

ggsave('img/December2024/TradeOff/SQ.png',width=8, height=6.66)

make_plot(DF,stock=c('Red Snapper',
                     'Gag Grouper',
                     'Black Sea Bass'),
          mp=c('SQ_0', 'SQ_0.65'))

ggsave('img/December2024/TradeOff/SQ_0.35.png',width=8, height=6.66)


make_plot(DF,stock=c('Red Snapper',
                     'Gag Grouper',
                     'Black Sea Bass'),
          mp=c('SQ_0', 'SQ_0.65', 'SQ_FR_OS_0'))

ggsave('img/December2024/TradeOff/SQ_OS.png',width=8, height=6.66)




make_plot(DF,stock=c('Red Snapper'),
          mp='SQ_0')

ggsave('img/December2024/TradeOff/RS_Example.png',width=2.66, height=6.66)






make_plot(DF, mp=c('SQ_0', 'SQ_0.65', 'SQ_OS_0'))





make_plot(DF,stock='Gag Grouper', mp='SQ')
ggsave('img/December2024/TradeOff/GG_SQ.png', width=7.2, height=2.4)

make_plot(DF,stock='Black Sea Bass', mp='SQ')
ggsave('img/December2024/TradeOff/BS_SQ.png', width=7.2, height=2.4)

p1 <- make_plot(DF,mp='SQ', mytheme=theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank()))

p2 <- make_plot(DF,stock='Gag Grouper', mp='SQ',
                , mytheme=theme(axis.title.x = element_blank(),
                                axis.text.x = element_blank()))
p3 <- make_plot(DF,stock='Black Sea Bass', mp='SQ')

p <- cowplot::plot_grid(p1, p2, p3, ncol=1)
ggsave('img/December2024/TradeOff/All_SQ.png', width=7.2, height=7.2)




p1 <- make_plot(DF,mp=c('SQ', 'SQ_FR'), mytheme=theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank()))

p2 <- make_plot(DF,stock='Gag Grouper', mp=c('SQ', 'SQ_FR')
                , mytheme=theme(axis.title.x = element_blank(),
                                axis.text.x = element_blank()))
p3 <- make_plot(DF,stock='Black Sea Bass', mp=c('SQ', 'SQ_FR'))

p <- cowplot::plot_grid(p1, p2, p3, ncol=1)
ggsave('img/December2024/TradeOff/SQ_FR.png', width=7.2, height=7.2)




p1 <- make_plot(DF,mp=c('SQ', 'SQ_OS'), mytheme=theme(axis.title.x = element_blank(),
                                                      axis.text.x = element_blank()))

p2 <- make_plot(DF,stock='Gag Grouper', mp=c('SQ', 'SQ_OS')
                , mytheme=theme(axis.title.x = element_blank(),
                                axis.text.x = element_blank()))
p3 <- make_plot(DF,stock='Black Sea Bass', mp=c('SQ', 'SQ_OS'))

p <- cowplot::plot_grid(p1, p2, p3, ncol=1)
ggsave('img/December2024/TradeOff/SQ_OS', width=7.2, height=7.2)






p1 <- make_plot(DF, mytheme=theme(axis.title.x = element_blank(),
                                          axis.text.x = element_blank()))

p2 <- make_plot(DF,stock='Gag Grouper',
                , mytheme=theme(axis.title.x = element_blank(),
                                axis.text.x = element_blank()))
p3 <- make_plot(DF,stock='Black Sea Bass')

p <- cowplot::plot_grid(p1, p2, p3, ncol=1)
ggsave('img/December2024/TradeOff/All.png', width=7.2, height=7.2)




