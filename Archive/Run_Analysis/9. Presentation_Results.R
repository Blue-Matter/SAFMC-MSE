library(SAMSE)
library(patchwork)

Stock_Names <- c('Red Snapper', 'Gag Grouper', 'Black Sea Bass')

Ref_DF <- readRDS('Misc_Objects/ref_df.rda')
Ref_DF$stock <- tolower(Ref_DF$Stock)
mse_files <- list.files('MSE_Objects')

# Spatial Distribution ----

fls <- mse_files[grepl('BaseCase', mse_files)]
fls <- fls[grepl('_SQ.mmse', fls)]


for (i in 1:3) {

  MSE <- readRDS(file.path('MSE_Objects', fls[i]))

  txt <- strsplit(fls[i], '_SQ.mmse')
  stck <- strsplit(txt[[1]], 'BaseCase_')[[1]][2]
  stock <- switch(stck,
                  'RS'='Red snapper',
                  'GG'= 'Gag grouper',
                  'BS' = 'Black sea bass')

  maxage <- MSE@Stocks[[1]]@maxage

  MSE@multiHist[[1]][[1]]@AtAge$VBiomass[1,,MSE@nyears,]
  MSE@multiHist[[1]][[2]]@AtAge$VBiomass[1,,MSE@nyears,]
  MSE@multiHist[[1]][[3]]@AtAge$VBiomass[1,,MSE@nyears,]

  b_dist <- MSE@multiHist[[1]][[1]]@AtAge$VBiomass[1,,MSE@nyears,]

  b_dist <- MSE@multiHist[[1]][[1]]@AtAge$Number[1,,MSE@nyears,]

  wght <-  matrix(MSE@multiHist[[1]][[1]]@AtAge$Weight[1,,MSE@nyears],
                  nrow=maxage+1,ncol=6)
  wght[wght<=0] <- 0.001

  b_dist <- b_dist * wght

  df <- data.frame(Age=0:maxage,
                   Region=rep(c('NC & SC', 'GA - Cp. C.', 'Cp. C. - Fl'),
                              each=(maxage+1)*2),
                   Depth=rep(c('Nearshore', 'Offshore'), each=(maxage+1)) ,
                   Biomass=as.vector(b_dist), Stock=stock)


  by <- 0.01
  breaks <- seq(0, 1, by=by)
  mids <- seq(by*0.5, by=by, length.out=length(breaks)-1)
  df <- df |> dplyr::group_by(Age) |>
    dplyr::mutate(Biomass=Biomass/sum(Biomass)) |>
    dplyr::mutate(b=cut(Biomass, breaks=breaks))

  df$b <- factor(df$b, ordered = TRUE, levels=levels(df$b))
  df$val <- mids[as.numeric(df$b)]

  ggplot(df|> dplyr::filter(Age>0), aes(x=Depth, y=Region)) +
    facet_wrap(~Age, ncol=5) +
    geom_tile(aes(fill=Biomass), alpha=0.5) +
    geom_text(aes(label = round(Biomass, 2)), size=3) +
    theme_classic() +
    guides(fill='none') +
    scale_fill_gradientn(limits = c(0,1),
                         colours=c("lightblue", "darkblue"),
                         breaks=breaks,
                         labels=format(breaks)) +
    theme(axis.title = element_text(size=8),
          axis.text = element_text(size=6),
          strip.text = element_text(size=8),
          strip.background = element_blank())

  nm <- paste0(stock, '_spatial.png')

  width <- 5.5
  nrow <- ceiling((maxage)/5)
  height <- 0.875 * nrow
  ggsave(file.path('img', nm), width=width, height=height)
}






# Base Case

## SQ
library(openMSE)
library(dplyr)
library(ggplot2)
om <- 'BaseCase'
mp_code <- 'SQ'

DF_SQ <- make_DFs(om, mp_code='SQ')
DF_FR <- make_DFs(om, mp_code='SQ_FR')
DF_MLL <- make_DFs(om, mp_code='SQ_MLL')
DF_NS <- make_DFs(om, mp_code='SQ_NS')
DF_OS <- make_DFs(om, mp_code='SQ_OS')


DF_FR_OS <- make_DFs(om, mp_code='SQ_FR_OS')

FM <- DF_SQ$FM

plotTS_FM(FM, c('RS', 'GG', 'BS'), rel.to = TRUE)
ggsave('img/F_TS_SQ_0.png', width=5, height=2)


SSB <- bind_rows(DF_SQ$SSB, DF_FR$SSB, DF_MLL$SSB,
                 DF_NS$SSB, DF_OS$SSB,
                 DF_FR_OS$SSB)

Landings <- bind_rows(DF_SQ$Landings,
                      DF_FR$Landings,
                      DF_MLL$Landings,
                      DF_NS$Landings,
                      DF_OS$Landings,
                      DF_FR_OS$Landings)


## Overall - SB Rebuild ----

plotTS_SSB(SSB, stock=c('RS', 'GG', 'BS'),  period='Historical', inc.ref.labels = TRUE)
ggsave('img/SB_Hist.png', width=8.5, height=2.5)


plot_rebuild_matrix(c('RS', 'GG', 'BS'))
ggsave('img/Rebuild_Matrix.png', width=8.5, height=2.5)


plot_rebuild_matrix(c('RS', 'GG', 'BS'),
                    selectMPs=data.frame(MP_Name=c('SQ', 'SQ', 'SQ_OS', 'SQ_FR_OS'),
                                         Rec_Reduction=c(1,0.35,1,1)))

ggsave('img/Rebuild_Matrix_2.png', width=8.5, height=2.5)

# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
#
# gg_color_hue(4)
#
# "#F8766D" "#7CAE00" "#00BFC4" "#C77CFF"

## ---- SQ ----

stocks <- c('RS', 'GG', 'BS')

mp <- 'SQ_0'

plotTS_SSB(SSB, stock=stocks, mp=mp, inc.perc = TRUE ) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_SQ.png', width=6.5, height=2.5)

### ----- Landings ----

plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = mp_names,
                mp.col=mp_colors,
                y_scales='free_y', byFleet = TRUE) +
  facet_grid(Stock~Fleet, scales='free_y') +
  theme_bw() +
  guides(linetype='none') +
  theme(strip.background = element_blank())

ggsave('img/Landings_fleets.png', width=5.5, height=4)

plotTS_Landings(Landings |> dplyr::filter(Year>=2020), stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = mp_names,
                mp.col=mp_colors,
                y_scales='free_y', byFleet = TRUE) +
  facet_grid(Stock~Fleet, scales='free_y') +
  theme_bw() +
  guides(linetype='none') +
  theme(strip.background = element_blank())

ggsave('img/Landings_fleets_recent.png', width=5.5, height=4)

#
# plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
#                 y_scales='free_y', byFleet = FALSE)
#
# ggsave('img/Landings.png', width=5.5, height=2.5)
#
# plotTS_Landings(Landings, 'RS', y_scales='free_y', byFleet = TRUE)
# ggsave('img/Landings_RS.png', width=5.5, height=2.5)
#
# plotTS_Landings(Landings, 'GG', y_scales='free_y')
# ggsave('img/Landings_GG.png', width=5.5, height=2.5)
#
# plotTS_Landings(Landings, 'BS', y_scales='free_y')
# ggsave('img/Landings_BS.png', width=5.5, height=2.5)



## SQ_0.65 ----

mp_colors <- c('darkgreen', '#706b6b')

mp <- 'SQ_0.65'
ref_mp <- 'SQ_0'

mp_names <- c("SQ 0.35 Gen. Rec. Effort", "Status Quo")

plotTS_SSB(SSB, stock=stocks, mp=mp, ref_mp=ref_mp, inc.perc = TRUE ) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_SQ_0.65.png', width=6.5, height=2.5)


plotTS_Landings(Landings |> dplyr::filter(Year>=2020), stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = mp_names,
                mp.col=mp_colors,
                y_scales='free_y', byFleet = TRUE) +
  facet_grid(Stock~Fleet, scales='free_y') +
  theme_bw() +
  guides(linetype='none') +
  theme(strip.background = element_blank())

ggsave('img/Landings_SQ_0.65_fleets.png', width=5.5, height=4)


# plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
#                 mp=mp,
#                 ref_mp=NULL,
#                 mp_names = mp_names,
#                 mp.col=mp_colors,
#                 y_scales='free_y', byFleet = FALSE)
#
# ggsave('img/Landings_SQ_0.65.png', width=5.5, height=2.5)
#


#
# plotTS_Landings(Landings, 'RS', mp=mp,
#                 ref_mp=NULL,
#                 mp_names = mp_names,
#                 y_scales='free_y', mp.col=mp_colors)
#
# ggsave('img/Landings_SQ_0.65_RS.png', width=5.5, height=2.5)
#
# plotTS_Landings(Landings, 'GG',  mp=mp, ref_mp=ref_mp,
#                 mp_names = mp_names,
#                 y_scales='free_y', mp.col=mp_colors)
#
# ggsave('img/Landings_SQ_0.65_GG.png', width=5.5, height=2.5)
#
# plotTS_Landings(Landings, 'BS',  mp=mp, ref_mp=ref_mp,
#                 mp_names = mp_names,
#                 y_scales='free_y', mp.col=mp_colors)
# ggsave('img/Landings_SQ_0.65_BS.png', width=5.5, height=2.5)
#

## SQ_OS ----

mp <- 'SQ_OS_0'
ref_mp <- 'SQ_0'

plotTS_SSB(SSB, stock=stocks, mp=mp, ref_mp=ref_mp, inc.perc = TRUE ) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_SQ_OS.png', width=6.5, height=2.5)


plotTS_Landings(Landings |> dplyr::filter(Year>=2020), stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = mp_names,
                mp.col=mp_colors,
                y_scales='free_y', byFleet = TRUE) +
  facet_grid(Stock~Fleet, scales='free_y') + theme_bw() +
  guides(linetype='none') +
  theme(strip.background = element_blank())

ggsave('img/Landings_SQ_OS_fleets.png', width=5.5, height=4)


# mp_names <- c("SQ Offshore Effort", "Status Quo")
#
# plotTS_Landings(Landings, c('RS', 'GG', 'BS'),
#                 mp=mp, ref_mp=NULL, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors, byFleet = FALSE)
#
# ggsave('img/Landings_SQ_OS.png', width=5.5, height=2.5)
#
#
# plotTS_Landings(Landings, 'RS', mp=mp, ref_mp=NULL, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_OS_RS.png', width=9, height=5)
#
# plotTS_Landings(Landings, 'GG',  mp=mp, ref_mp=ref_mp, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_OS_GG.png', width=9, height=5)
#
# plotTS_Landings(Landings, 'BS',  mp=mp, ref_mp=ref_mp,y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_OS_BS.png', width=9, height=5)
#
#
# plotTS_Landings(Landings, stock=c('RS', 'GG', 'BS'),
#                 mp=mp,
#                 ref_mp=NULL,
#                 mp_names = mp_names,
#                 mp.col=mp_colors,
#                 y_scales='free_y', byFleet = TRUE) +
#   facet_grid(Stock~Fleet, scales='free_y')
#
# ggsave('img/Landings_SQ_OS_fleets.png', width=5.5, height=4.5)


## SQ_FR_OS ----

mp <- 'SQ_FR_OS_0'
ref_mp <- 'SQ_0'

mp_names <- c("SQ Offshore Effort with Full Retention", "Status Quo")

plotTS_SSB(SSB, stock=stocks, mp=mp, ref_mp=ref_mp, inc.perc = TRUE ) +
  ggpp::geom_table(data =  calc_rebuild_table(SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_SQ_FR_OS.png', width=6.5, height=2.5)


plotTS_Landings(Landings |> dplyr::filter(Year>=2020), stock=c('RS', 'GG', 'BS'),
                mp=mp,
                ref_mp=NULL,
                mp_names = mp_names,
                mp.col=mp_colors,
                y_scales='free_y', byFleet = TRUE) +
  facet_grid(Stock~Fleet, scales='free_y') +
  theme_bw() +
  guides(linetype='none') +
  theme(strip.background = element_blank())

ggsave('img/Landings_SQ_FR_OS_fleets.png', width=5.5, height=4)


# plotTS_Landings(Landings, c('RS', 'GG', 'BS'),
#                 mp=mp, ref_mp=NULL, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors, byFleet = FALSE)
#
# ggsave('img/Landings_SQ_FR_OS.png', width=5.5, height=2.5)
#
#
# plotTS_Landings(Landings, 'RS', mp=mp, ref_mp=ref_mp, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_FR_OS_RS.png', width=9, height=5)
#
# plotTS_Landings(Landings, 'GG',  mp=mp, ref_mp=ref_mp, y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_FR_OS_GG.png', width=9, height=5)
#
# plotTS_Landings(Landings, 'BS',  mp=mp, ref_mp=ref_mp,y_scales='free_y',
#                 mp_names = mp_names,mp.col=mp_colors)
# ggsave('img/Landings_SQ_FR_OS_BS.png', width=9, height=5)



## ---- Sensitivity Tests ----

SSB <- bind_rows(DF_SQ$SSB, DF_FR$SSB, DF_MLL$SSB,
                 DF_NS$SSB, DF_OS$SSB,
                 DF_FR_OS$SSB)

### ---- Reduced Rec Effort ----
om <- 'LowerRecEffort'

LowerRec_SQ <- make_DFs(om, mp_code='SQ')

SSB$Model <- "Base Case"
LowerRec_SQ$SSB$Model <- 'Reduced. Rec. Removals'

DF <- dplyr::bind_rows(SSB, LowerRec_SQ$SSB)

plotTS_SSB(DF, stock=c('RS', 'GG', 'BS'),
           period='Historical', inc.ref.labels = TRUE, rel.to = FALSE) +
  facet_grid(Stock~Model, scales = 'free') +
  theme_bw()

ggsave('img/SB_Hist_LowerRec1.png', width=8, height=5)

plotTS_SSB(DF, stock=c('RS', 'GG', 'BS'),
           period='Historical', inc.ref.labels = TRUE, rel.to = TRUE) +
  facet_grid(Stock~Model, scales = 'free') +
  theme_bw()

ggsave('img/SB_Hist_LowerRec2.png', width=8, height=5)


# p1 <- plotTS_SSB(SSB, stock=c('RS', 'GG', 'BS'),
#            period='Historical', inc.ref.labels = TRUE, rel.to = FALSE) +
#   labs(title='Base Case')
#
# p2 <- plotTS_SSB(LowerRec_SQ$SSB, stock=c('RS', 'GG', 'BS'),
#            period='Historical', inc.ref.labels = TRUE, rel.to = FALSE) +
#   labs(title='Reduced. Rec. Removals')
#
# p <- p1 / p2
#
# ggsave('img/SB_Hist_LowerRec.png', width=9, height=5)


# plot_rebuild_matrix(c('RS', 'GG', 'BS'), om=om)
# ggsave('img/Rebuild_Matrix_LowerRecRemovals.png', width=8.5, height=2.5)
#


mp <- 'SQ_0'

plotTS_SSB(LowerRec_SQ$SSB, stock=c('RS', 'GG', 'BS'),
                 inc.ref.labels = TRUE,
           rel.to = FALSE,
           inc.perc = TRUE) +
  ggpp::geom_table(data =  calc_rebuild_table(LowerRec_SQ$SSB, stock=c('RS', 'GG', 'BS')),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_LowerRec.png', width=6.5, height=2.5)


### ---- Recent Recruitment ----


bc <- mse_files[grepl('BaseCase', mse_files)]
bc <- bc[grepl('_SQ.mmse', bc)]

rec1 <- mse_files[grepl('Rec1', mse_files)]
rec1 <- rec1[grepl('_SQ_REC1.mmse', rec1)]

plot_list <- list()

for (i in 1:3) {

  MSE_BC <- readRDS(file.path('MSE_Objects', bc[i]))
  MSE_Rec <- readRDS(file.path('MSE_Objects', rec1[i]))

  txt <- strsplit(bc[i], '_SQ.mmse')
  stck <- strsplit(txt[[1]], 'BaseCase_')[[1]][2]
  stock <- switch(stck,
                  'RS'='Red snapper',
                  'GG'= 'Gag grouper',
                  'BS' = 'Black sea bass')

  maxage <- MSE_BC@Stocks[[1]]@maxage

  years <- c(rev(seq(MSE_BC@Fleets[[1]][[1]]@CurrentYr[1], by=-1, length.out=MSE_BC@nyears)),
               seq(MSE_BC@Fleets[[1]][[1]]@CurrentYr[1]+1, by=1, length.out=MSE_BC@proyears))
  perr_bc <- MSE_BC@multiHist[[1]][[1]]@SampPars$Stock$Perr_y
  perr_rec <- MSE_Rec@multiHist[[1]][[1]]@SampPars$Stock$Perr_y

  perr_BC <- data.frame(Sim=1:MSE_BC@nsim, Year=rep(years, each=MSE_BC@nsim),
             Deviation=as.vector(perr_bc[,(maxage+1):(ncol(perr_bc))]),
             Stock=stock, Model='Base Case')

  perr_Rec <- data.frame(Sim=1:MSE_BC@nsim, Year=rep(years, each=MSE_BC@nsim),
                   Deviation=as.vector(perr_rec[,(maxage+1):(ncol(perr_rec))]),
                   Stock=stock, Model='Recent Recruitment')

  plot_list[[i]] <- dplyr::bind_rows(perr_BC, perr_Rec)
  plot_list[[i]]$Period <- 'Historical'
  plot_list[[i]]$Period[plot_list[[i]]$Year>=MSE_BC@Fleets[[1]][[1]]@CurrentYr[1]+1] <- 'Projection'

}

DF <- do.call('rbind', plot_list)

DF$Stock <- factor(DF$Stock, levels=Stock_levels(), ordered = TRUE)

DF <- DF |>
  dplyr::group_by(Stock, Model, Year, Period) |>
  dplyr::summarise(Mean=mean(Deviation))

ggplot(DF |> dplyr::filter(Year>=2010),
       aes(x=Year, y=Mean, color=Period)) +
  facet_grid(Stock~Model, scales='free_y') +
  expand_limits(y=0) +
  geom_line() +
  guides(color='none') +
  theme_bw()


om <- 'Rec1'

Rec_SQ <- make_DFs(om, mp_code='SQ')


mp <- 'SQ_0'
ref_mp <- 'NULL'

plotTS_SSB(Rec_SQ$SSB, stock=stocks, mp=mp, ref_mp=ref_mp, inc.perc = TRUE ) +
  ggpp::geom_table(data =  calc_rebuild_table(Rec_SQ$SSB, mp, stock=stocks),
                   aes(x = x, y = y,label = tbl),
                   hjust = 'inward', vjust = 'inward',
                   size=2)

ggsave('img/SB_Rec1_SQ.png', width=6.5, height=2.5)

# plot_rebuild_matrix(c('RS', 'GG', 'BS'), om=om)
# ggsave('img/Rebuild_Matrix_LowerRec.png', width=8.5, height=2.5)




## ---- Selectivity ----

Selectivity <- get_selectivity_retention()
Selectivity$OM |> unique()

cols <- RColorBrewer::brewer.pal(4, 'Dark2')

df <- Selectivity |> dplyr::filter(OM=='BaseCase', Fleet!='Commercial Dive',
                                   Stock=='Red snapper')


p <- ggplot(Selectivity |> dplyr::filter(OM=='BaseCase', Fleet!='Commercial Dive',
                                         Stock=='Red snapper'),
            aes(x=Age, y=value, color=name, linetype=name)) +
  facet_grid(~Fleet, scales='free_x') +
  scale_color_manual(values=cols[1:3]) +
  scale_linetype_manual(values=c(1:3)) +
  geom_line(linewidth=0.7) +
  geom_line(aes(y=Maturity), linetype=4, color='darkgray') +
  theme_bw() +
  labs(y='Probability', color='', linetype='') +
  theme(legend.key.width = unit(2, "line"),
        strip.background = element_blank())
p

ggsave('img/Selectivity_RS.png', p, width=8.5, height=3)





## ---- Reference Points ----

Hist_RS <- readRDS('Hist_Objects/BaseCase_RS.hist')
df <- Calc_RS_Ref_Points(Hist_RS)

Hist_GG <- readRDS('Hist_Objects/BaseCase_GG.hist')
df <- Calc_GG_Ref_Points(Hist_GG)
df

Hist_BS <- readRDS('Hist_Objects/BaseCase_BS.hist')
df <- Calc_BS_Ref_Points(Hist_BS)
df

Hist <- Hist_BS
