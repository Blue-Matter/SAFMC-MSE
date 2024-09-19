library(SAMSE)
library(ggplot2)

# Report Stock Status & Reference Points ----
DF <- get_stock_status()

# make figure
pDF <- DF |> dplyr::select(Stock, OM, Year, "SSB/MSST"=SSB_MSST, 'F/MFMT'=F_MFMT) |>
  tidyr::pivot_longer(cols=c("SSB/MSST", 'F/MFMT'))

pDF$name <- factor(pDF$name, levels=c("SSB/MSST", 'F/MFMT'), ordered = TRUE)

cols <- RColorBrewer::brewer.pal(length(unique(pDF$OM)), 'Dark2')


pDF$OM <- forcats::fct_recode(pDF$OM,
                    'Base Case'='BaseCase',
                    'Lower M'='LowM',
                    'Higher M'='HighM',
                    'Reduced Recreational Removals'='LowerRecEffort')

p <- ggplot(pDF |> filter(name=='SSB/MSST'),
            aes(x=Year, y=value)) +
  facet_grid(Stock~OM, scales='free_y') +
  geom_line(linewidth=0.7) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=c(1,2,3,4)) +
  labs(y='SB/MSST', color='', linetype='') +
  theme_bw() +
  theme(legend.key.width = unit(2, "line"),
        legend.position = 'bottom')

p
ggsave('img/SB_MSST.png', p, width=9, height=4)


p <- ggplot(pDF |> filter(name!='SSB/MSST'),
       aes(x=Year, y=value)) +
  facet_grid(Stock~OM, scales='free_y') +
  geom_line(linewidth=0.7) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=c(1,2,3,4)) +
  labs(y='F/MFMT', color='', linetype='') +
  theme_bw() +
  theme(legend.key.width = unit(2, "line"),
        legend.position = 'bottom')

ggsave('img/F_MFMT.png', p, width=9, height=4)

p <- ggplot(pDF, aes(x=Year, y=value, color=OM, linetype=OM)) +
  facet_grid(name~Stock, scales='free_y') +
  geom_line(linewidth=0.7) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=c(1,2,3,4)) +
  labs(y='', color='', linetype='') +
  theme_bw() +
  theme(legend.key.width = unit(2, "line"),
        legend.position = 'bottom')
p

ggsave('img/Stock_Status.png', p, width=6, height=4)


# save reference points
ref_df <- DF |> dplyr::distinct(Stock,
                                OM,
                                MSST=round(MSST,0),
                                MFMT=round(MFMT,2)) |>
  dplyr::arrange(Stock, OM)

ref_df_2 <- DF |> dplyr::group_by(Stock, OM) |>
  dplyr::filter(is.na(SSB_MSST)==FALSE) |>
  dplyr::filter(Year==max(Year)) |>
  dplyr::select(SSB_MSST, F_MFMT)


ref_df <- left_join(ref_df, ref_df_2)
saveRDS(ref_df, 'Misc_Objects/ref_df.rda')

# fishing mortality -----

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

F_mortality <- get_F()

F_mortality <- F_mortality |> dplyr::filter(name!='Interact')

ggplot(F_mortality, aes(x=Year, y=value, color=name, linetype=name)) +
  facet_grid(Stock~Fleet, scales='free_y') +
  geom_line(linewidth=0.7) +
  theme_bw() +
  labs(y='Fishing Mortality', color='', linetype='') +
  scale_color_manual(values=cols[c(1,2,3)]) +
  scale_linetype_manual(values=c(1,2,3)) +
  theme(legend.key.width = unit(2, "line"))

ggsave('img/Fishing_Mortality.png', width=8, height=4.5)



# plot landings and discards by stock and fleet ----

Landings |>  dplyr::filter(OM=='BaseCase') |>
  dplyr::group_by(Stock, Fleet) |>
  summarise(Min=min(Year), Max=max(Year))

Landings <- get_landings_discards()

p <- ggplot(Landings |> dplyr::filter(OM=='BaseCase'),
       aes(x=Year, y=value/1000, color=name, linetype=name)) +
  facet_grid(Stock~Fleet, scales = 'free_y') +
  geom_line(linewidth=0.7) +
  labs(y='Landings/Discards (1000 t)', color='', linetype='') +
  scale_color_manual(values=cols[1:2]) +
  scale_linetype_manual(values=c(1,2)) +
  theme_bw() +
  theme(legend.key.width = unit(2, "line"))

p

ggsave('img/Landings_Discards.png', p, width=8, height=4.5)


# selectivity and retention ----

Selectivity <- get_selectivity_retention()
Selectivity$OM |> unique()

p <- ggplot(Selectivity |> dplyr::filter(OM=='BaseCase'),
            aes(x=Age, y=value, color=name, linetype=name)) +
  facet_grid(Fleet~Stock, scales='free_x') +
  scale_color_manual(values=cols[1:3]) +
  scale_linetype_manual(values=c(1:3)) +
  geom_line(linewidth=0.7) +
  geom_line(aes(y=Maturity), linetype=4, color='darkgray') +
  theme_bw() +
  labs(y='Probability', color='', linetype='') +
  theme(legend.key.width = unit(2, "line"))
p

ggsave('img/Selectivity.png', p, width=8, height=6.5)

# Recruitment figure
RecDevs <- get_rec_devs()

RecDevs |> filter(Period=='Historical') |>
  group_by(Stock, Period) |>
  summarise(MinYear=min(Year),
            MaxYear=max(Year))

yrs <- RecDevs |> group_by(Stock) |>
  summarise(MinYear=min(Year),
            MaxYear=max(Year))
yrs <- max(yrs$MinYear):min(yrs$MaxYear)
sims <- 1:5 # sample(1:max(df$Sim), 5)

p <- ggplot(RecDevs |> filter(Year %in% yrs, Sim%in% sims,
                              OM=='BaseCase'),
       aes(x=Year, y=Value, group=Sim, color=Period)) +
  facet_grid(Stock~Sim, scales='free_y') +
  geom_hline(yintercept = 1, linetype=2, color='darkgray') +
  geom_line() +
  scale_color_manual(values=cols[2:3]) +
  scale_linetype_manual(values=c(1:2)) +
  theme_bw() +
  labs(y='Recruitment Deviation', color='', linetype='')

p
ggsave('img/RecDevs.png', p, width=9, height=4.5)

# ---- Projection Fishing Mortality ----


Ref_DF <- readRDS('Misc_Objects/ref_df.rda') |>
  dplyr::filter(OM=='BaseCase')


stocks <- c('RS', 'GG', 'BS')
df_list <- list()
for (st in seq_along(stocks)) {

  stock_code <- stocks[st]
  stock <- switch(stock_code,
                  'RS'="Red snapper",
                  "GG"="Gag grouper",
                  'BS'="Black sea bass"
  )

  file1 <- paste0('MSE_Objects/BaseCase_', stock_code, '_SQ.mmse')
  file2 <- paste0('MSE_Objects/BaseCase_', stock_code, '_SQ_EC.mmse')
  MSE_1 <- readRDS(file1)
  MSE_2 <- readRDS(file2)

  year1 <- MSE_1@multiHist[[1]][[1]]@OMPars$CurrentYr[1] +1
  years <- seq(year1, by=1, length.out=MSE_1@proyears)

  ref <- Ref_DF |> dplyr::filter(Stock==stock)

  # SQ
  effort_mod <- 1- gsub('SQ_', "", MSE_1@MPs[[1]]) |> as.numeric()

  Fs <- apply(MSE_1@FM[1,1,,,], 2:3, sum)

  df1 <- data.frame(Eff=effort_mod,
                   F=as.vector(Fs),
                   Year=rep(years, each=length(effort_mod)),
                   MFMT=ref$MFMT,
                   class='Base Case',
                   Stock=stock)


  # Effort Creep

  Fs <- apply(MSE_2@FM[1,1,,,], 2:3, sum)

  df2 <- data.frame(Eff=effort_mod,
                    F=as.vector(Fs),
                    Year=rep(years, each=length(effort_mod)),
                    MFMT=ref$MFMT,
                    class='Increasing Rec. Effort',
                    Stock=stock)


  df <- dplyr::bind_rows(df1, df2)
  df$Eff <- factor(df$Eff, ordered=TRUE, levels=unique(df$Eff))

  df_list[[st]] <- df
}
DF <- do.call('rbind', df_list)

eff_levels <- unique(DF$Eff)
eff_levels <- eff_levels[seq(1, by=2, length.out=length(eff_levels)/2)]

cols <- RColorBrewer::brewer.pal(length(eff_levels), 'Dark2')

label <- 'Gen. Rec. Effort'

DF$Stock <- factor(DF$Stock, levels=unique(DF$Stock), ordered = TRUE)
ggplot(DF |> dplyr::filter(Year %in% 2025:2044,
                           Eff%in%eff_levels),
       aes(x=Year, y=F, color=Eff, linetype=Eff)) +
  facet_grid(Stock~class, scales='free_y') +
  geom_hline(aes(yintercept = MFMT), linetype=2, color='black') +
  geom_line() +
  expand_limits(y=0) +
  scale_color_manual(values=cols) +
  theme_bw() +
  labs(x='Projection Year', y='Apical Fishing Mortality (F)',
       color=label,
       linetype=label) +
  theme(legend.key.width = unit(2, "line"))

ggsave('img/ApicalFishingMortality.png', width=8, height=5)
