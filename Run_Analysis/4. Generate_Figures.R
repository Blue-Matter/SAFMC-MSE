library(SAMSE)
library(ggplot2)

# Report Stock Status & Reference Points
DF <- get_stock_status()

# make figure
pDF <- DF |> dplyr::select(Stock, OM, Year, "SSB/MSST"=SSB_MSST, 'F/MFMT'=F_MFMT) |>
  tidyr::pivot_longer(cols=c("SSB/MSST", 'F/MFMT'))

pDF$name <- factor(pDF$name, levels=c("SSB/MSST", 'F/MFMT'), ordered = TRUE)

cols <- RColorBrewer::brewer.pal(length(unique(pDF$OM)), 'Set1')

p <- ggplot(pDF, aes(x=Year, y=value, color=OM, linetype=OM)) +
  facet_grid(name~Stock, scales='free_y') +
  geom_line() +
  geom_hline(yintercept = 1, linetype=2) +
  scale_color_manual(values=cols) +
  scale_linetype_manual(values=c(1,2,3,4)) +
  labs(y='', color='', linetype='') +
  theme_bw()
p

ggsave('img/Stock_Status.png', p, width=8, height=4)

DF |> dplyr::filter(Stock=='Red snapper',
                    OM %in% c('Base Case', 'Lower Rec. Effort'),
                    Year==2019)

# save reference points
ref_df <- DF |> dplyr::distinct(Stock,
                                OM,
                                MSST=round(MSST,0),
                                MFMT=round(MFMT,2)) |>
  dplyr::arrange(Stock, OM)

saveRDS(ref_df, 'Misc_Objects/ref_df.rda')


# plot landings and discards by stock and fleet


Landings <- get_landings_discards()

p <- ggplot(Landings,
       aes(x=Year, y=value/1000, color=name, linetype=name)) +
  facet_grid(Fleet~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Landings/Discards (1000 t)', color='', linetype='') +
  scale_color_manual(values=cols[2:3]) +
  scale_linetype_manual(values=c(1,2)) +
  theme_bw()

ggsave('img/Landings_Discards.png', p, width=8, height=6.5)


# selectivity and retention

Selectivity <- get_selectivity_retention()

p <- ggplot(Selectivity, aes(x=Age, y=value, color=name, linetype=name)) +
  facet_grid(Fleet~Stock, scales='free_x') +
  scale_color_manual(values=cols[2:4]) +
  scale_linetype_manual(values=c(1:3)) +
  geom_line() +
  theme_bw() +
  labs(y='Probability', color='', linetype='')
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

p <- ggplot(RecDevs |> filter(Year %in% yrs, Sim%in% sims),
       aes(x=Year, y=Value, group=Sim, color=Period)) +
  facet_grid(Stock~Sim, scales='free_y') +
  geom_hline(yintercept = 1, linetype=2, color='darkgray') +
  geom_line() +
  scale_color_manual(values=cols[2:3]) +
  scale_linetype_manual(values=c(1:2)) +
  theme_bw() +
  labs(y='Recruitment Deviation', color='', linetype='')

ggsave('img/RecDevs.png', p, width=9, height=4.5)



