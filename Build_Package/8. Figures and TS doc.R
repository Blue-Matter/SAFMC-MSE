
# Steps:
# 1. run this script to update figures for TS doc
# 2. Compile docs/TS html
# 3. Copy TS html over to 'homepage'

#
library(SAMSE)
MOM_multiHist <- readRDS('Hist_Objects/01.hist')


# Plots ----
## Selectivity & Retention Curves by Fleet ----
Select_Retain <- get_at_Age(MOM_multiHist) %>%
  filter(Sim==1, Variable %in% c('Select', 'Retention'), Year==2019)

Select_Retain <- left_join(Select_Retain, Fleet_Details)

Process_Select <- function(df, ...) {
  sel_max <- df %>% filter(Variable=='Select') %>% filter(Value==max(Value)) %>% distinct(Value)
  ret_max <- df %>% filter(Variable=='Retention') %>% filter(Value==max(Value)) %>% distinct(Value)

  ratio <- ret_max$Value/sel_max$Value
  df <- df %>% group_by(Variable) %>% mutate(Value=Value/max(Value))
  df$Value[!is.finite(df$Value)] <- 0
  df$Value[df$Variable=='Retention'] <- df$Value[df$Variable=='Retention'] * ratio
  df
}

# modify to scale to maximum value of 1
Select_Retain_2 <- Select_Retain %>% group_by(Stock, Fleet) %>% dplyr::group_modify(.,Process_Select)
Select_Retain_2$Variable[Select_Retain_2$Variable=='Select'] <- 'Selectivity'

# add Zeros
Select_Retain_2$Value[Select_Retain_2$Stock=='Red Snapper' & Select_Retain_2$Fleet=='Commercial Dive'] <- 0

Select_Retain_2$Value[Select_Retain_2$Stock!='Red Snapper' & Select_Retain_2$Age>=17] <- NA

Select_Retain_2$Stock <- factor(Select_Retain_2$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)
Select_Retain_2$Fleet[Select_Retain_2$Fleet=='cDV'] <- 'Commercial Dive'

Select_Retain_2$Variable <- factor(Select_Retain_2$Variable, levels=c('Selectivity', 'Retention'), ordered = TRUE)
Select_Retain_2$Season <- factor(Select_Retain_2$Season, levels=c('On', 'Off'), ordered = TRUE)


ggplot(Select_Retain_2 %>% filter(Stock=='Red Snapper', Fleets !="Commercial Dive"),
       aes(x=Age, y=Value, linetype=Variable)) +
  facet_grid(Season~Fleets) +
  geom_line() +
  theme_bw() +
  labs(linetype='', x='Age (year)', y='Probability')

ggsave('img/OM_Properties/RS_Select_hist.png', width=8, height=4)

ggplot(Select_Retain_2 %>% filter(Stock!='Red Snapper'),
       aes(x=Age, y=Value, linetype=Variable)) +
  facet_grid(Season~Fleets) +
  geom_line() +
  theme_bw() +
  labs(linetype='', x='Age (year)', y='Probability')

ggsave('img/OM_Properties/GG_Select_hist.png', width=8, height=4)

## Removals and Landings ----
# Total Removals
Removals <- get_Removals(MOM_multiHist)
Removals$Stock <- factor(Removals$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)
Removals <- Removals %>% group_by(Year, Sim, Stock, Variable) %>%
  summarize(Value=sum(Value))

ggplot(Removals %>% filter(Sim==1), aes(x=Year, y=Value/1000)) +
  facet_wrap(~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Total Removals (1,000 t)') +
  expand_limits(y=0) +
  theme_bw()

ggsave('img/OM_Properties/RS_GG_Removals_hist.png', width=6, height=3)


Landings <- get_Landings(MOM_multiHist)
Landings$Stock <- factor(Landings$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)
Landings <- left_join(Landings, Fleet_Details)

Landings <- Landings %>% group_by(Year, Sim, Stock, Fleets, Variable) %>%
  summarize(Value=sum(Value))

ggplot(Landings %>% filter(Sim==1), aes(x=Year, y=Value/1000)) +
  facet_grid(Fleets~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Total Landings (1,000 t)') +
  theme_bw()

ggsave('img/OM_Properties/RS_GG_Landings_hist.png', width=6, height=8)

Removals <- get_Removals(MOM_multiHist)
Removals$Stock <- factor(Removals$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)
Removals <- left_join(Removals, Fleet_Details)

Removals <- Removals %>% group_by(Year, Sim, Stock, Fleets, Variable) %>%
  summarize(Value=sum(Value))
Removals$Value <- Removals$Value - Landings$Value

ggplot(Removals %>% filter(Sim==1), aes(x=Year, y=Value/1000)) +
  facet_grid(Fleets~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Total Discards (1,000 t)') +
  theme_bw()

ggsave('img/OM_Properties/RS_GG_Discards_hist.png', width=6, height=8)

# Spawning Biomass
SSB <- get_SSB(MOM_multiHist)
SSB$Stock <- factor(SSB$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)

ggplot(SSB %>% filter(Sim==1), aes(x=Year, y=Value/1000)) +
  facet_wrap(~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Spawning Stock (1,000)') +
  theme_bw()

ggsave('img/OM_Properties/RS_GG_SSB_hist.png', width=6, height=3)


# ----- Reference Points -----

## Red Snapper -----
y <- ncol(MOM_multiHist[[1]][[1]]@TSdata$Find)
SP_SPR_df <- Calc_RS_Ref_Points(1, y, MOM_multiHist)

SPR_targ <- 0.3
ind <- which.min(abs(SP_SPR_df$SPR-SPR_targ))
pdf <- SP_SPR_df %>% tidyr::pivot_longer(., cols=4:5)

RS_Ref_df <- SP_SPR_df[ind,]
RS_Ref_df$MSST <- RS_Ref_df$SB*0.75


p1 <- ggplot(SP_SPR_df, aes(x=F, y=SB)) +
  geom_line(data=data.frame(x=RS_Ref_df$F, y=c(0, RS_Ref_df$SB)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,RS_Ref_df$F), y=RS_Ref_df$SB),
            aes(x=x, y=y),
            linetype=2) +
  geom_hline(data=data.frame(yintercept=RS_Ref_df$MSST, name='SB'),
             aes(yintercept=yintercept), linetype=3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Biomass (1E8 eggs)')

p1

p2 <- ggplot(SP_SPR_df, aes(x=F, y=SPR)) +
  geom_line(data=data.frame(x=RS_Ref_df$F, y=c(0, RS_Ref_df$SPR)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,RS_Ref_df$F), y=RS_Ref_df$SPR),
            aes(x=x, y=y),
            linetype=2) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Potential Ratio (SPR))')

p2

cowplot::plot_grid(p1,p2, labels=c('a)', 'b)'))

ggsave('img/OM_Properties/RS_Ref_Points.png', width=6, height=2.5)

saveRDS(RS_Ref_df, 'Hist_Objects/RS_basecase_ref_points.rda')


## Gag Grouper -----
y <- ncol(MOM_multiHist[[2]][[1]]@TSdata$Find)
GG_SP_SPR_df <- Calc_GG_Ref_Points(1, y, MOM_multiHist)

ind <- which.max(GG_SP_SPR_df$Removals)
GG_Ref_df <- GG_SP_SPR_df[ind,]
GG_Ref_df$MSST <- GG_Ref_df$SB*0.75

GG_Ref_df

p1 <- ggplot(GG_SP_SPR_df, aes(x=F, y=Removals/1000)) +
  geom_line(data=data.frame(x=GG_Ref_df$F, y=c(0, GG_Ref_df$Removals/1000)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,GG_Ref_df$F), y=GG_Ref_df$Removals/1000),
            aes(x=x, y=y),
            linetype=2) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Removals (mt)')

p2 <- ggplot(GG_SP_SPR_df, aes(x=F, y=SB)) +
  geom_line(data=data.frame(x=GG_Ref_df$F, y=c(0, GG_Ref_df$SB)),
            aes(x=x, y=y),
            linetype=2) +
  geom_line(data=data.frame(x=c(0,GG_Ref_df$F), y=GG_Ref_df$SB),
            aes(x=x, y=y),
            linetype=2) +
  geom_hline(data=data.frame(yintercept=GG_Ref_df$MSST),
             aes(yintercept=yintercept), linetype=3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x='Fishing Mortality (F)', y='Spawning Stock Biomass (mt)')

cowplot::plot_grid(p1,p2, labels=c('a)', 'b)'))

ggsave('img/OM_Properties/GG_Ref_Points.png', width=6, height=2.5)

saveRDS(GG_Ref_df, 'Hist_Objects/GG_basecase_ref_points.rda')


## SB and Reference Points ----
# Spawning Biomass

SSB <- get_SSB(MOM_multiHist) %>% filter(Sim==1)

ref <- RS_Ref_df %>% tidyr::pivot_longer(., col=c(SB, MSST))
p1 <- ggplot(SSB %>% filter(Stock=='Red Snapper'), aes(x=Year, y=Value/1000)) +
  geom_line() +
  labs(y='Spawning Stock (Eggs 1000)') +
  geom_hline(data=ref, aes(yintercept=value/1000, linetype=name)) +
  theme_bw() +
  labs(linetype='Reference Points')

ggsave('img/OM_Properties/RS_SSB_ref_hist.png', p1, width=6, height=4)


ref <- GG_Ref_df %>% tidyr::pivot_longer(., col=c(SB, MSST))
p2 <- ggplot(SSB %>% filter(Stock=='Gag Grouper'), aes(x=Year, y=Value/1000)) +
  geom_line() +
  labs(y='Spawning Stock (1000)') +
  geom_hline(data=ref, aes(yintercept=value/1000, linetype=name)) +
  theme_bw() +
  labs(linetype='Reference Points')

ggsave('img/OM_Properties/GG_SSB_ref_hist.png', p2, width=6, height=4)


# ---- Recruitment Deviations ----
# Base Case Model

Years <- get_Years(MOM_multiHist)

RSrec_deviations <- OM_01@cpars$`Red Snapper`$`Commercial Handline: On-Season`$Perr_y[,21:90] %>% log()
GGrec_deviations <- OM_01@cpars$`Gag Grouper`$`Commercial Handline: On-Season`$Perr_y[,21:90] %>% log()

df <- data.frame(RS=as.vector(RSrec_deviations), GG=as.vector(GGrec_deviations))
ind <- which(!(round(df$RS,3)==0 | round(df$GG,3)==0))

df <- df[ind,]
lims <- range(df)
p <- ggplot(df, aes(x=RS, y=GG)) +
  geom_point() +
  xlim(lims) +
  ylim(lims) +
  theme_bw() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title='Log Recruitment Deviations',
       x='Red Snapper',
       y='Gag')
p
ggsave('img/OM_Properties/correlated_rec_devs.png', p, width=5, height=5)

## Red Snapper

rec_deviations <- OM_01@cpars$`Red Snapper`$`Commercial Handline: On-Season`$Perr_y
dd <- dim(rec_deviations)
rec_deviations <- rec_deviations[,(OM_01@Stocks$`Red Snapper`@maxage+1):dd[2]]

RS_dev_devs <- data.frame(Sim=1:nsim, Year=rep(Years$Year, each=nsim), Rec_dev=as.vector(rec_deviations), Stock='Red Snapper')
RS_dev_devs <- left_join(RS_dev_devs, Years, by = join_by(Year))

RS_dev_devs_p <-  RS_dev_devs  %>% filter(Sim %in% 1:9)
ggplot(RS_dev_devs_p) +
  facet_wrap(~Sim, nrow=3) +
  geom_line(aes(x=Year, y=Rec_dev))  +
  geom_line(data=RS_dev_devs_p %>% filter(Period=='Projection'),
            aes(x=Year, y=Rec_dev, color=Period)) +
  theme_bw() +
  scale_color_manual(values='blue') +
  labs(y='Recruitment Deviation') +
  guides(color='none')

ggsave('img/OM_Properties/RS_rec_devs.png', width=6, height=6)

## Gag Grouper
rec_deviations <- OM_01@cpars$`Gag Grouper`$`Commercial Handline: On-Season`$Perr_y
dd <- dim(rec_deviations)
rec_deviations <- rec_deviations[,(OM_01@Stocks$`Gag Grouper`@maxage+1):dd[2]]

GG_dev_devs <- data.frame(Sim=1:nsim, Year=rep(Years$Year, each=nsim), Rec_dev=as.vector(rec_deviations), Stock='Gag Grouper')
GG_dev_devs <- left_join(GG_dev_devs, Years, by = join_by(Year))

GG_dev_devs_p <-  GG_dev_devs  %>% filter(Sim %in% 1:9)
ggplot(GG_dev_devs_p) +
  facet_wrap(~Sim, nrow=3) +
  geom_line(aes(x=Year, y=Rec_dev))  +
  geom_line(data=GG_dev_devs_p %>% filter(Period=='Projection'),
            aes(x=Year, y=Rec_dev, color=Period)) +
  theme_bw() +
  scale_color_manual(values='blue') +
  labs(y='Recruitment Deviation') +
  guides(color='none')

ggsave('img/OM_Properties/GG_rec_devs.png', width=6, height=6)


## Statistical properties
RS_rec_dev_hist <- RS_dev_devs %>% filter(Sim==1, Period=='Historical') %>%
  mutate(log_dev=log(Rec_dev)) %>% filter(Year>=1976)

GG_rec_dev_hist <- GG_dev_devs %>% filter(Sim==1, Period=='Historical') %>%
  mutate(log_dev=log(Rec_dev)) %>% filter(Year>=1976)


calc_stats <- function(df) {
  tt <-acf(df$log_dev)
  data.frame(Stock=unique(df$Stock), SD=sd(df$log_dev), AC=tt$acf[2,1,1])
}

RS_rec_devs <- calc_stats(RS_rec_dev_hist)
GG_rec_devs <- calc_stats(GG_rec_dev_hist)

base_case_process_error <- bind_rows(RS_rec_devs, GG_rec_devs)
saveRDS(base_case_process_error, 'Hist_Objects/Base_Case_Process_Error.rda')


# make plot
rr <- qnorm(0.999, 0, max(base_case_process_error$SD))

p1 <- ggplot(RS_rec_dev_hist, aes(x=log_dev)) +

  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, args = list(mean =mean(RS_rec_dev_hist$log_dev), sd = sd(RS_rec_dev_hist$log_dev)),
                color='blue') +
  theme_bw() +
  expand_limits(x=c(-rr, max(rr))) +
  labs(x='Log Deviations', y='Count')

p3 <- ggplot(RS_rec_dev_hist, aes(x=Rec_dev)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dlnorm, args = list(mean =mean(RS_rec_dev_hist$log_dev), sd = sd(RS_rec_dev_hist$log_dev)),
                color='blue') +
  theme_bw() +
  expand_limits(x=c(0, max(rr))) +
  labs(x='Deviations', y='Count')


p2 <- ggplot(GG_rec_dev_hist, aes(x=log_dev)) +

  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, args = list(mean =mean(GG_rec_dev_hist$log_dev), sd = sd(GG_rec_dev_hist$log_dev)),
                color='blue') +
  theme_bw() +
  expand_limits(x=c(-rr, max(rr))) +
  labs(x='Log Deviations', y='Count')


p4 <- ggplot(GG_rec_dev_hist, aes(x=Rec_dev)) +

  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dlnorm, args = list(mean =mean(GG_rec_dev_hist$log_dev), sd = sd(GG_rec_dev_hist$log_dev)),
                color='blue') +
  theme_bw() +
  expand_limits(x=c(0, max(rr))) +
  labs(x='Deviations', y='Count')

cowplot::plot_grid(p1, p2, p3, p4, labels=c('a)', 'b)', 'c)', 'd)'))

ggsave('img/OM_Properties/Base_Case_recdev_stats.png', width=6, height=4)
