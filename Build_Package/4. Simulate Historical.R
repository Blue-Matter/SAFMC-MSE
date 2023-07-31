library(SAMSE)

MOM_RS_GG <- readRDS('Hist_Objects/BaseCaseMOM.mom')

MOM_multiHist <- SimulateMOM(MOM_RS_GG)
saveRDS(MOM_multiHist, 'Hist_Objects/MOM_BaseCase.hist')



MOM_multiHist <- readRDS('Hist_Objects/MOM_BaseCase.hist')

Fleets <- unlist(lapply(strsplit(names(MOM_multiHist[[1]]), ':'), '[[', 1))
Fleet_Details <- data.frame(Fleets=Fleets,
                            Fleet=names(MOM_multiHist[[1]]),
                            Category=c('Comm', 'Rec', 'Rec', 'Comm', 'Rec', 'Rec', 'Comm'),
                            Season=c('On', 'On', 'On', 'Off', 'Off', 'Off', 'On'))

Fleet_Details$Fleet <- factor(Fleet_Details$Fleet, levels=Fleet_Details$Fleet, ordered = TRUE)
Fleet_Details$Fleets <- factor(Fleet_Details$Fleets, levels=unique(Fleet_Details$Fleets), ordered = TRUE)

usethis::use_data(Fleet_Details, overwrite = TRUE)
# ---- Plots ----

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

# add Zeros
Select_Retain_2$Value[Select_Retain_2$Stock=='Red Snapper' & Select_Retain_2$Fleet=='Commercial Dive'] <- 0

Select_Retain_2$Value[Select_Retain_2$Stock!='Red Snapper' & Select_Retain_2$Age>=17] <- NA

Select_Retain_2$Stock <- factor(Select_Retain_2$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)
Select_Retain_2$Fleet[Select_Retain_2$Fleet=='cDV'] <- 'Commercial Dive'

Select_Retain_2$Variable <- factor(Select_Retain_2$Variable, levels=c('Select', 'Retention'), ordered = TRUE)
Select_Retain_2$Season <- factor(Select_Retain_2$Season, levels=c('On', 'Off'), ordered = TRUE)

ggplot(Select_Retain_2 %>% filter(Stock=='Red Snapper', Fleets !="Commercial Dive"),
       aes(x=Age, y=Value, linetype=Variable)) +
  facet_grid(Season~Fleets) +
  geom_line() +
  theme_bw() +
  labs(linetype='Legend', x='Age (year)', y='Probability')

ggsave('img/RS_Select_hist.png', width=8, height=4)


ggplot(Select_Retain_2 %>% filter(Stock!='Red Snapper'),
       aes(x=Age, y=Value, linetype=Variable)) +
  facet_grid(Season~Fleets) +
  geom_line() +
  theme_bw() +
  labs(linetype='Legend', x='Age (year)', y='Probability')

ggsave('img/GG_Select_hist.png', width=8, height=4)

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
  theme_bw()

ggsave('img/RS_GG_Removals_hist.png', width=6, height=3)


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

ggsave('img/RS_GG_Landings_hist.png', width=6, height=8)



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

ggsave('img/RS_GG_Discards_hist.png', width=6, height=8)

# Spawning Biomass
SSB <- get_SSB(MOM_multiHist)
SSB$Stock <- factor(SSB$Stock, c('Red Snapper', 'Gag Grouper'), ordered = TRUE)

ggplot(SSB %>% filter(Sim==1), aes(x=Year, y=Value/1000)) +
  facet_wrap(~Stock, scales = 'free_y') +
  geom_line() +
  labs(y='Spawning Stock (1,000)') +
  theme_bw()

ggsave('img/RS_GG_SSB_hist.png', width=6, height=3)





