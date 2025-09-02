
if (packageVersion('MSEtool') < '4.0.0')
  stop('This code requires the latest development version of `MSEtool`')

library(MSEtool)
library(ggplot2)

img.dir <- file.path('img', '2025_08_28_TT')

# Create OMs
OM_Base <- ImportBAM(pYear=30)
Hist <- Simulate(OM_Base)

Removals <- Removals(Hist, ByFleet=TRUE, ByAge=TRUE)
Landings <- Landings(Hist, ByFleet=TRUE, ByAge=TRUE)
DiscardsDead <- Discards(Hist, ByAge=TRUE, ByFleet=TRUE, type='Dead')
DiscardsAlive <- Discards(Hist, ByAge=TRUE, ByFleet=TRUE, type='Alive')

LandingsDiscards <- dplyr::bind_rows(Removals, Landings, DiscardsDead, DiscardsAlive) |>
  dplyr::filter(TimeStep==2019, Sim==1)

LandingsDiscards$Fleet <- dplyr::case_match(LandingsDiscards$Fleet,
                                            'cHL'~'Commercial Hook & Line',
                                            'rHB'~'Recreational Head Boat',
                                            'rGN'~'Recreational General')

LandingsDiscards$Variable <- factor(LandingsDiscards$Variable,
                                    levels=c('Removals', 'Landings', 'Discards (dead)', 'Discards (alive)'),
                                    ordered = TRUE)


ggplot(LandingsDiscards |> dplyr::filter(Variable!='Removals'),
       aes(x=Age, y=Value/1000, fill=Variable)) +
  facet_grid(~Fleet, scales='free') +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb') +
  scale_fill_brewer(type='qual', palette='Dark2')

ggsave(file.path(img.dir, 'RS_Landings_Discards.png'), width=12, height=3)

ggplot(LandingsDiscards |> dplyr::filter(Variable!='Removals', Fleet=='Recreational General'),
       aes(x=Age, y=Value/1000, fill=Variable)) +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb') +
  scale_fill_brewer(type='qual', palette='Dark2')

ggsave(file.path(img.dir, 'RS_Landings_Discards_Rec_Gen.png'), width=8, height=6)


LandingsDiscards |> dplyr::filter(Variable!='Removals', Fleet=='Recreational General') |>
  dplyr::group_by(Variable) |>
  dplyr::summarise(Total=sum(Value)) |>
  dplyr::ungroup() |>
  dplyr::reframe(Total=Total/sum(Total))


DF <- LandingsDiscards |> dplyr::filter(Variable!='Removals', Fleet=='Recreational General') |>
  dplyr::group_by(Age) |>
  dplyr::mutate(Total=sum(Value)) |>
  dplyr::group_by(Age, Variable) |>
  dplyr::mutate(Proportion=Value/Total)

ggplot(DF, aes(x=Age, y=Proportion, color=Variable)) +
  geom_line(linewidth=1.2) +
  theme_bw() +
  labs(y="Proportion")

ggsave(file.path(img.dir, 'RS_Landings_Discards_Rec_Gen_Proportion.png'), width=8, height=6)


DF |> dplyr::filter(Variable!='Landings') |> print(n=21)


Selectivity <- GetSelectivityAtAge(Hist)
Retention <- GetRetentionAtAge(Hist)

SelectRetention <- dplyr::bind_rows(Selectivity, Retention) |>
  dplyr::filter(Sim==1, TimeStep==2019)

SelectRetention$Fleet <- dplyr::case_match(SelectRetention$Fleet,
                              'cHL'~'Commercial Hook & Line',
                              'rHB'~'Recreational Head Boat',
                              'rGN'~'Recreational General')

ggplot(SelectRetention, aes(x=Age, y=Value, color=Variable)) +
  facet_grid(~Fleet,) +
  geom_line() +
  theme_bw() +
  labs(y='Probability')

ggplot(SelectRetention |> dplyr::filter(Fleet=='Recreational General'),
       aes(x=Age, y=Value, color=Variable)) +
  geom_line(linewidth=1.2) +
  theme_bw() +
  labs(y='Probability')

ggsave(file.path(img.dir, 'RS_Select_Retain.png'), width=8, height=6)


# ---- MSE Runs with Different Assumptions ----

CurrentEffort <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}

FullRetention <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice@Retention@Pars <- list(RL50=0, RL50_95=0.1)
  advice
}

NoDiscardMortality <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice@DiscardMortality@MeanAtAge <- 0
  advice
}

FullRetention_0.25Effort <- function(Data) {
  advice <- Advice()
  advice@Effort <- 0.25
  advice@Retention@Pars <- list(RL50=0, RL50_95=0.1)
  advice
}


MPs <- c('CurrentEffort',
         'FullRetention',
         'NoDiscardMortality')

MSE <- Project(Hist, MPs=MPs)


b <- Biomass(MSE) |>
  dplyr::group_by(TimeStep, MP, Variable) |>
  dplyr::summarise(Value=mean(Value)/1000)

labDF1 <- b |> dplyr::filter(TimeStep==1975)
labDF2 <- b |> dplyr::filter(TimeStep==2048)

labDF <- dplyr::bind_rows(labDF1, labDF2)

p1 <- ggplot(b, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line() +
  labs(y='Biomass (1000 lb)') +
  theme_bw() +
  ggrepel::geom_text_repel(data=labDF, aes(label=MP, y=Value)) +
  guides(color='none')

ggsave(file.path(img.dir, 'RS_Biomass.png'),
       width=6, height=4)


r <- Removals(MSE) |>
  dplyr::group_by(TimeStep, MP, Variable) |>
  dplyr::summarise(Value=mean(Value)/1000)

labDF1 <- r |> dplyr::filter(TimeStep==1975)
labDF2 <- r |> dplyr::filter(TimeStep==2048)
labDF <- dplyr::bind_rows(labDF1, labDF2)

p2 <- ggplot(r, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line() +
  theme_bw() +
  ggrepel::geom_text_repel(data=labDF, aes(label=MP, y=Value)) +
  guides(color='none') +
  labs(y='Removals (1000 lb)')

ggsave(file.path(img.dir, 'RS_Removals.png'),
       width=6, height=4)

l <- Landings(MSE) |>
  dplyr::group_by(TimeStep, MP, Variable) |>
  dplyr::summarise(Value=mean(Value)/1000)

labDF1 <- l |> dplyr::filter(TimeStep==1975)
labDF2 <- l |> dplyr::filter(TimeStep==2048)
labDF <- dplyr::bind_rows(labDF1, labDF2)

p3 <- ggplot(l, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line() +
  labs(y='Landings (1000 lb)') +
  theme_bw() +
  ggrepel::geom_text_repel(data=labDF, aes(label=MP, y=Value)) +
  guides(color='none') +
  labs(y='Landings (1000 lb)')

ggsave(file.path(img.dir, 'RS_Landing.png'),
       width=6, height=4)

library(patchwork)

pout <- p1 / p3

ggsave(file.path(img.dir, 'RS_Projections.png'),
       plot=pout,
       width=10, height=6)

DF <- dplyr::bind_rows(b,r,l)
write.csv(DF, '2025_RS/plotData.csv')


# ----- New Assessment Run with Zero Historical Discard Mortality -----


# Run Red Snapper Assessment assuming no discard mortality
devtools::install_github("Nikolai-Klibansky/bamExtras", build_vignettes = TRUE, force=TRUE)
vignette("intro","bamExtras")

library(bamExtras)

bam_RS <- bam2r("RedSnapper")
bam_RS$init$set_Dmort_cHL1 <- "0"
bam_RS$init$set_Dmort_cHL2 <- "0"
bam_RS$init$set_Dmort_cHL3 <- "0"

bam_RS$init$set_Dmort_rHB1 <- "0"
bam_RS$init$set_Dmort_rHB2 <- "0"
bam_RS$init$set_Dmort_rHB3 <- "0"

bam_RS$init$set_Dmort_rGN1 <- "0"
bam_RS$init$set_Dmort_rGN2 <- "0"
bam_RS$init$set_Dmort_rGN3 <- "0"


rdat_RS <- run_bam(bam=bam_RS, fileName="RedSnapper", run_cleanup = FALSE)





# No discard mortality OM
bamExtras::cxx_RedSnapper



rdat_AtMe <- run_bam("AtlanticMenhaden")$rdat


