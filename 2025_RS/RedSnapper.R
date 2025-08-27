
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
Discards <- Removals
Discards$Variable <- 'Dead Discards'
Discards$Value <- Removals$Value - Landings$Value

LandingsDiscards <- dplyr::bind_rows(Removals, Landings, Discards) |>
  dplyr::filter(TimeStep==2019, Sim==1)

LandingsDiscards$Fleet <- dplyr::case_match(LandingsDiscards$Fleet,
                                            'cHL'~'Commercial Hook & Line',
                                            'rHB'~'Recreational Head Boat',
                                            'rGN'~'Recreational General')

LandingsDiscards$Variable <- factor(LandingsDiscards$Variable,
                                    levels=c('Removals', 'Landings', 'Dead Discards'),
                                    ordered = TRUE)


ggplot(LandingsDiscards |> dplyr::filter(Variable!='Removals'),
       aes(x=Age, y=Value/1000, fill=Variable)) +
  facet_grid(~Fleet, scales='free') +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb')

ggsave(file.path(img.dir, 'RS_Landings_Discards.png'), width=12, height=3)

ggplot(LandingsDiscards |> dplyr::filter(Variable!='Removals', Fleet=='Recreational General'),
       aes(x=Age, y=Value/1000, fill=Variable)) +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(y='1000 lb')

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

CE <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}

CE_FR <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice@Retention@Pars <- list(RL50=0, RL50_95=0.1)
  advice
}

CE_nDR <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice@DiscardMortality@MeanAtAge <- 0
  advice
}


MPs <- c('CE', 'CE_FR', 'CE_nDR')
MSE <- Project(Hist, MPs=MPs)

b <- Biomass(MSE) |>
  dplyr::group_by(TimeStep, MP) |>
  dplyr::summarise(Value=mean(Value))

ggplot(b, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line() +
  theme_bw()







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


