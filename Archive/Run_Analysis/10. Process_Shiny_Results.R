
MSEResults <- vector('list', 6)

# Reference Points ----
RefPoints <- readRDS('inst/ref_df.rda')
OMNames <- unique(RefPoints$OM)

ReferencePoints <- get_stock_status()
OMCodes <- ReferencePoints$OM |> unique()

for (i in seq_along(OMCodes)) {
  RefDF <- ReferencePoints |> dplyr::filter(OM==OMCodes[i])
  Ref_Points <- RefDF |> dplyr::distinct(Stock, MFMT, `Rebuild Target`=Rebuild, MSST)
  Ref_Points$Stock <- forcats::fct_recode(Ref_Points$Stock,
                                    `Red Snapper`='Red snapper',
                                    `Gag Grouper`='Gag grouper',
                                    `Black Sea Bass`='Black sea bass')

  MSEResults[[i]]$Ref_Points <- Ref_Points
}

# Historical ----
Files <- list.files('Hist_Objects')
for (i in seq_along(OMCodes)) {
  print(paste0(i, '/', length(OMCodes)))
  HistFiles <- Files[grepl(OMCodes[i], Files)]

  stockList <- list()
  for (j in seq_along(HistFiles)) {
    data <- readRDS(file.path('Hist_Objects', HistFiles[j]))
    biomass <- openMSE::get_SSB(data)
    landings <- openMSE::get_Landings(data)
    removals <- openMSE::get_Removals(data)
    discards <- removals
    discards$Value <- discards$Value - landings$Value
    discards$Variable <- 'Discards'
    fishingmortality <- openMSE::get_F(data)
    stockList[[j]] <- dplyr::bind_rows(biomass, landings, discards, fishingmortality)
  }
  df <- do.call('rbind', stockList) |> dplyr::filter(Sim==1)
  df$Stock <- factor(df$Stock, levels=c("Red Snapper", "Gag Grouper", "Black Sea Bass"), ordered = TRUE)

  MSEResults[[i]]$Historical <- df
}

# Projection ----
OMdat <- read.csv('inst/shiny_app/Data/OM_descriptions.csv')
OMNames <- OMdat$Name
OMCodes <- c("BaseCase", 'LowM', 'HighM', 'LowerRecEffort', 'EC', 'Rec1')
StockNames <- unique(MSEResults[[1]]$Historical$Stock) |> sort()
StockCodes <- c('RS', 'GG', 'BS')

## F
LandingsFiles <- list.files('Results_Objects/F_Landing_Discards', full.names = TRUE)
SSBFiles <- list.files('Results_Objects/SSB', full.names = TRUE)

for (i in seq_along(OMCodes)) {

  print(paste0(i, '/', length(OMCodes)))
  landfiles <- LandingsFiles[grepl(OMCodes[i], LandingsFiles)]
  ssbfiles <- SSBFiles[grepl(OMCodes[i], SSBFiles)]

  resultsList <- list()
  for (j in seq_along(landfiles)) {
    print(paste0(j, '/', length(landfiles)))
    landdata <- readRDS(landfiles[j])
    ssbdata <- readRDS(ssbfiles[j])

    landdata <- landdata |>
      # dplyr::filter(Rec_Reduction %in% c(0, 0.25, 0.75)) |>
      dplyr::select(Stock, Sim, Fleet, MP_Name, Year, F, Landings, Discards, Rec_Reduction) |>
      dplyr::mutate(`Apical Fishing Mortality`= F) |>
      tidyr::pivot_longer(c(`Apical Fishing Mortality`, 'Landings', 'Discards'),
                          names_to = 'Variable',
                          values_to='Value')

    landdata <- landdata |>
      dplyr::group_by(Year, Variable, Fleet, Rec_Reduction) |>
      dplyr::mutate(Upper=quantile(Value, 0.95),
                    Lower=quantile(Value, 0.05),
                    Value=median(Value)) |>
      dplyr::distinct(Stock, Year, Fleet, MP_Name, Rec_Reduction,
                    Variable, Value, Lower, Upper)

    ssbdata <- ssbdata |>
      # dplyr::filter(Rec_Reduction %in% c(0, 0.25, 0.75)) |>
      dplyr::select(Stock, Sim, MP_Name, Year, SSB, Rec_Reduction) |>
      dplyr::mutate(`Spawning Biomass`=SSB) |>
      tidyr::pivot_longer(c(`Spawning Biomass`),
                          names_to = 'Variable',
                          values_to='Value')


    ssbdata <- ssbdata |>
      dplyr::group_by(Year, Variable, Rec_Reduction) |>
      dplyr::mutate(Upper=quantile(Value, 0.95),
                    Lower=quantile(Value, 0.05),
                    Value=median(Value)) |>
      dplyr::distinct(Stock, Year, MP_Name, Rec_Reduction,
                      Variable, Value, Lower, Upper)

    df <- dplyr::bind_rows(landdata, ssbdata)

    df$Stock <- factor(df$Stock, levels=c("Red snapper", "Gag grouper", "Black sea bass"), ordered = TRUE)
    df$Stock <- forcats::fct_recode(df$Stock,
                                            `Red Snapper`='Red snapper',
                                            `Gag Grouper`='Gag grouper',
                                            `Black Sea Bass`='Black sea bass')

    resultsList[[j]] <- df
  }

  # add historical
  df <- do.call('rbind',resultsList)

  OMind <- i
  if (OMind>4)
    OMind <- 1
  df <- dplyr::bind_rows(MSEResults[[OMind]]$Historical, df)

  MSEResults[[i]]$Projection <- df
}


saveRDS(MSEResults, 'inst/shiny_app/Data/MSE_info.rda')

# Probability Tables ----


