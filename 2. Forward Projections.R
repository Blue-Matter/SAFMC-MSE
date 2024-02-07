library(SAMSE)
MPs <- c('StatusQuo','StatusQuo_MLL', 'SQRecEffort20', 'Ftarget')

incRecEMPs <- paste0(MPs, '_IncRecEff')

# ---- Loop through OMs and Run Projections ----

OM_hists <- list.files('Hist_Objects', pattern='.hist')

for (i in seq_along(OM_hists)) {
  multiHist <- readRDS(file.path('Hist_Objects', OM_hists[i]))

  if (i == 6) {
    MSE <- ProjectMOM(multiHist, MPs=incRecEMPs,
                      dropHist = FALSE)
  } else {
    MSE <- ProjectMOM(multiHist, MPs=MPs,
                      dropHist = FALSE)
  }


  rm(multiHist)

  fl <- paste0(tools::file_path_sans_ext(OM_hists[i]), '.mmse')
  saveRDS(MSE, file.path('MSE_Objects', fl))
  rm(MSE)

}


# ---- Calculate Summary Statistics & Performance Metrics ----
OMs <- paste0("OM_", unlist(strsplit(OM_hists, '.hist')))

for (i in seq_along(OMs)) {
  fl <- paste0(unlist(strsplit(OM_hists[i], '.hist')), '.mmse')
  MSE <- readRDS(file.path('MSE_Objects', fl))


}




# Make list of MSE results to load into Shiny

# Historical OM
## SSB
## Catch
## Fishing Mortality

# Projections
## SSB
## Catch
## Fishing Mortality

# Performance Metrics



F_DF <- get_F(MSE)


plot_Fmort(MSE)
plot_Catch(MSE)
plot_SB(MSE)

Landings_10(MSE)
Landings_20(MSE)

avail('PM', 'SAMSE')






