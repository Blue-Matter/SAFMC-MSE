
# ---- sourceMPs ----
library(SAMSE)
All_MPs <- Source_MPs()

Hist_Files <- list.files('Hist_Objects', pattern='BaseCase')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))
  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    if (grepl('MLL', nm) & stock !='RS')
      next()
    nm <- paste(OM, stock, nm, sep='_')
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE)
    saveRDS(MSE, file.path('MSE_Objects', paste0(nm, '.mmse')))
  }
}


MSE <- ProjectMOM(Hist, MPs=c('SQ_FR_MLL_OS_0', 'SQ_FR_MLL_0', 'SQ_FR_0', 'SQ_0'), silent=TRUE)


