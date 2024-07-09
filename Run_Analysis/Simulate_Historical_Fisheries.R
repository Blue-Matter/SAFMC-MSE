library(SAMSE)

OMdir <- 'OM_Objects'
Histdir <- 'Hist_Objects'

if (!dir.exists(Histdir))
  dir.create(Histdir)

fls <- list.files(OMdir)

run_hist <- TRUE

if (run_hist) {
  for (i in seq_along(fls)) {
    fl <- fls[i]
    OM <- readRDS(file.path(OMdir, fl))
    Hist <- Simulate(OM)
    hist_fl <- paste0(strsplit(fl, 'OM')[[1]][1], 'hist')
    saveRDS(Hist, file.path(Histdir, hist_fl))
  }
}



