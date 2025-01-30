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



#
OM <- readRDS('OM_Objects/BaseCase_RS.OM')
OM@cpars$`Red Snapper`$`Commercial Line`$Mat_age[1,,1]
OM@cpars$`Red Snapper`$`Commercial Line`$Fec_age[1,,1]
OM@cpars$`Red Snapper`$`Commercial Line`$R0
OM@cpars$`Red Snapper`$`Commercial Line`$hs
OM@cpars$`Red Snapper`$`Commercial Line`$Perr
OM@cpars$`Red Snapper`$`Commercial Line`$AC
OM@cpars$`Red Snapper`$`Commercial Line`$spawn_time_frac


OM@cpars$`Red Snapper`$`Commercial Line`$Perr_y[1,]
