# devtools::install_github("nikolaifish/bamExtras")
library(bamExtras)
library(MSEtool)

fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))


# Red snapper  MOM
RSMOM <- BAM2MOM(rdat=rdat_RedSnapper, nsim = 3)

lapply(RSMOM@Fleets[[1]], slot, 'Name') %>% unlist()

multiHist <- SimulateMOM(MOM)


# Gag MOM

GGMOM <- BAM2MOM(rdat=rdat_GagGrouper, nsim = 3)
lapply(GGMOM@Fleets[[1]], slot, 'Name') %>% unlist()


plot(RSMOM@cpars[[1]][[1]]$Len_age[1,,1],RSMOM@cpars[[1]][[1]]$V[1,,1], type='l')
lines(GGMOM@cpars[[1]][[1]]$Len_age[1,,1], GGMOM@cpars[[1]][[1]]$V[1,,1], col='blue')

fl <- tempfile()
fl
saveRDS(MOM, fl)
multiHist <- SimulateMOM(MOM)
