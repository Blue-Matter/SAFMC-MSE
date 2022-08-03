# devtools::install_github("nikolaifish/bamExtras")
library(bamExtras)
library(MSEtool)

fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))


# Red snapper OM and MOM
rs <- standardize_rdat(rdat_RedSnapper)
OM <- BAM2OM(rs, nsim = 3)
MOM <- BAM2MOM(rs, nsim = 3)
MOM@cpars[[1]][[5]]$V %>% range()



multiHist <- SimulateMOM(MOM)


Hist <- Simulate(OM)

# Gag OM and MOM
gag <- standardize_rdat(rdat_GagGrouper)
OM <- BAM2OM(gag, nsim = 3)
MOM <- BAM2MOM(gag, nsim = 3)

fl <- tempfile()
fl
saveRDS(MOM, fl)
multiHist <- SimulateMOM(MOM)
