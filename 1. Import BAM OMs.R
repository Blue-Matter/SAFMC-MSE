
# TODO:
# - reference points

# Issues:
# - Black Sea Bass - Mismatch in MSY reference points. OM predicts FMSY ~ Inf vs BAM value of 0.31

library(SAMSE)


Import_Stocks <- names(BAM_Info_List)
Import_Stocks # BAM assessments to import

Import_OMs('RedSnapper', plot=TRUE) # demo



# Loop over Import_Stocks, import OMs, and save to disk
Import_OMs(Import_Stocks)


# ---- Red Snapper - Update ----
# SEDAR 73 - Update
# 1950 - 2023
# https://sedarweb.org/documents/sefsc-2024-update-to-sedar-73-south-atlantic-red-snapper-assessment/


# RS_Dir <- "G:/Shared drives/BM shared/1. Projects/SAFMC Snapper - Grouper/BAM/RedSnapper_Update"
#
# BAMOutput <- list(rdat=dget(file.path(RS_Dir, 's73u.rdat')),
#                   dat=readLines(file.path(RS_Dir, 's73u.dat'))
# )
#
# OM <- ImportBAM(BAMOutput, nSim=nSim, pYear=pYear, StockName='Red Snapper')
# CompareBAM(BAMOutput, OM)
# Save(OM, 'Objects/OM/SingleStock/RedSnapper_Update.om', overwrite = TRUE)


BAM_Info_List$GrayTriggerfish
