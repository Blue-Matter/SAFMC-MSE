source('0. Specifications.R')

OM_Dir <- 'OM_Objects/Base'

# ---- Import Stock OMs from BAM -----

## ---- Red Snapper SEDAR 73 -----
# https://sedarweb.org/assessments/sedar-73/

OM_RS <- ImportBAM(nSim=nSim, pYear=pYear)
Save(OM_RS, file.path(OM_Dir, 'RS_SEDAR_73.om'))


## ---- Red Snapper SEDAR 73 - Update -----
# https://sedarweb.org/documents/sefsc-2024-update-to-sedar-73-south-atlantic-red-snapper-assessment/

BAMDir <- "G:/Shared drives/BM shared/1. Projects/SAFMC Snapper - Grouper/BAM/RedSnapper"

BAMOutput <- dget(file.path(BAMDir, 's73u.rdat'))
OM_RS_Update <- ImportBAM(BAMOutput, nSim=nSim, pYear=pYear)
Save(OM_RS_Update, file.path(OM_Dir, 'RS_SEDAR_73Update.om'))


