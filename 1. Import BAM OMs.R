source('0. Specifications.R')

# ---- Black Sea Bass ----

# SEDAR 76
# 1978 - 2021
# https://sedarweb.org/documents/sedar-76-stock-assessment-report-south-atlantic-black-sea-bass/

# Issues:
# - Mismatch in MSY reference points. OM predicts FMSY ~ Inf vs BAM value of 0.31

DiscMortDF <- data.frame(Fleet=c('cHL', 'cPT', 'cPT', 'rHB', 'rGN'),
                         Value=c(0.19,   0.14, 0.068, 0.152, 0.137),
                         Year= c(1977, 1977, 2007, 1977, 1977))

OM <- ImportBAM(Stock='BlackSeaBass', nSim=nSim, pYear=pYear,
                    StockName='Black Sea Bass',
                    DiscMortDF=DiscMortDF)

CompareBAM(Stock='BlackSeaBass', OM)

Save(OM, 'Objects_OM/SingleStock/BlackSeaBass.om', overwrite = TRUE)

# ---- Gag Grouper ----

# SEDAR 71
# 1962 - 2019
# https://sedarweb.org/documents/sedar-71-stock-assessment-report-south-atlantic-gag/

OM <- ImportBAM(Stock='GagGrouper', nSim=nSim, pYear=pYear, StockName='Gag Grouper')
CompareBAM('GagGrouper', OM)
Save(OM, 'Objects_OM/SingleStock/GagGrouper.om', overwrite = TRUE)

# ---- Gray Triggerfish ----

# SEDAR 82
# 1982- 2021
# https://sedarweb.org/documents/sedar-82-south-atlantic-gray-triggerfish-final-stock-assessment-report/

OM <- ImportBAM(Stock='GrayTriggerfish',
                   nSim=nSim,
                   pYear=pYear,
                   StockName='Gray Triggerfish',
                   DiscMortDF=data.frame(Fleet=c('cHLs', 'rHBs', 'rGNs', 'rGNn'),
                                         Value=c(0.589),
                                         Year= c(1981)),
                   DiscFleets=c(rHBs="F.rHDs.D",
                                rGNs="F.rGDs.D",
                                rGNn="F.rGDn.D"),
                   DiscSelFleets=c(rHBs="sel.m.rHDs",
                                   rGNs="sel.m.rGDs",
                                   rGNn="sel.m.rGDs"),
                   RetSelFleets= c(cHLn="cHLs", rGNn='rGNs')
)
CompareBAM('GrayTriggerfish', OM)
Save(OM, 'Objects_OM/SingleStock/GrayTriggerfish.om', overwrite = TRUE)

# ---- Greater Amberjack ----

# SEDAR 59
# 1980 - 2017
# https://sedarweb.org/documents/sedar-59-stock-assessment-report-south-atlantic-greater-amberjack/

OM <- ImportBAM(Stock='GreaterAmberjack', nSim=nSim, pYear=pYear, StockName='Greater Amberjack')
CompareBAM('GreaterAmberjack', OM=OM)
Save(OM, 'Objects_OM/SingleStock/GreaterAmberjack.om', overwrite = TRUE)

# ---- Red Grouper -----

# SEDAR 53
# 1976 - 2015
# https://sedarweb.org/documents/sedar-53-stock-assessment-report-south-atlantic-red-grouper/
OM <- ImportBAM(Stock='RedGrouper', nSim=nSim, pYear=pYear, StockName='Red Grouper')
CompareBAM('RedGrouper', OM)
Save(OM, 'Objects_OM/SingleStock/RedGrouper.om', overwrite = TRUE)

# ---- Red Porgy ----
# SEDAR 60
# 1972 - 2017
# https://sedarweb.org/documents/sedar-60-stock-assessment-report-south-atlantic-red-porgy/

OM <- ImportBAM(Stock='RedPorgy', nSim=nSim, pYear=pYear)
CompareBAM('RedPorgy', OM)
Save(OM, 'Objects_OM/SingleStock/RedPorgy.om', overwrite = TRUE)

# ---- Red Snapper ----

# SEDAR 73
# 1950 - 2019
# https://sedarweb.org/documents/sedar-73-stock-assessment-report-south-atlantic-red-snapper/

OM <- ImportBAM('RedSnapper', nSim=nSim, pYear=pYear, StockName='Red Snapper')
CompareBAM('RedSnapper', OM)
Save(OM, 'Objects_OM/SingleStock/RedSnapper.om', overwrite = TRUE)

# ---- Red Snapper - Update ----
# SEDAR 73 - Update
# 1950 - 2023
# https://sedarweb.org/documents/sefsc-2024-update-to-sedar-73-south-atlantic-red-snapper-assessment/

RS_Dir <- "G:/Shared drives/BM shared/1. Projects/SAFMC Snapper - Grouper/BAM/RedSnapper_Update"

BAMOutput <- list(rdat=dget(file.path(RS_Dir, 's73u.rdat')),
                  dat=readLines(file.path(RS_Dir, 's73u.dat'))
)

OM <- ImportBAM(BAMOutput, nSim=nSim, pYear=pYear, StockName='Red Snapper')
CompareBAM(BAMOutput, OM)
Save(OM, 'Objects_OM/SingleStock/RedSnapper_Update.om', overwrite = TRUE)


# ---- Scamp Grouper / Yellowmouth ----

# SEDAR 68
# 1969 - 2021
# https://sedarweb.org/documents/sedar-68oa-south-atlantic-scamp-operational-assessment-final-stock-assessment-report/

OM <- ImportBAM(Stock='ScampGrouper', nSim=nSim, pYear=pYear, StockName = 'Scamp Grouper')
CompareBAM(Stock='ScampGrouper', OM)
Save(OM, 'Objects_OM/SingleStock/ScampGrouper.om', overwrite = TRUE)

# ---- Snowy Grouper ----

# SEDAR 36 - Update 2020
# 1974 - 2018

OM <- ImportBAM(Stock='SnowyGrouper', nSim=nSim, pYear=pYear, StockName='Snowy Grouper')
CompareBAM('SnowyGrouper', OM)
Save(OM, 'Objects_OM/SingleStock/SnowyGrouper.om', overwrite = TRUE)

# ---- Tilefish ----

# SEDAR 66
# 1972 - 2018
# https://sedarweb.org/documents/sedar-66-stock-assessment-report-south-atlantic-tilefish/

OM <- ImportBAM(Stock='Tilefish', nSim=nSim, pYear=pYear)
CompareBAM('Tilefish', OM)
Save(OM, 'Objects_OM/SingleStock/Tilefish.om', overwrite = TRUE)

# ---- Vermilion Snapper -----
# SEDAR 55
# 1946 - 2016
# https://sedarweb.org/documents/sedar-55-stock-assessment-report-south-atlantic-vermilion-snapper/

OM <- ImportBAM(Stock='VermilionSnapper',
                   nSim=nSim,
                   pYear=pYear,
                   StockName='Vermilion Snapper',
                   DiscSelFleets=c(rGN="sel.m.rHB.D"))

CompareBAM('VermilionSnapper', OM)
Save(OM, 'Objects_OM/SingleStock/VermilionSnapper.om', overwrite = TRUE)
