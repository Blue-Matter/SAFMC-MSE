
library(SAMSE)

# Base Case - OM_01 ----
multiHist <- SimulateMOM(OM_01)
saveRDS(multiHist, 'Hist_Objects/01.hist')


# - OM_02 ----
multiHist <- SimulateMOM(OM_02)
saveRDS(multiHist, 'Hist_Objects/02.hist')


# - OM_03 ----

multiHist <- SimulateMOM(OM_03)
saveRDS(multiHist, 'Hist_Objects/03.hist')

# - OM_04 ----

multiHist <- SimulateMOM(OM_04)
saveRDS(multiHist, 'Hist_Objects/04.hist')

# - OM_05 ----

multiHist <- SimulateMOM(OM_05)
saveRDS(multiHist, 'Hist_Objects/05.hist')

# - OM_06 ----

multiHist <- SimulateMOM(OM_06)
saveRDS(multiHist, 'Hist_Objects/06.hist')

# - OM_07 ----

multiHist <- SimulateMOM(OM_07)
saveRDS(multiHist, 'Hist_Objects/07.hist')

# - OM_08 ----

multiHist <- SimulateMOM(OM_08)
saveRDS(multiHist, 'Hist_Objects/08.hist')
