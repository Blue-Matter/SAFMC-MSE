

first_management_year <- 2025


# mean effort

# status quo
- geometric mean F from three most recent years
- allocation - mean from three most recent years
- across stocks?

- modify Selectivity for full retention - no seasons




# ---- Status Quo and Size Limits ----

Hist <- readRDS('Hist_Objects/BaseCase_RS.hist')

DataList <- list()

for (p in 1:length(Hist)) {
  DataList[[p]] <- list()
  for (f in 1:length(Hist[[1]])) {
    DataList[[p]][[f]] <-Hist[[p]][[f]]@Data
  }
}

x <- 1 # for stepping through MP code





MSE <- ProjectMOM(Hist, MPs='SQ')






Fs = MSE@F_FMSY[1,1,,1,] |> round(2)
Fs
round(f[,1]/sum(f[,1]), 2)

ref <- ref_df |> filter(Stock=='Red snapper', OM=='Base Case')

sb_sbref <- MSE@SSB[,1,1,]/ref$MSST

matplot(t(sb_sbref), type='l', ylim=c(0,3))
abline(h=1, lty=2)

