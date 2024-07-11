library(SAMSE)
All_MPs <- Source_MPs()

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')

Hist_Files <- list.files('Hist_Objects')
Hist_Files <- Hist_Files[!grepl('BaseCase', Hist_Files)]


# re_do <- FALSE
#
# for (st in seq_along(Hist_Files)) {
#   txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
#   OM <- txt[1]
#   stock <- txt[2]
#
#   Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))
#
#   for (mm in seq_along(All_MPs)) {
#     nm <- names(All_MPs)[mm]
#     nm <- paste(OM, stock, nm, sep='_')
#     nm <- paste0(nm, '.mmse')
#     if (!re_do) {
#       if (file.exists(file.path('MSE_Objects', nm)))
#         next()
#     }
#     MPs <- All_MPs[[mm]]
#     MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)
#     saveRDS(MSE, file.path('MSE_Objects', nm))
#   }
# }
