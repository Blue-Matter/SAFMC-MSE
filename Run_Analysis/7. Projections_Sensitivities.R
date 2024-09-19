library(SAMSE)
All_MPs <- Source_MPs()

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')

re_do <- TRUE

# --- LowerRecEffort  ----

Hist_Files <- list.files('Hist_Objects', pattern='LowerRecEffort')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))

  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    nm <- paste(OM, stock, nm, sep='_')
    nm <- paste0(nm, '.mmse')

    # ######
    # if (grepl('FR', nm)) {
    #   re_do <- TRUE
    # } else {
    #   re_do <- FALSE
    # }
    #
    # ######

    if (!re_do) {
      if (file.exists(file.path('MSE_Objects', nm)))
        next()
    }
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)
    saveRDS(MSE, file.path('MSE_Objects', nm))
  }
}

# --- LowerM  ----

Hist_Files <- list.files('Hist_Objects', pattern='LowM')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))

  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    nm <- paste(OM, stock, nm, sep='_')
    nm <- paste0(nm, '.mmse')

    ######
    if (grepl('FR', nm)) {
      redo <- TRUE
    } else {
      redo <- FALSE
    }

    ######

    if (!re_do) {
      if (file.exists(file.path('MSE_Objects', nm)))
        next()
    }
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)
    saveRDS(MSE, file.path('MSE_Objects', nm))
  }
}


# --- HighM  ----

Hist_Files <- list.files('Hist_Objects', pattern='HighM')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))

  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    nm <- paste(OM, stock, nm, sep='_')
    nm <- paste0(nm, '.mmse')

    ######
    if (grepl('FR', nm)) {
      redo <- TRUE
    } else {
      redo <- FALSE
    }

    ######
    if (!re_do) {
      if (file.exists(file.path('MSE_Objects', nm)))
        next()
    }
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)
    saveRDS(MSE, file.path('MSE_Objects', nm))
  }
}

# ---- EffortCreep ----

# 2% per year increase in effort and or efficiency

All_MPs <- Source_MPs(dir='MP_functions_effort_creep')
Hist_Files <- list.files('Hist_Objects', pattern='BaseCase')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))

  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    nm <- paste(OM, stock, nm, 'EC', sep='_')
    nm <- paste0(nm, '.mmse')

    ######
    if (grepl('FR', nm)) {
      redo <- TRUE
    } else {
      redo <- FALSE
    }

    ######

    if (!re_do) {
      if (file.exists(file.path('MSE_Objects', nm)))
        next()
    }
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)

    saveRDS(MSE, file.path('MSE_Objects', nm))
  }
}



# ---- Recruitment 1 ----

# Make OMs
OMList <- list(readRDS('OM_Objects/BaseCase_RS.OM'),
               readRDS('OM_Objects/BaseCase_GG.OM'),
               readRDS('OM_Objects/BaseCase_BS.OM'))

rec_dev_BC <- Plot_Correlated_Rec_Devs(OMList,
                                       addtheme=theme(axis.title.x = element_blank()),
                                       ylim=c(-2,2))

OMList_Rec1 <- Generate_Correlated_Rec_Devs(OMList, truncsd=2, yrind=10)

rec_dev_R1 <- Plot_Correlated_Rec_Devs(OMList_Rec1, ylim=c(-2,2))


fn <- function(x) mean(x)
# plot(apply(OMList[[1]]@cpars[[1]][[1]]$Perr_y, 2, fn),type='l',ylim=c(0,4))
# lines(apply(OMList_Rec1[[1]]@cpars[[1]][[1]]$Perr_y, 2, fn), col='blue')
#
#
# plot(apply(OMList[[2]]@cpars[[1]][[1]]$Perr_y, 2, fn),type='l',ylim=c(0,4))
# lines(apply(OMList_Rec1[[2]]@cpars[[1]][[1]]$Perr_y, 2, fn), col='blue')
#
# matplot(t(OMList[[2]]@cpars[[1]][[1]]$Perr_y), type='l', ylim=c(0,2))
# matplot(t(OMList_Rec1[[2]]@cpars[[1]][[1]]$Perr_y), type='l', ylim=c(0,2))
#
# plot(apply(OMList[[3]]@cpars[[1]][[1]]$Perr_y, 2, fn),type='l' ,ylim=c(0,4))
# lines(apply(OMList_Rec1[[3]]@cpars[[1]][[1]]$Perr_y, 2, fn), col='blue')
#
#
#
#
#
#
#
#
# plot(OMList[[2]]@cpars[[1]][[1]]$Perr_y[sim,], type='l', ylim=c(0,4))
# lines(OMList_Rec1[[2]]@cpars[[1]][[1]]$Perr_y[sim,], col='blue')

RecDevplot <- cowplot::plot_grid(rec_dev_BC,
                                 rec_dev_R1, nrow=2,
                                 labels=c('a)', 'b)'))

RecDevplot


ggsave('img/Rec_Devs.png', RecDevplot, width=9, height=5.5)

OM_RS <- OMList_Rec1[[1]]
OM_GG <- OMList_Rec1[[2]]
OM_BS <- OMList_Rec1[[3]]

saveRDS(OM_RS, 'OM_Objects/Rec1_RS.OM')
saveRDS(OM_GG, 'OM_Objects/Rec1_GG.OM')
saveRDS(OM_BS, 'OM_Objects/Rec1_BS.OM')



# Simulate Historical
fls <- list.files('OM_Objects', pattern='Rec1')

for (i in seq_along(fls)) {
  fl <- fls[i]
  OM <- readRDS(file.path('OM_Objects', fl))
  Hist <- Simulate(OM)
  hist_fl <- paste0(strsplit(fl, 'OM')[[1]][1], 'hist')
  saveRDS(Hist, file.path('Hist_Objects', hist_fl))
}


# Project

All_MPs <- Source_MPs()

Hist_Files <- list.files('Hist_Objects', pattern='Rec1')

for (st in seq_along(Hist_Files)) {
  txt <- strsplit(tools::file_path_sans_ext(Hist_Files[st]), '_')[[1]]
  OM <- txt[1]
  stock <- txt[2]

  Hist <- readRDS(file.path('Hist_Objects', Hist_Files[st]))

  for (mm in seq_along(All_MPs)) {
    nm <- names(All_MPs)[mm]
    nm <- paste(OM, stock, nm, 'REC1', sep='_')
    nm <- paste0(nm, '.mmse')

    if (!re_do) {
      if (file.exists(file.path('MSE_Objects', nm)))
        next()
    }
    MPs <- All_MPs[[mm]]
    MSE <- ProjectMOM(Hist, MPs=All_MPs[[mm]], silent=TRUE, extended=TRUE)
    saveRDS(MSE, file.path('MSE_Objects', nm))
  }
}


