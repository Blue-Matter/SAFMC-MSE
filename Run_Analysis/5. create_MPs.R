
Rec_Reduction <- c(0, seq(0.05, to=0.95, length.out=10))

MPList <- list('SQ'=paste('SQ', Rec_Reduction, sep='_'),
               'SQ_NS'=paste('SQ_NS', Rec_Reduction, sep='_'),
               'SQ_OS'=paste('SQ_OS', Rec_Reduction, sep='_'),
               'SQ_FR'=paste('SQ_FR', Rec_Reduction, sep='_'),
               'SQ_FR_NS'=paste('SQ_FR_NS', Rec_Reduction, sep='_'),
               'SQ_FR_OS'=paste('SQ_FR_OS', Rec_Reduction, sep='_'),
               'SQ_MLL'=paste('SQ_MLL', Rec_Reduction, sep='_'),
               'SQ_MLL_NS'=paste('SQ_MLL_NS', Rec_Reduction, sep='_'),
               'SQ_MLL_OS'=paste('SQ_MLL_OS', Rec_Reduction, sep='_'),
               'SQ_FR_MLL'=paste('SQ_FR_MLL', Rec_Reduction, sep='_'),
               'SQ_FR_MLL_NS'=paste('SQ_FR_MLL_NS', Rec_Reduction, sep='_'),
               'SQ_FR_MLL_OS'=paste('SQ_FR_MLL_OS', Rec_Reduction, sep='_')
)


Write_MPs(MPList, Rec_Reduction)


# ---- Effort Creep ----

MPList <- list('SQ'=paste('SQ_EC', Rec_Reduction, sep='_'),
               'SQ_NS'=paste('SQ_NS_EC', Rec_Reduction, sep='_'),
               'SQ_OS'=paste('SQ_OS_EC', Rec_Reduction, sep='_'),
               'SQ_FR'=paste('SQ_FR_EC', Rec_Reduction, sep='_'),
               'SQ_FR_NS'=paste('SQ_FR_NS_EC', Rec_Reduction, sep='_'),
               'SQ_FR_OS'=paste('SQ_FR_OS_EC', Rec_Reduction, sep='_'),
               'SQ_MLL'=paste('SQ_MLL_EC', Rec_Reduction, sep='_'),
               'SQ_MLL_NS'=paste('SQ_MLL_NS_EC', Rec_Reduction, sep='_'),
               'SQ_MLL_OS'=paste('SQ_MLL_OS_EC', Rec_Reduction, sep='_'),
               'SQ_FR_MLL'=paste('SQ_FR_MLL_EC', Rec_Reduction, sep='_'),
               'SQ_FR_MLL_NS'=paste('SQ_FR_MLL_NS_EC', Rec_Reduction, sep='_'),
               'SQ_FR_MLL_OS'=paste('SQ_FR_MLL_OS_EC', Rec_Reduction, sep='_')
)


Write_MPs_effort_creep(MPList, Rec_Reduction)
