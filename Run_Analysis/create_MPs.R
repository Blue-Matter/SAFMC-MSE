
Rec_Reduction <- c(0, seq(0.05, to=0.95, length.out=5))

MPList <- list('SQ'=paste('SQ', Rec_Reduction, sep='_'),
            'SQ_FR'=paste('SQ_FR', Rec_Reduction, sep='_'),
            'SQ_MLL'=paste('SQ_MLL', Rec_Reduction, sep='_'),
            'SQ_NS'=paste('SQ_NS', Rec_Reduction, sep='_'),
            'SQ_OS'=paste('SQ_OS', Rec_Reduction, sep='_'),
            'SQ_FR_MLL'=paste('SQ_FR_MLL', Rec_Reduction, sep='_'),
            'SQ_FR_MLL_NS'=paste('SQ_FR_MLL_NS', Rec_Reduction, sep='_'),
            'SQ_FR_MLL_OS'=paste('SQ_FR_MLL_OS', Rec_Reduction, sep='_')
)


Write_MPs(MPList, Rec_Reduction)

