# Performance Metrics
#
# MMSEobj <- MMSE
#
# P_MSST <- function (MMSEobj = NULL, Ref = 1, Yrs = c(4,13))  {
#   if(!inherits(MMSEobj,'MMSE'))
#     stop('This PM method is designed for objects of class `MMSE`')
#   Yrs <- ChkYrs(Yrs, MMSEobj)
#   PMobj <- new("PMobj")
#   PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-10 (2024-2033)"
#   PMobj@Caption <- "Prob. Green Zone of Kobe Space (2024-2033)"
#
#   PMobj@Ref <- Ref
#   tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
#   if (is.null(dim(tt)))
#     tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
#   PMobj@Stat <- tt
#   PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
#   PMobj@Mean <- calcMean(PMobj@Prob)
#   PMobj@MPs <- MMSEobj@MPs[[1]]
#   PMobj
#
# }
# class(PGK_short) <- 'PM'
