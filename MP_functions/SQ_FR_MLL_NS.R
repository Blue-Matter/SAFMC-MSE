## ----SQ_FR_MLL_NS_0----
SQ_FR_MLL_NS_0<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0,0))
}
class(SQ_FR_MLL_NS_0) <- "MMP"

## ----SQ_FR_MLL_NS_0.05----
SQ_FR_MLL_NS_0.05<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0.05,0))
}
class(SQ_FR_MLL_NS_0.05) <- "MMP"

## ----SQ_FR_MLL_NS_0.275----
SQ_FR_MLL_NS_0.275<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0.275,0))
}
class(SQ_FR_MLL_NS_0.275) <- "MMP"

## ----SQ_FR_MLL_NS_0.5----
SQ_FR_MLL_NS_0.5<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0.5,0))
}
class(SQ_FR_MLL_NS_0.5) <- "MMP"

## ----SQ_FR_MLL_NS_0.725----
SQ_FR_MLL_NS_0.725<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0.725,0))
}
class(SQ_FR_MLL_NS_0.725) <- "MMP"

## ----SQ_FR_MLL_NS_0.95----
SQ_FR_MLL_NS_0.95<- function(x, DataList, ...) {
  RecList <- SQ_FR_MLL_NS(x, DataList, ...)
  Adjust_Effort(RecList, Effort_Mod=c(0,0,0.95,0))
}
class(SQ_FR_MLL_NS_0.95) <- "MMP"

