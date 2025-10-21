## ----SQ_MLL_NS_0----
SQ_MLL_NS_0<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0) <- "MMP"

## ----SQ_MLL_NS_0.05----
SQ_MLL_NS_0.05<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.05,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.05) <- "MMP"

## ----SQ_MLL_NS_0.15----
SQ_MLL_NS_0.15<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.15,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.15) <- "MMP"

## ----SQ_MLL_NS_0.25----
SQ_MLL_NS_0.25<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.25,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.25) <- "MMP"

## ----SQ_MLL_NS_0.35----
SQ_MLL_NS_0.35<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.35,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.35) <- "MMP"

## ----SQ_MLL_NS_0.45----
SQ_MLL_NS_0.45<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.45,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.45) <- "MMP"

## ----SQ_MLL_NS_0.55----
SQ_MLL_NS_0.55<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.55,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.55) <- "MMP"

## ----SQ_MLL_NS_0.65----
SQ_MLL_NS_0.65<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.65,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.65) <- "MMP"

## ----SQ_MLL_NS_0.75----
SQ_MLL_NS_0.75<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.75,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.75) <- "MMP"

## ----SQ_MLL_NS_0.85----
SQ_MLL_NS_0.85<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.85,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.85) <- "MMP"

## ----SQ_MLL_NS_0.95----
SQ_MLL_NS_0.95<- function(x, DataList, ...) {
  RecList <- SQ_MLL_NS(x, DataList, ...)
  RecList <-  Adjust_Effort(RecList, DataList, Effort_Mod=c(0,0,0.95,0))
  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)
}
class(SQ_MLL_NS_0.95) <- "MMP"

