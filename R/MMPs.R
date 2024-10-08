# #' Get stock specific size limit
# #'
# #' @param Name Name of the stock
# #'
# #' @return Numeric value. Minimum size limit (mm)
# #' @export
# get_MLL <- function(Name) {
#
#   grepl('Red Snapper', Name)
#
#
# }
#



#' Write MP functions
#'
#' @param MPList A character string with names of the MPs to create
#' @param Rec_Reduction A numeric vector with effort reductions for General
#' Recreational fleet
#' @param dir Directory to write the MPs to
#' @export
Write_MPs <- function(MPList, Rec_Reduction, dir='MP_functions') {
  if (!dir.exists(dir))
    dir.create(dir)

  for (i in seq_along(MPList)) {
    nm <- names(MPList)[i]
    if (file.exists(file.path(dir, paste0(nm,'.R'))))
      file.remove(file.path(dir, paste0(nm,'.R')))

    file.create(file.path(dir, paste0(nm,'.R')))

    mp_names <- paste(nm, Rec_Reduction, sep='_')

    for (j in seq_along(mp_names)) {
      mp <- strsplit(mp_names[j], '_')[[1]]
      Effort_Mod <- c(0,0,as.numeric(mp[length(mp)]),0)
      Effort_Mod <- paste0('Effort_Mod=c(', paste0(Effort_Mod, collapse=','), ')')
      mp <- mp[1:(length(mp)-1)]
      mp <- paste(mp, collapse='_')

      txt<- paste(paste0("## ----", mp_names[j], "----\n",
                         mp_names[j], '<- function(x, DataList, ...) {'),
                  paste0('  RecList <- ', mp, '(x, DataList, ...)'),
                  paste0('  Adjust_Effort(RecList, DataList, ', Effort_Mod, ')'),
                  '}',
                  paste0('class(', mp_names[j], ') <- "MMP"\n\n'),
                  sep='\n'
      )

      cat(txt, file=file.path(dir, paste0(nm,'.R')), append=TRUE)
    }
  }
}



#' @describeIn Write_MPs Source the MPs with a 2% effort creep
#' @return A list of MPs names
#' @export
Write_MPs_effort_creep <- function(MPList, Rec_Reduction, dir='MP_functions_effort_creep') {
  if (!dir.exists(dir))
    dir.create(dir)

  for (i in seq_along(MPList)) {
    nm <- names(MPList)[i]
    if (file.exists(file.path(dir, paste0(nm,'.R'))))
      file.remove(file.path(dir, paste0(nm,'.R')))

    file.create(file.path(dir, paste0(nm,'.R')))

    mp_names <- paste(nm, Rec_Reduction, sep='_')

    for (j in seq_along(mp_names)) {
      mp <- strsplit(mp_names[j], '_')[[1]]
      Effort_Mod <- c(0,0,as.numeric(mp[length(mp)]),0)
      Effort_Mod <- paste0('Effort_Mod=c(', paste0(Effort_Mod, collapse=','), ')')
      mp <- mp[1:(length(mp)-1)]
      mp <- paste(mp, collapse='_')

      txt<- paste(paste0("## ----", mp_names[j], "----\n",
                         mp_names[j], '<- function(x, DataList, ...) {'),
                  paste0('  RecList <- ', mp, '(x, DataList, ...)'),
                  paste0('  RecList <-  Adjust_Effort(RecList, DataList, ', Effort_Mod, ')'),
                  '  Effort_Creep(RecList, DataList, First_Management_Year=2025,  effort_creep=0.02)',
                  '}',
                  paste0('class(', mp_names[j], ') <- "MMP"\n\n'),
                  sep='\n'
      )

      cat(txt, file=file.path(dir, paste0(nm,'.R')), append=TRUE)
    }
  }
}


#' @describeIn Write_MPs Source the MPs
#' @return A list of MPs names
#' @export
Source_MPs <- function(dir='MP_functions') {
  mp_files <- list.files(dir)
  out_list <- list()
  for (fl in seq_along(mp_files)) {
    nm <- tools::file_path_sans_ext(mp_files[fl])

    out_list[nm] <- ''
    txt <- readLines(file.path(dir, mp_files[fl]))
    names <- txt[grepl('## ----', txt)]
    names <- gsub('----', '', names)
    names <- gsub('##', '', names)
    names <- trimws(names)
    out_list[[nm]] <- names

    source(file.path(dir, mp_files[fl]))
  }
  out_list
}

#' Management Procedures
#'
#' @name MPs
#'
NULL

#' @describeIn MPs Generate a list of `Rec` objects for an `MMP`
#' @export
Create_Rec_List <- function(DataList) {
  nstocks <- length(DataList)
  nfleets <- length(DataList[[1]])
  RecList <- vector('list', nstocks)
  for (s in 1:nstocks) {
    RecList[[s]] <- vector('list', nfleets)
    for (fl in 1:nfleets) {
      RecList[[s]][[fl]] <- new('Rec')
    }
  }
  RecList
}

#' @describeIn MPs Return information on current year from a `DataList`
#' @export
Get_Year_Info <- function(DataList) {
  Years <- DataList[[1]][[1]]@Year
  LHYear <- DataList[[1]][[1]]@LHYear
  pyear <- length(Years[Years>LHYear])
  hist_yrs <- Years[Years <=  LHYear]
  recent_yrs <- hist_yrs[(length(hist_yrs)-2):length(hist_yrs)]
  yr_ind <- which(hist_yrs %in% recent_yrs)
  list(Last_3_Yrs=yr_ind, Current_Year=max(Years)+1)
}



#' @describeIn MPs Adjust fishing effort for General Recreational Fleet
#' @param RecList A list of `Rec` objects returned by an `MMP`
#' @param Effort_Mod Effort modifier. Effort is adjusted as
#' `Effort_Rec_Fleet <- Effort_Rec_Fleet * (1-Effort_Mod)`
#' @export
Adjust_Effort <- function(RecList, DataList, Effort_Mod=0, First_Management_Year=2025, ...) {

  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(RecList)

  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      if (length(Effort_Mod)<nfleets)
        Effort_Mod <- rep(Effort_Mod, nfleets)
      RecList[[s]][[f]]@Effort <- RecList[[s]][[f]]@Effort * (1-Effort_Mod[f])
    }
  }
  RecList
}

#' @describeIn MPs Close areas to fishing
#' @export
Close_Areas <- function(RecList, DataList, areas=c(1,1,1,1,1,1), Allocate=1, First_Management_Year) {
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(RecList)
  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      RecList[[s]][[f]]@Spatial <- areas
      RecList[[s]][[f]]@Allocate <- 1
    }
  }
  RecList
}


#' @describeIn MPs 2% annual creep in fishing effort for Gen Rec fleet
#' @export
Effort_Creep <- function(RecList, DataList,First_Management_Year=2025,  effort_creep=0.02, ...) {
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(RecList)

  nyear <- (max(DataList[[1]][[1]]@Year)+1) -First_Management_Year
  this_effort <- RecList[[1]][[3]]@Effort
  RecList[[1]][[3]]@Effort <- this_effort * (1+effort_creep)^(nyear)
  RecList
}
#' Return the minimum legal length (MLL) for a given stock
#'
#' @param Name The common name of the stock
#'
#' @return numeric value. MLL in mm
#' @export
get_MLL <- function(Name) {
  name <- tolower(Name)
  if (grepl('red snapper', name)) {
    return(openMSE::inch2mm(24))
  } else if (grepl('gag grouper', name)) {
    return(openMSE::inch2mm(24))
  } else if (grepl('black sea bass', name)) {
    return(openMSE::inch2mm(12))
  } else {
    warning('No MLL specified for this stock ', Name)
  }
  0
}


add_MLL <- function(x, RecList, DataList, First_Management_Year=2025) {

  MLL <- get_MLL(Name=DataList[[1]][[1]]@Name)

  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(RecList)

  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {

      # calculate retention-at-length
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Len_age <- DataList[[s]][[f]]@Misc$StockPars$Len_age[x,,yr_ind]
      LenCV <- DataList[[s]][[f]]@Misc$StockPars$LenCV[x]
      Len_SD <- LenCV*Len_age

      CAL_binsmid <- seq(0, max(Len_age)+2*max(Len_SD), by=2)
      sel_at_length <- rep(1, length(CAL_binsmid))
      sel_at_length[CAL_binsmid<=MLL] <- 0

      ret_a <- MSEtool:::calcVatAge(matrix(Len_age),
                                    matrix(Len_SD),
                                    matrix(sel_at_length),
                                    length(Len_age),
                                    nyears=1,
                                    proyears=0,
                                    CAL_binsmid)
      ret_a <- ret_a[,1]
      ret_a[!is.finite(ret_a)] <- 0
      if (is.null(RecList[[s]][[f]]@Misc$R_age)) {
        RecList[[s]][[f]]@Misc$R_age <-  DataList[[s]][[f]]@Misc$FleetPars$retA_P[x,, yr_ind] * ret_a
        RecList[[s]][[f]]@Misc$Fdisc <- DataList[[s]][[f]]@Misc$FleetPars$Fdisc_array1[x,,yr_ind]
      } else {
        RecList[[s]][[f]]@Misc$R_age <-  RecList[[s]][[f]]@Misc$R_age * ret_a
        RecList[[s]][[f]]@Misc$Fdisc <- DataList[[s]][[f]]@Misc$FleetPars$Fdisc_array1[x,,yr_ind]
      }

      DataList[[s]][[f]]
    }
  }
  RecList
}


#' @describeIn MPs Status Quo. Fishing effort for each fleet is fixed in the projection
#' years at the geometric mean of the three last historical years
#' @param x Simulation number
#' @param DataList A nested list of `Data` objects
#' @param ... Additional arguments
#' @export
SQ <- function(x, DataList, ...) {
  RecList <- Create_Rec_List(DataList)
  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])

  year_info <- Get_Year_Info(DataList)
  yr_ind <- year_info$Last_3_Yrs

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- exp(mean(log(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind])))
      lastF <- DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,yr_ind[length(yr_ind)]]

      deltaE <- meanF/lastF
      if (!is.finite(deltaE)) deltaE <- 1E-5
      RecList[[s]][[f]]@Effort <- deltaE
    }
  }
  RecList
}
class(SQ) <- 'MMP'

#' @describeIn MPs Status Quo with offshore areas closed to fishing
#' @export
SQ_NS <- function(x, DataList, First_Management_Year=2025, areas=c(1,0,1,0,1,0), Allocate=1, ...) {
  RecList <- SQ(x, DataList)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate,
              First_Management_Year=First_Management_Year)
}
class(SQ_NS) <- 'MMP'

#' @describeIn MPs Status Quo with nearshore areas closed to fishing
#' @export
SQ_OS <- function(x, DataList, First_Management_Year=2025, areas=c(0,1,0,1,0,1), Allocate=1, ...) {
  RecList <- SQ(x, DataList)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate,
              First_Management_Year=First_Management_Year)
}
class(SQ_OS) <- 'MMP'


#' @describeIn MPs Status Quo with Full Retention. Modify retention curve with an asymptote of 1
#' @param First_Management_Year Year the management procedures are first implemented
#' @export
SQ_FR <- function(x, DataList, First_Management_Year=2025,...) {

  RecList <- SQ(x, DataList)
  year_info <- Get_Year_Info(DataList)
  if (year_info$Current_Year<First_Management_Year)
    return(RecList)

  nstocks <- length(RecList)
  nfleets <- length(RecList[[1]])
  yr_ind <- which.max(DataList[[1]][[1]]@Year)
  for (s in 1:nstocks) {
    nage <- DataList[[s]][[1]]@MaxAge+1

    for (f in 1:nfleets) {
      V_age <- DataList[[s]][[f]]@Misc$FleetPars$V[x,,yr_ind]
      Fdisc <- DataList[[s]][[f]]@Misc$FleetPars$Fdisc_array1[x,,yr_ind]

      r_age <- rep(1, length(V_age))

      RecList[[s]][[f]]@Misc$V_age <- V_age # R_age
      RecList[[s]][[f]]@Misc$R_age <- r_age # R_age
      RecList[[s]][[f]]@Misc$Fdisc <- Fdisc
    }
  }
  RecList
}
class(SQ_FR) <- 'MMP'



#' @describeIn MPs Status Quo with full retention and offshore closed
#' @export
SQ_FR_NS <- function(x, DataList,First_Management_Year=2025, areas=c(1,0,1,0,1,0), Allocate=1, ...) {
  RecList <- SQ_FR(x, DataList)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_FR_NS) <- 'MMP'

#' @describeIn MPs Status Quo with full retention and nearshore closed
#' @export
SQ_FR_OS <- function(x, DataList,First_Management_Year=2025, areas=c(0,1,0,1,0,1), Allocate=1, ...) {
  RecList <- SQ_FR(x, DataList)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_FR_OS) <- 'MMP'


#' @describeIn MPs Status Quo with a stock specific size limit.
#' @export
SQ_MLL <- function(x, DataList, First_Management_Year=2025, ...) {
  RecList <- SQ(x, DataList)
  add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
}
class(SQ_MLL) <- 'MMP'


#' @describeIn MPs Status Quo with a stock specific size limit and offshore areas closed to fishing
#' @export
SQ_MLL_NS <- function(x, DataList, First_Management_Year=2025, areas=c(1,0,1,0,1,0), Allocate=1, ...) {
  RecList <- SQ(x, DataList)
  RecList <- add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_MLL_NS) <- 'MMP'

#' @describeIn MPs Status Quo with a stock specific size limit and nearshore areas closed to fishing
#' @export
SQ_MLL_OS <- function(x, DataList, First_Management_Year=2025, areas=c(0,1,0,1,0,1), Allocate=1, ...) {
  RecList <- SQ(x, DataList)
  RecList <- add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_MLL_OS) <- 'MMP'

#' @describeIn MPs Status Quo with full retention and minimum size limit
#' @export
SQ_FR_MLL <- function(x, DataList, First_Management_Year=2025, ...) {
  RecList <- SQ_FR(x, DataList)
  add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
}
class(SQ_FR_MLL) <- 'MMP'

#' @describeIn MPs Status Quo with full retention, minimum size limit, and offshore closed
#' @export
SQ_FR_MLL_NS <- function(x, DataList,First_Management_Year=2025, areas=c(1,0,1,0,1,0), Allocate=1, ...) {
  RecList <- SQ_FR(x, DataList)
  RecList <- add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_FR_MLL_NS) <- 'MMP'

#' @describeIn MPs Status Quo with full retention, minimum size limit, and nearshore closed
#' @export
SQ_FR_MLL_OS <- function(x, DataList,First_Management_Year=2025, areas=c(0,1,0,1,0,1), Allocate=1, ...) {
  RecList <- SQ_FR(x, DataList)
  RecList <- add_MLL(x, RecList, DataList, First_Management_Year=First_Management_Year)
  Close_Areas(RecList, DataList, areas=areas, Allocate=Allocate, First_Management_Year=First_Management_Year)
}
class(SQ_FR_MLL_OS) <- 'MMP'








