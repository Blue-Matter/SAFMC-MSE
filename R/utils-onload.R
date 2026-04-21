
.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    SAMSE.nSim     = 50,
    SAMSE.pYear    = 30
  )

  toset <- !names(defaults) %in% names(op)
  if (any(toset)) options(defaults[toset])

  invisible()
}
