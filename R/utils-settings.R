#' Get or Set SAMSE Package Settings
#'
#' Prints current package settings when called with no arguments, or updates
#' one or more settings when arguments are supplied. Settings persist for the
#' duration of the R session and can be set at startup via
#' [`.Rprofile`][Startup] (e.g. `options(SAMSE.nSim = 100)`).
#'
#' @param ... Named arguments corresponding to one or more settings to update.
#'   Unrecognised names trigger an error. Valid settings are:
#'   - `nSim` — number of stochastic simulations (default: `50`).
#'   - `pYear` — number of projection years (default: `30`).
#'
#' @return Invisibly returns a named list of all current settings after any
#'   updates are applied. When called with no arguments, the current settings
#'   are also printed to the console.
#'
#' @examples
#' # Print current settings
#' Settings()
#'
#' # Update one setting
#' Settings(nSim = 100)
#'
#' # Update multiple settings at once
#' Settings(nSim = 200, pYear = 50)
#'
#'
#' @export
Settings <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    current <- get_settings()
    cli::cli_h1("`SAMSE` Settings")
    for (nm in names(current))
      cli::cli_inform("{.field {nm}}: {.val {current[[nm]]}}")
    return(invisible(current))
  }


  valid <- c("nSim", "pYear")
  bad <- setdiff(names(dots), valid)
  if (length(bad))
    cli::cli_abort(c(
      "x" = "Unknown setting(s): {.val {bad}}",
      "i" = "Valid settings: {.val {valid}}"
    ))


  prefixed <- setNames(dots, paste0("SAMSE.", names(dots)))
  options(prefixed)
  invisible(get_settings())
}


get_settings <- function() {
  nms <- c("nSim", "pYear")
  vals <- lapply(paste0("SAMSE.", nms), getOption)
  setNames(vals, nms)
}

get_setting <- function(name) {
  getOption(paste0("SAMSE.", name))
}


