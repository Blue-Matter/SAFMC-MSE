#' Load Project Settings
#'
#' Sources the project specifications file (`0. Specifications.R`) from the
#' current working directory, making all settings and parameters defined within
#' it available in the global environment.
#'
#' @return Nothing. Called for side effects.
#'
#' The file `0. Specifications.R` must exist in the current working directory
#' (`getwd()`). This file is expected to define project-wide settings and
#' parameters.
#'
#' The working directory should be the root directory of the `SAMSE` package,
#' available for download from [GitHub](https://github.com/Blue-Matter/SAFMC-MSE).
#'
#'
#' @examples
#' \dontrun{
#' setwd("path/to/project")
#' LoadSettings()
#' }
#'
#' @export
LoadSettings <- function() {
  fls <- list.files()
  specs <- '0. Specifications.R'
  if (!specs %in% fls)
    cli::cli_abort(c('x'='Cannot find file {.val {specs}} in working directory {.val {getwd()}}'))

  source(specs)

  invisible(NULL)
}

