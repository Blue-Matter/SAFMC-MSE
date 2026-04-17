#' Check Required Package Dependencies
#'
#' Validates that required packages meet minimum version requirements. Throws
#' an informative error for each package that is missing or out of date,
#' including the install command needed to resolve the issue.
#'
#' @return Invisibly returns `NULL` if all checks pass.
#'
#' @details
#' The following packages and minimum versions are checked:
#'
#' | Package | Minimum Version | Source |
#' |---------|----------------|--------|
#' | `MSEtool` | 4.0.0 | GitHub: `blue-matter/MSEtool\@prelease` |
#' | `bamExtras` | 0.0.2 | GitHub: `Nikolai-Klibansky/bamExtras` |
#'
#' @examples
#' \dontrun{
#' CheckPackages()
#' }
#'
#' @export
CheckPackages <- function() {

  check_pkg_version <- function(pkg, min_version, install_cmd) {
    installed <- tryCatch(packageVersion(pkg), error = function(e) NULL)

    if (is.null(installed)) {
      cli::cli_abort(c(
        "Package {.pkg {pkg}} is not installed.",
        "i" = "Minimum required version: {.val {min_version}}",
        "*" = "Install with: {.code {install_cmd}}"
      ))
    }

    if (!installed >= package_version(min_version)) {
      cli::cli_abort(c(
        "Package {.pkg {pkg}} is out of date.",
        "x" = "Installed version: {.val {as.character(installed)}}",
        "i" = "Minimum required version: {.val {min_version}}",
        "*" = "Update with: {.code {install_cmd}}"
      ))
    }
  }

  check_pkg_version(
    pkg         = "MSEtool",
    min_version = "4.0.0",
    install_cmd = "pak::pkg_install('blue-matter/MSEtool@prelease')"
  )

  check_pkg_version(
    pkg         = "bamExtras",
    min_version = "0.0.2",
    install_cmd = "pak::pkg_install('Nikolai-Klibansky/bamExtras')"
  )

  invisible(NULL)
}
