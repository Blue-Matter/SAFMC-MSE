#' Open the SEDAR Stock Assessment Report for a Stock
#'
#' Opens the SEDAR stock assessment report for a given stock in the default
#' web browser. Report URLs are stored in the internal `BAM_Specs` list.
#'
#' @param Stock A character string giving the stock name. Spaces are ignored.
#'   Available stocks can be viewed with `names(BAM_Specs)`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of opening a
#'   URL in the default browser.
#'
#' @examples
#' \dontrun{
#' # Open the SEDAR report for Red Snapper
#' SEDAR("RedSnapper")
#' SEDAR("Red Snapper")  # spaces are ignored
#' }
#'
#' @seealso [BAM_Specs] for the full list of available stocks and their
#'   metadata.
#'
#' @export
SEDAR <- function(Stock = NULL) {
  Stock <- gsub(' ', '', Stock)
  if (!Stock %in% names(BAM_Specs))
    cli::cli_abort(c('x'='{.val {Stock}} not found in `BAM_Specs',
                     'i'='Available stocks are: {.val {names(BAM_Specs)}}'))

  link <- BAM_Specs[[Stock]]$Link
  if (is.null(link)) {
    cli::cli_alert_warning("No `Link` field found for {.val {Stock}}")
  } else {
    utils::browseURL(link)
  }

  invisible(NULL)
}
