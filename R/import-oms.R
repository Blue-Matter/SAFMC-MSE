#' Import BAM Operating Models
#'
#' Imports one or more stock Operating Models (OMs) from BAM (Beaufort
#' Assessment Model) output via [MSEtool::ImportBAM()], optionally validates
#' them against BAM population dynamics with [MSEtool::CompareBAM()], and saves
#' each OM to disk as a `.om` file.
#'
#' @param Import_Stocks `character`. Names of stocks to import. If `NULL`
#'   (default), all stocks defined in `BAM_Specs`
#' @param Compare `logical`. If `TRUE` (default), runs
#'   [MSEtool::CompareBAM()] on each imported OM to validate against BAM
#'   reference points.
#' @param plot `logical`. If `TRUE`, diagnostic plots are produced during
#'   [MSEtool::CompareBAM()]. Passed directly to that function. Defaults to
#'   `FALSE`.
#' @param save `logical`. Save the OM to disk? Default TRUE
#' @param OMpath `character`. Directory where `.om` files are saved.
#'   Defaults to `"Objects/OM/SingleStock"`.
#'
#' @return Called for its side effects. Each imported OM is saved to `OMpath`
#'   as `<StockName>.om`. Returns list of imported OMs invisibly.
#'
#' @details
#' Project-wide settings `nSim`, `pYear`) are loaded at the
#' start of the function via [Settings()]. Each stock's BAM configuration
#' is drawn from [BAM_Specs].
#'
#' @seealso [Settings()], [MSEtool::ImportBAM()], [MSEtool::CompareBAM()]
#'
#' @export
Import_OMs <- function(Import_Stocks=NULL,
                       Compare=TRUE,
                       plot=FALSE,
                       save= TRUE,
                       OMpath="Objects/OM/SingleStock") {


  if (is.null(Import_Stocks)) {
    Import_Stocks <- names(BAM_Specs)
  } else {
    missing <- Import_Stocks[!Import_Stocks %in% names(BAM_Specs)]
    if (length(missing))
      cli::cli_abort(c('x'='{.val {missing}} not found in `BAM_Specs',
                       'i'= 'Available stocks are: {.val {names(BAM_Specs)}}'))
  }

  OM_List <- list()
  for (i in seq_along(Import_Stocks)) {

    BAM_Stock <- Import_Stocks[i]
    ind <- match(BAM_Stock, names(BAM_Specs))
    BAM_Info <- BAM_Specs[[ind]]

    cli::cli_text('')

    OM <- MSEtool::ImportBAM(Stock = BAM_Stock,
                             nSim = get_setting('nSim'),
                             pYear = get_setting('pYear'),
                             Source = BAM_Info$Link,
                             StockName = BAM_Info$Stock,
                             DiscMortDF = BAM_Info$DiscMortDF,
                             DiscFleets = BAM_Info$DiscFleets,
                             DiscSelFleets = BAM_Info$DiscSelFleets,
                             RetSelFleets = BAM_Info$RetSelFleets,
                             SurveyNames = BAM_Info$SurveyNames,
                             UnitsLandings = BAM_Info$UnitsLandings,
                             UnitsDiscards = BAM_Info$UnitsDiscards
                             )

    OM_List[[i]] <- OM
    cli::cli_text('')
    if (Compare)
      MSEtool::CompareBAM(Stock=BAM_Stock, OM, plot=plot)

    cli::cli_text('')

    if (save) {
      fl <- paste0(BAM_Stock, '.om')
      Save(OM, file.path(OMpath, fl), overwrite = TRUE)
    }

  }
  invisible(OM_List)

}
