#' Import BAM Operating Models
#'
#' Imports one or more stock Operating Models (OMs) from BAM (Beaufort
#' Assessment Model) output via [MSEtool::ImportBAM()], optionally validates
#' them against BAM population dynamics with [MSEtool::CompareBAM()], and saves
#' each OM to disk as a `.om` file.
#'
#' @param Import_Stocks `character`. Names of stocks to import. If `NULL`
#'   (default), all stocks defined in `BAM_Info_List` (loaded via
#'   [LoadSettings()]) are imported.
#' @param Compare `logical`. If `TRUE` (default), runs
#'   [MSEtool::CompareBAM()] on each imported OM to validate against BAM
#'   reference points.
#' @param plot `logical`. If `TRUE`, diagnostic plots are produced during
#'   [MSEtool::CompareBAM()]. Passed directly to that function. Defaults to
#'   `FALSE`.
#' @param OMpath `character`. Directory where `.om` files are saved.
#'   Defaults to `"Objects/OM/SingleStock"`.
#'
#' @return Called for its side effects. Each imported OM is saved to `OMpath`
#'   as `<StockName>.om`. Returns `NULL` invisibly.
#'
#' @details
#' Project-wide settings (`BAM_Info_List`, `nSim`, `pYear`) are loaded at the
#' start of the function via [LoadSettings()]. Each stock's BAM configuration
#' is drawn from `BAM_Info_List`.
#'
#' @seealso [LoadSettings()], [MSEtool::ImportBAM()], [MSEtool::CompareBAM()]
#'
#' @export
Import_OMs <- function(Import_Stocks=NULL, Compare=TRUE, plot=FALSE, OMpath="Objects/OM/SingleStock") {

  LoadSettings()

  if (is.null(Import_Stocks))
    Import_Stocks <- names(BAM_Info_List)

  for (i in seq_along(Import_Stocks)) {

    BAM_Stock <- Import_Stocks[i]
    ind <- match(BAM_Stock, names(BAM_Info_List))
    BAM_Info <- BAM_Info_List[[ind]]


    cli::cli_text('')

    OM <- MSEtool::ImportBAM(Stock = BAM_Stock,
                             nSim = nSim,
                             pYear = pYear,
                             StockName = BAM_Info$Stock,
                             DiscMortDF = BAM_Info$DiscMortDF,
                             DiscFleets = BAM_Info$DiscFleets,
                             DiscSelFleets = BAM_Info$DiscSelFleets,
                             RetSelFleets = BAM_Info$RetSelFleets)

    cli::cli_text('')
    if (Compare)
      MSEtool::CompareBAM(Stock=BAM_Stock, OM, plot=plot)

    cli::cli_text('')

    fl <- paste0(BAM_Stock, '.om')
    saveRDS(OM, file.path(OMpath, fl))

  }

}
