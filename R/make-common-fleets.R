#' Make Common Fleets Across Operating Models
#'
#' Combines fleets within each single-stock OM, adds dummy fleets where
#' required, and reorders all OMs so they share an identical fleet structure.
#' Optionally saves the resulting OM objects to disk.
#'
#' @param OM_List Optional named list of single-stock `om` objects. If `NULL`
#'   (default), OMs are loaded from `OMpath` via [LoadOM()].
#' @param CommonFleets Character vector of fleet names that every OM must
#'   contain after processing, in the desired order.
#' @param StockFleetList Named list where each element corresponds to a stock
#'   in `OM_List` (matched by name) and contains a fleet list passed to
#'   [MSEtool::CombineFleets()]. Used to combine multiple fleets within each
#'   stock into a single fleet.
#' @param OMpath Path to the directory containing OM objects. Used both for
#'   loading (when `OM_List = NULL`) and saving. Defaults to
#'   `"Objects/OM"`.
#' @param save Logical. If `TRUE` (default), each processed OM is saved to
#'   `<OMpath>/MultiStock/<StockName>.om`.
#'
#' @return A named list of processed `om` objects, returned invisibly. Each OM
#'   has fleets combined per `StockFleetList`, dummy fleets added for any
#'   missing `CommonFleets`, and all fleets reordered to match `CommonFleets`.
#'
#' Processing proceeds in three steps:
#'
#' 1. **Combine fleets** — for each stock named in `StockFleetList`,
#'    [MSEtool::CombineFleets()] is called with the supplied fleet list.
#' 2. **Add dummy fleets** — [MissingFleets()] identifies any `CommonFleets`
#'    absent from each OM; [MSEtool::AddFleet()] adds an empty placeholder for
#'    each.
#' 3. **Reorder** — [Reorder()] reorders every OM's fleets to match
#'    `CommonFleets` exactly.
#'
#' An error is thrown if, after steps 1 and 2, any OM contains fleets not in
#' `CommonFleets`, or if any name in `StockFleetList` is not found in
#' `OM_List`.
#'
#' @seealso [LoadOM()], [MissingFleets()], [MSEtool::Reorder()],
#'   [MSEtool::CombineFleets()], [MSEtool::AddFleet()]
#'
#' @export
MakeCommonFleets <- function(OM_List=NULL,
                             CommonFleets,
                             StockFleetList,
                             OMpath = "Objects/OM",
                             save=TRUE) {

  if (is.null(OM_List))
    OM_List <- LoadOM(OMpath=OMpath, type='SingleStock')

  if (any(!names(StockFleetList) %in% names(OM_List)))
    cli::cli_abort("All `StockFleetList` names must be in `names(OM_List)`")

  # Combine Fleets as specified in `StockFleetList`
  for (st in seq_along(StockFleetList)) {

    stock_name <- names(StockFleetList)[st]
    cli::cli_text('')
    cli::cli_alert("{.val {stock_name}}")
    FleetList <- StockFleetList[[st]]
    OM_List[[stock_name]] <- MSEtool::CombineFleets(OM=OM_List[[stock_name]],
                                                    FleetList=FleetList)
  }

  # Add dummy fleets
  MissingFleetDF <- MissingFleets(OM_List, OMpath=OMpath)
  cli::cli_text('Adding Dummy Fleets')
  stocks <- unique(MissingFleetDF$Stock)
  for (st in seq_along(stocks)) {
    stock <- stocks[st]
    cli::cli_alert("{.val {stock}}")

    OM <- OM_List[[gsub(' ', '', stock)]]
    stock_df <- MissingFleetDF |> dplyr::filter(Stock==stock)
    for (fl in seq_len(nrow(stock_df))) {
      FleetName <- stock_df$Code[fl]
      cli::cli_ul('{FleetName}')
      OM <- MSEtool::AddFleet(OM, FleetName)
    }
    OM_List[[gsub(' ', '', stock)]] <- OM
  }

  cli::cli_text('')

  FleetNameList <- purrr::map(OM_List, FleetNames)
  OM_Fleets <- unique(unlist(FleetNameList))
  match <- OM_Fleets %in% CommonFleets
  if (!any(match))
    cli::cli_abort(c('x'='All OMs do not have only `CommonFleet` fleets',
                     'i'='`CommonFleets`: {.val {CommonFleets}}',
                     'i'='`OM_Fleets`: {.val {OM_Fleets}}')
    )

  # Order fleets the same for all OMs
  for (st in seq_along(OM_List)) {
    OM_List[[st]] <- MSEtool::Reorder(OM_List[[st]], Fleets=CommonFleets)
  }

  if (save) {
    for (st in seq_along(OM_List)) {
      OM <- OM_List[[st]]
      stock <- names(OM_List)[st]
      file <- paste0(stock, '.om')
      path <- file.path(OMpath, 'MultiStock', file)
      Save(OM, path, overwrite=TRUE)
    }
  }

  invisible(OM_List)
}

