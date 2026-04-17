#' Fleet and Stock-Fleet Reference Tables
#'
#' `FleetTable()` retrieves all Operating Models via [LoadOM()], extracts stock
#' and fleet names, and returns a tidy data frame joining fleet codes to their
#' full names via `FleetCodesTable`. `StockFleetTable()` summarises this by
#' collapsing stocks into a single comma-separated string per fleet.
#'
#' @return
#' - `FleetTable()`: a data frame with one row per stock-fleet combination
#'   and columns:
#'   - `Stock`: stock name derived from the first stock slot of each OM.
#'   - `Code`: short fleet code (e.g. `"cHL"`, `"rHB"`).
#'   - `Fleet`: full fleet name looked up from `FleetCodesTable`.
#' - `StockFleetTable()`: a data frame with one row per fleet and columns:
#'   - `Fleet`: full fleet name.
#'   - `Stock`: comma-separated string of all stocks associated with that fleet.
#'
#' @name fleet-tables
NULL

#' @export
#' @rdname fleet-tables
FleetTable <- function(OM_List=NULL) {

  if (is.null(OM_List))
    OM_List <- LoadOM()
  StockNames_List <- purrr::map(OM_List, \(OM) MSEtool::Name(Stock(OM,1)))
  FleetNames_List <- purrr::map(OM_List, MSEtool::FleetNames)
  names(FleetNames_List) <- StockNames_List

  AllFleets <- FleetNames_List |> unlist() |> unique() |> sort()

  data.frame(
    Stock = rep(names(FleetNames_List), lengths(FleetNames_List)),
    Code = unlist(FleetNames_List, use.names = FALSE)
  ) |> dplyr::left_join(FleetCodesTable, by=dplyr::join_by(Code))

}

#' @export
#' @rdname fleet-tables
StockFleetTable <- function(OM_List=NULL) {
  FleetTable(OM_List) |>
    dplyr::group_by(Fleet) |>
    dplyr::mutate(nStock=length(unique(Stock))) |>
    dplyr::group_by(Fleet, Code, nStock) |>
    dplyr::summarise(Stock = paste(unique(Stock), collapse = ', '))
}
