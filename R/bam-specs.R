
#' Specifications for Importing BAM Assessments
#'
#' `BAM_Specs` is a named list of specifications for importing BAM output into
#' an operating model object (see [MSEtool::OM()]).
#'
#' @format
#' `BAM_Specs` is a named list, where the names correspond to a stock assessment
#' available in the `bamExtras` package (i.e., no spaces in name, see [MSEtool::ListBAMStocks()]).
#'
#' Each element in `BAM_Specs` is a named list with the following named elements:
#'
#' - `Stock` - `character(1)`The name of the stock (typically same as the name
#'             of the parentlist element, but includes spaces)
#' - `SEDAR` - `numeric(1)`The SEDAR assessment number. Optional.
#'
#' - `Link` - URL to the SEDAR stock assessment report. Optional but recommended.
#'
#' - `SurveyNames` - A character vector of user-facing names for survey
#'   indices detected in `BAMdata`. Must have the same length as the number of
#'   survey indices found (i.e. indices not matched to any fleet name). Use
#'   this to override the default names derived from column names.
#'
#' - `UnitsLandings` - A character vector of length equal to the number of
#'   landings fleets detected. Required.
#'   Each element must be one of:
#'   - `"1000 lb"` — values will be converted from thousands of pounds to kg.
#'   - `"1000 n"` — values will be multiplied by 1,000 (number in thousands).
#'
#' - `UnitsDiscards` - A character vector of length equal to the number of
#'   discard fleets detected. Accepts the same values as `UnitsLandings`.
#'
#' - `DiscMortDF` - `data.frame` with columns Fleet, Value, and Year
#'  specifying discard mortality rates by fleet and time block. Required in some
#'  cases.
#' - `DiscFleets` - Named character vector mapping retain fleet names
#'   to their corresponding discard fleet names in the BAM output, for cases
#'   where naming conventions differ. Required in some cases.
#' - `DiscSelFleets` - Named character vector mapping retain fleet
#'   names to the selectivity series to use for discards. Required in some
#'  cases. Required in some cases.
#'
#' - `RetSelFleets` - Named character vector mapping retain fleet
#'   names to alternative retention selectivity series. Required in some cases.
#'
#' @examples
#' names(BAM_Specs)
#'
#' BAM_Specs[[1]]
#'
#'
"BAM_Specs"





