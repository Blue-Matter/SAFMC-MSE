# Specifications for Importing BAM Assessments

`BAM_Specs` is a named list of specifications for importing BAM output
into an operating model object (see
[`MSEtool::OM()`](https://msetool.openmse.com/reference/OM.html)).

## Usage

``` r
BAM_Specs
```

## Format

`BAM_Specs` is a named list, where the names correspond to a stock
assessment available in the `bamExtras` package (i.e., no spaces in
name, see
[`MSEtool::ListBAMStocks()`](https://msetool.openmse.com/reference/ImportBAM.html)).

Each element in `BAM_Specs` is a named list with the following named
elements:

- `Stock` - `character(1)`The name of the stock (typically same as the
  name of the parentlist element, but includes spaces)

- `SEDAR` - `numeric(1)`The SEDAR assessment number. Optional.

- `Link` - URL to the SEDAR stock assessment report. Optional but
  recommended.

- `SurveyNames` - A character vector of user-facing names for survey
  indices detected in `BAMdata`. Must have the same length as the number
  of survey indices found (i.e. indices not matched to any fleet name).
  Use this to override the default names derived from column names.

- `UnitsLandings` - A character vector of length equal to the number of
  landings fleets detected. Required. Each element must be one of:

  - `"1000 lb"` — values will be converted from thousands of pounds to
    kg.

  - `"1000 n"` — values will be multiplied by 1,000 (number in
    thousands).

- `UnitsDiscards` - A character vector of length equal to the number of
  discard fleets detected. Accepts the same values as `UnitsLandings`.

- `DiscMortDF` - `data.frame` with columns Fleet, Value, and Year
  specifying discard mortality rates by fleet and time block. Required
  in some cases.

- `DiscFleets` - Named character vector mapping retain fleet names to
  their corresponding discard fleet names in the BAM output, for cases
  where naming conventions differ. Required in some cases.

- `DiscSelFleets` - Named character vector mapping retain fleet names to
  the selectivity series to use for discards. Required in some cases.
  Required in some cases.

- `RetSelFleets` - Named character vector mapping retain fleet names to
  alternative retention selectivity series. Required in some cases.

## Examples

``` r
names(BAM_Specs)
#>  [1] "BlackSeaBass"     "GagGrouper"       "GrayTriggerfish"  "GreaterAmberjack"
#>  [5] "RedGrouper"       "RedPorgy"         "RedSnapper"       "ScampGrouper"    
#>  [9] "SnowyGrouper"     "Tilefish"         "VermilionSnapper"

BAM_Specs[[1]]
#> $Stock
#> [1] "Black Sea Bass"
#> 
#> $SEDAR
#> [1] 76
#> 
#> $Link
#> [1] "https://sedarweb.org/documents/sedar-76-stock-assessment-report-south-atlantic-black-sea-bass/"
#> 
#> $SurveyNames
#> [1] "MARMAP blackfish/snapper trap" "SERFS Chevron Trap"           
#> 
#> $UnitsLandings
#> [1] "1000 lb"  "1000 lb"  " 1000 lb" "1000 lb"  "1000 lb" 
#> 
#> $UnitsDiscards
#> [1] "1000 lb" "1000 n"  "1000 n" 
#> 
#> $DiscMortDF
#>   Fleet Value Year
#> 1   cHL 0.190 1977
#> 2   cPT 0.140 1977
#> 3   cPT 0.068 2007
#> 4   rHB 0.152 1977
#> 5   rGN 0.137 1977
#> 

```
