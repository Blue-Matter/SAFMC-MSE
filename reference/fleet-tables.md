# Fleet and Stock-Fleet Reference Tables

`FleetTable()` retrieves all Operating Models via
[`LoadOM()`](https://safmc-mse.bluematterscience.com/reference/OM-files.md),
extracts stock and fleet names, and returns a tidy data frame joining
fleet codes to their full names via `FleetCodesTable`.
`StockFleetTable()` summarises this by collapsing stocks into a single
comma-separated string per fleet.

## Usage

``` r
FleetTable(
  OM_List = NULL,
  OMpath = "Objects/OM",
  type = c("SingleStock", "MultiStock")
)

StockFleetTable(OM_List = NULL)

MissingFleets(
  OM_List = NULL,
  OMpath = "Objects/OM",
  type = c("SingleStock", "MultiStock")
)
```

## Value

- `FleetTable()`: a data frame with one row per stock-fleet combination
  and columns:

  - `Stock`: stock name derived from the first stock slot of each OM.

  - `Code`: short fleet code (e.g. `"cHL"`, `"rHB"`).

  - `Fleet`: full fleet name looked up from `FleetCodesTable`.

- `StockFleetTable()`: a data frame with one row per fleet and columns:

  - `Fleet`: full fleet name.

  - `Stock`: comma-separated string of all stocks associated with that
    fleet.
