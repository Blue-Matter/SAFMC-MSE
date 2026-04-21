# Import BAM Operating Models

Imports one or more stock Operating Models (OMs) from BAM (Beaufort
Assessment Model) output via
[`MSEtool::ImportBAM()`](https://msetool.openmse.com/reference/ImportBAM.html),
optionally validates them against BAM population dynamics with
[`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html),
and saves each OM to disk as a `.om` file.

## Usage

``` r
Import_OMs(
  Import_Stocks = NULL,
  Compare = TRUE,
  plot = FALSE,
  save = TRUE,
  OMpath = "Objects/OM/SingleStock"
)
```

## Arguments

- Import_Stocks:

  `character`. Names of stocks to import. If `NULL` (default), all
  stocks defined in `BAM_Specs`

- Compare:

  `logical`. If `TRUE` (default), runs
  [`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html)
  on each imported OM to validate against BAM reference points.

- plot:

  `logical`. If `TRUE`, diagnostic plots are produced during
  [`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html).
  Passed directly to that function. Defaults to `FALSE`.

- save:

  `logical`. Save the OM to disk? Default TRUE

- OMpath:

  `character`. Directory where `.om` files are saved. Defaults to
  `"Objects/OM/SingleStock"`.

## Value

Called for its side effects. Each imported OM is saved to `OMpath` as
`<StockName>.om`. Returns list of imported OMs invisibly.

## Details

Project-wide settings `nSim`, `pYear`) are loaded at the start of the
function via
[`Settings()`](https://safmc-mse.bluematterscience.com/reference/Settings.md).
Each stock's BAM configuration is drawn from
[BAM_Specs](https://safmc-mse.bluematterscience.com/reference/BAM_Specs.md).

## See also

[`Settings()`](https://safmc-mse.bluematterscience.com/reference/Settings.md),
[`MSEtool::ImportBAM()`](https://msetool.openmse.com/reference/ImportBAM.html),
[`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html)
