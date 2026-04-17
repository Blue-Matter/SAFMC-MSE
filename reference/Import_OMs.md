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
  OMpath = "Objects/OM/SingleStock"
)
```

## Arguments

- Import_Stocks:

  `character`. Names of stocks to import. If `NULL` (default), all
  stocks defined in `BAM_Info_List` (loaded via
  [`LoadSettings()`](https://safmc-mse.bluematterscience.com/reference/LoadSettings.md))
  are imported.

- Compare:

  `logical`. If `TRUE` (default), runs
  [`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html)
  on each imported OM to validate against BAM reference points.

- plot:

  `logical`. If `TRUE`, diagnostic plots are produced during
  [`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html).
  Passed directly to that function. Defaults to `FALSE`.

- OMpath:

  `character`. Directory where `.om` files are saved. Defaults to
  `"Objects/OM/SingleStock"`.

## Value

Called for its side effects. Each imported OM is saved to `OMpath` as
`<StockName>.om`. Returns `NULL` invisibly.

## Details

Project-wide settings (`BAM_Info_List`, `nSim`, `pYear`) are loaded at
the start of the function via
[`LoadSettings()`](https://safmc-mse.bluematterscience.com/reference/LoadSettings.md).
Each stock's BAM configuration is drawn from `BAM_Info_List`.

## See also

[`LoadSettings()`](https://safmc-mse.bluematterscience.com/reference/LoadSettings.md),
[`MSEtool::ImportBAM()`](https://msetool.openmse.com/reference/ImportBAM.html),
[`MSEtool::CompareBAM()`](https://msetool.openmse.com/reference/CompareBAM.html)
