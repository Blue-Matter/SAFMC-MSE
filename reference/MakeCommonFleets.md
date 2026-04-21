# Make Common Fleets Across Operating Models

Combines fleets within each single-stock OM, adds dummy fleets where
required, and reorders all OMs so they share an identical fleet
structure. Optionally saves the resulting OM objects to disk.

## Usage

``` r
MakeCommonFleets(
  OM_List = NULL,
  CommonFleets,
  StockFleetList,
  OMpath = "Objects/OM",
  save = TRUE
)
```

## Arguments

- OM_List:

  Optional named list of single-stock `om` objects. If `NULL` (default),
  OMs are loaded from `OMpath` via
  [`LoadOM()`](https://safmc-mse.bluematterscience.com/reference/OM-files.md).

- CommonFleets:

  Character vector of fleet names that every OM must contain after
  processing, in the desired order.

- StockFleetList:

  Named list where each element corresponds to a stock in `OM_List`
  (matched by name) and contains a fleet list passed to
  [`MSEtool::CombineFleets()`](https://msetool.openmse.com/reference/CombineFleets.html).
  Used to combine multiple fleets within each stock into a single fleet.

- OMpath:

  Path to the directory containing OM objects. Used both for loading
  (when `OM_List = NULL`) and saving. Defaults to `"Objects/OM"`.

- save:

  Logical. If `TRUE` (default), each processed OM is saved to
  `<OMpath>/MultiStock/<StockName>.om`.

## Value

A named list of processed `om` objects, returned invisibly. Each OM has
fleets combined per `StockFleetList`, dummy fleets added for any missing
`CommonFleets`, and all fleets reordered to match `CommonFleets`.

Processing proceeds in three steps:

1.  **Combine fleets** — for each stock named in `StockFleetList`,
    [`MSEtool::CombineFleets()`](https://msetool.openmse.com/reference/CombineFleets.html)
    is called with the supplied fleet list.

2.  **Add dummy fleets** —
    [`MissingFleets()`](https://safmc-mse.bluematterscience.com/reference/fleet-tables.md)
    identifies any `CommonFleets` absent from each OM;
    [`MSEtool::AddFleet()`](https://msetool.openmse.com/reference/AddFleet.html)
    adds an empty placeholder for each.

3.  **Reorder** —
    [`MSEtool::Reorder()`](https://msetool.openmse.com/reference/Reorder.html)
    reorders every OM's fleets to match `CommonFleets` exactly.

An error is thrown if, after steps 1 and 2, any OM contains fleets not
in `CommonFleets`, or if any name in `StockFleetList` is not found in
`OM_List`.

## See also

[`LoadOM()`](https://safmc-mse.bluematterscience.com/reference/OM-files.md),
[`MissingFleets()`](https://safmc-mse.bluematterscience.com/reference/fleet-tables.md),
[`MSEtool::Reorder()`](https://msetool.openmse.com/reference/Reorder.html),
[`MSEtool::CombineFleets()`](https://msetool.openmse.com/reference/CombineFleets.html),
[`MSEtool::AddFleet()`](https://msetool.openmse.com/reference/AddFleet.html)
