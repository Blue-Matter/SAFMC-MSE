# Manage Operating Model Files

A family of functions for listing, loading, and deleting saved Operating
Model (OM) files stored under the `Objects/OM/` directory structure.

## Usage

``` r
ListOMFiles(
  OMpath = "Objects/OM",
  type = c("SingleStock", "MultiStock"),
  print = FALSE
)

LoadOM(
  name = NULL,
  OMpath = "Objects/OM",
  type = c("SingleStock", "MultiStock")
)

DeleteOMs(OMpath = "Objects/OM", type = c("SingleStock", "MultiStock"))
```

## Arguments

- OMpath:

  `character(1)`. Path to the directory containing OM subdirectories.
  Defaults to `"Objects/OM"`.

- type:

  `character(1)`. Type of Operating Model. One of `"SingleStock"`
  (default) or `"MultiStock"`. Determines the subdirectory used within
  `OMpath`.

- print:

  `logical(1)`. If `TRUE`, prints the list of found OM files to the
  console. If `FALSE` (default), returns the file paths invisibly
  without printing.

- name:

  `character(1)`. Name of the OM to load, without the `.om` extension.
  Defaults to `"BlackSeaBass"`. (`LoadOM` only.)

## Value

- `ListOMFiles`: character vector of full file paths. Returned invisibly
  when `print = FALSE`.

- `LoadOM`: the `OM` object.

- `DeleteOMs`: a logical vector from
  [`base::file.remove()`](https://rdrr.io/r/base/files.html), one
  element per file, indicating whether each deletion succeeded.
