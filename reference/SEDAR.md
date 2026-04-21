# Open the SEDAR Stock Assessment Report for a Stock

Opens the SEDAR stock assessment report for a given stock in the default
web browser. Report URLs are stored in the internal `BAM_Specs` list.

## Usage

``` r
SEDAR(Stock = NULL)
```

## Arguments

- Stock:

  A character string giving the stock name. Spaces are ignored.
  Available stocks can be viewed with `names(BAM_Specs)`.

## Value

Invisibly returns `NULL`. Called for its side effect of opening a URL in
the default browser.

## See also

[BAM_Specs](https://safmc-mse.bluematterscience.com/reference/BAM_Specs.md)
for the full list of available stocks and their metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open the SEDAR report for Red Snapper
SEDAR("RedSnapper")
SEDAR("Red Snapper")  # spaces are ignored
} # }
```
