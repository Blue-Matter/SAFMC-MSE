# Load Project Settings

Sources the project specifications file (`0. Specifications.R`) from the
current working directory, making all settings and parameters defined
within it available in the global environment.

## Usage

``` r
LoadSettings()
```

## Value

Nothing. Called for side effects.

The file `0. Specifications.R` must exist in the current working
directory ([`getwd()`](https://rdrr.io/r/base/getwd.html)). This file is
expected to define project-wide settings and parameters.

The working directory should be the root directory of the `SAMSE`
package, available for download from
[GitHub](https://github.com/Blue-Matter/SAFMC-MSE).

## Examples

``` r
if (FALSE) { # \dontrun{
setwd("path/to/project")
LoadSettings()
} # }
```
