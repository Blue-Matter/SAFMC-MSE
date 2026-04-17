# Check Required Package Dependencies

Validates that required packages meet minimum version requirements.
Throws an informative error for each package that is missing or out of
date, including the install command needed to resolve the issue.

## Usage

``` r
CheckPackages()
```

## Value

Invisibly returns `NULL` if all checks pass.

## Details

The following packages and minimum versions are checked:

|             |                 |                                         |
|-------------|-----------------|-----------------------------------------|
| Package     | Minimum Version | Source                                  |
| `MSEtool`   | 4.0.0           | GitHub: `blue-matter/MSEtool\@prelease` |
| `bamExtras` | 0.0.2           | GitHub: `Nikolai-Klibansky/bamExtras`   |

## Examples

``` r
if (FALSE) { # \dontrun{
CheckPackages()
} # }
```
