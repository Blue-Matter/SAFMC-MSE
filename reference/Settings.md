# Get or Set SAMSE Package Settings

Prints current package settings when called with no arguments, or
updates one or more settings when arguments are supplied. Settings
persist for the duration of the R session and can be set at startup via
[`.Rprofile`](https://rdrr.io/r/base/Startup.html) (e.g.
`options(SAMSE.nSim = 100)`).

## Usage

``` r
Settings(...)
```

## Arguments

- ...:

  Named arguments corresponding to one or more settings to update.
  Unrecognised names trigger an error. Valid settings are:

  - `nSim` — number of stochastic simulations (default: `50`).

  - `pYear` — number of projection years (default: `30`).

## Value

Invisibly returns a named list of all current settings after any updates
are applied. When called with no arguments, the current settings are
also printed to the console.

## Examples

``` r
# Print current settings
Settings()
#> 
#> ── `SAMSE` Settings ────────────────────────────────────────────────────────────
#> nSim: 50
#> pYear: 30

# Update one setting
Settings(nSim = 100)

# Update multiple settings at once
Settings(nSim = 200, pYear = 50)

```
