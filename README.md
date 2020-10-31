
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forestable

<!-- badges: start -->

<!-- badges: end -->

The goal of forestable is to make it easy for you to create a
publication-quality forest plot with as much or as little information
displayed on either side as you require.

## Installation

This package is currently early in development, and must be installed
from this github repo.

``` r
devtools::install_github("rdboyes/forestable")
```

## Example

Suppose we wish to replicate the following figure published in the NEJM
\[1\]:

``` r
knitr::include_graphics(here::here("man/figures/target_figure.jpg"))
```

<img src="D:/Projects/forestable/man/figures/target_figure.jpg" width="100%" />

Forestable simply requires the left side of the table (in this case,
three columns with Subgroups and counts for each of two groups) and
vectors which contain the point estimates and confidence intervals.

``` r
library(forestable)

table <- readxl::read_excel(here::here("inst/extdata/example_figure_data.xlsx"))

forestable(left_side_data = table[,1:3],
           estimate = table$Estimate,
           ci_low = table$`CI low`,
           ci_high = table$`CI high`,
           display = FALSE,
           file_path = here::here("man/figures/forestable_plot.png"))
#> Warning: Removed 8 rows containing missing values (geom_point).
#> Warning: Removed 8 rows containing missing values (geom_errorbarh).
```

``` r
knitr::include_graphics(here::here("man/figures/forestable_plot.png"))
```

<img src="D:/Projects/forestable/man/figures/forestable_plot.png" width="100%" />

## To Do

  - Additional font support
  - Additional plot types, especially ridgeline plots

# References

1.  Ray, K. K., Wright, R. S., Kallend, D., Koenig, W., Leiter, L. A.,
    Raal, F. J., Bisch, J. A., Richardson, T., Jaros, M., Wijngaard, P.
    L. J., Kastelein, J. J. P., & ORION-10 and ORION-11 Investigators.
    (2020). Two Phase 3 Trials of Inclisiran in Patients with Elevated
    LDL Cholesterol. The New England Journal of Medicine, 382(16),
    1507–1519.
