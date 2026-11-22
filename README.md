# Hozo

Hozo estimates the mean, standard deviation and variance of a sample from
its median, minimum, maximum and sample size. It uses the methods of Hozo,
Djulbegovic and Hozo (2005). With it, a meta-analysis can include studies
that report a median and range instead of a mean and standard deviation.

## Installation

Install the released version from CRAN:

``` r
install.packages("Hozo")
```

Or install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("sepehr-khsrv/Hozo")
```

## Example

``` r
library(Hozo)

# One study: median 10, range 5 to 15, 30 participants
estimate_stats(median = 10, minimum = 5, maximum = 15, sample_size = 30)
#>   mean  sd variance mean_method sd_method
#> 1   10 2.5     6.25      median   range/4
```

The inputs are vectorised, so one call converts a whole table of studies:

``` r
studies <- data.frame(
  study   = c("A", "B", "C"),
  median  = c(12, 30, 7.5),
  minimum = c(4, 18, 2),
  maximum = c(25, 52, 16),
  n       = c(12, 40, 120)
)
est <- estimate_stats(studies$median, studies$minimum,
                      studies$maximum, studies$n)
cbind(studies["study"], est)
#>   study  mean       sd  variance      mean_method         sd_method
#> 1     A 13.25 6.104984 37.270833 Hozo formula (5) Hozo formula (16)
#> 2     B 30.00 8.500000 72.250000           median           range/4
#> 3     C  7.50 2.333333  5.444444           median           range/6
```

## Which estimator is used

The function follows Table 3 of Hozo et al. (2005). Here *a* is the
minimum, *m* the median, *b* the maximum and *n* the sample size.

| Sample size  | Mean                        | Standard deviation |
|--------------|-----------------------------|--------------------|
| n ≤ 15       | (a + 2m + b) / 4            | formula (16)       |
| 15 < n ≤ 25  | (a + 2m + b) / 4            | range / 4          |
| 25 < n ≤ 70  | median                      | range / 4          |
| n > 70       | median                      | range / 6          |

Formula (16) is S² = ((a − 2m + b)² / 4 + (b − a)²) / 12. For samples of
25 or fewer, `mean_formula = "4"` adds the sample-size correction of
formula (4), (a − 2m + b) / (4n), to the mean.

## Reference

Hozo SP, Djulbegovic B, Hozo I (2005). Estimating the mean and variance
from the median, range, and the size of a sample. *BMC Medical Research
Methodology*, 5, 13. <https://doi.org/10.1186/1471-2288-5-13>
