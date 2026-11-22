# Hozo 0.1.0

* First CRAN release.
* `estimate_stats()` estimates the mean, standard deviation and variance of a
  sample from its median, minimum, maximum and size. It uses the estimator
  that Table 3 of Hozo et al. (2005) recommends for each sample size.
* The inputs are vectorised, so one call converts a whole table of studies.
  A missing value gives `NA` in the matching row.
* For samples of 15 or fewer, the standard deviation now comes from formula
  (16) of Hozo et al. (2005). The earlier GitHub version used a formula that
  is not in the paper.
* The new `mean_formula` argument selects formula (5) (default) or
  formula (4) for the mean of samples of 25 or fewer.
* The result is now a data frame. It also reports which estimator was used
  for each row.
