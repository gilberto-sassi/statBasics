
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statBasics

`R` package of the course **Métodos Estatísticos** at Federal University
of Bahia.

# Confidence Interval

Here, we compute the confidence interval using the methods as presented
by Montgomery and Runger (2010). All methods implemented are slighted
modification of methods already implemented in `stats`. The user can
compute bilateral and unilateral confidence intervals.

## Confidence Intervalo for Proportion

There are three approaches to compute confidence interval for proportion
in this package.

### Number of sucess in `n` (scalar value) trials

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ ggplot2 3.3.3     ✔ purrr   0.3.4
#> ✔ tibble  3.1.1     ✔ dplyr   1.0.5
#> ✔ tidyr   1.1.3     ✔ stringr 1.4.0
#> ✔ readr   1.4.0     ✔ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(statBasics)
size  <- 1000
sample <- rbinom(size, 1, prob = 0.5)
n_success <- sum(sample)
ci_bern(n_success, size, conf_level = 0.99)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.475    0.557       0.99
```

### Number of sucess in a vector

``` r
library(tidyverse)
library(statBasics)
n <- c(30, 20, 10)
x <- seq_along(n) %>% map_int(~ sum(rbinom(1, size = n[.x], prob = 0.75)))
ci_bern(x, n, conf_level = 0.99)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.617    0.950       0.99
```

### Vector of success

``` r
library(tidyverse)
library(statBasics)
x <- rbinom(50, size = 1, prob = 0.75)
ci_bern(x, conf_level = 0.99)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.678        1       0.99
```

## Confidence interval of mean (normal Distribution)

Let’s illustrated with a simulated cases the confidence interval for two
cases: 1) standard deviation is known; 2) standard deviation is not
known.

### Standard Deviation is unknown

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop)
ci_norm(x, conf_level = 0.91)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     10.0     10.3       0.91
```

### Standard Deviation is known

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_norm(x, sd_pop = sd_pop, conf_level = 0.91)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.72     10.4       0.91
```

## Confidence interval for standard deviation (normal distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_norm(x, parameter = 'variance', conf_level = 0.91)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     3.22     5.23       0.91
```

## Confidence interval for mean (exponential distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 800
taxa_pop <- 1 / media_pop
x <- rexp(100, rate = taxa_pop)
ci_exp(x)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     707.    1047.       0.95
```

## Confidence interval for mean

Confidence interval using *t* distribution is suitable, even the
distribution is not normal, as illustrated in the example bellow.

``` r
library(tidyverse)
library(statBasics)
media_pop <- 50
x <- rpois(100, lambda  = media_pop)
ci_general(x)
#> # A tibble: 1 x 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     48.6     51.2       0.95
```
