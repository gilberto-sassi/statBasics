
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statBasics

`R` package of the course **Métodos Estatísticos** at Federal University
of Bahia.

## Confidence Interval for one population (or group)

Here, we compute the confidence interval using the methods as presented
by Montgomery and Runger (2010). All methods implemented are slighted
modification of methods already implemented in `stats`. The user can
compute bilateral and unilateral confidence intervals.

### Confidence Interval for Proportion

There are three approaches to compute confidence interval for proportion
in this package.

#### Number of success in `n` (scalar value) trials

``` r
library(tidyverse)
library(statBasics)
size  <- 1000
sample <- rbinom(size, 1, prob = 0.5)
n_success <- sum(sample)
ci_1pop_bern(n_success, size, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.437    0.519       0.99
```

#### Number of success in a vector

``` r
library(tidyverse)
library(statBasics)
n <- c(30, 20, 10)
x <- n |> map_int(~ sum(rbinom(1, size = .x, prob = 0.75)))
ci_1pop_bern(x, n, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.584    0.916       0.99
```

#### Vector of success

``` r
library(tidyverse)
library(statBasics)
x <- rbinom(50, size = 1, prob = 0.75)
ci_1pop_bern(x, conf_level = 0.99)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.498    0.862       0.99
```

### Confidence interval of mean (normal Distribution)

Let’s illustrated with a simulated cases the confidence interval for two
cases: 1) standard deviation is known; 2) standard deviation is not
known.

#### Standard Deviation is unknown

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop)
ci_1pop_norm(x, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.78     10.2       0.91
```

#### Standard Deviation is known

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_1pop_norm(x, sd_pop = sd_pop, conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     9.76     10.4       0.91
```

### Confidence interval for standard deviation (normal distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 10
sd_pop <- 2
x <- rnorm(100, mean = media_pop, sd = sd_pop)
ci_1pop_norm(x, parameter = 'variance', conf_level = 0.91)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     3.37     5.47       0.91
```

### Confidence interval for mean (exponential distribution)

``` r
library(tidyverse)
library(statBasics)
media_pop <- 800
taxa_pop <- 1 / media_pop
x <- rexp(100, rate = taxa_pop)
ci_1pop_exp(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     593.     879.       0.95
```

### Confidence interval for mean (general case)

Confidence interval using *t* distribution is suitable, even the
distribution is not normal, as illustrated in the example bellow.

``` r
library(tidyverse)
library(statBasics)
media_pop <- 50
x <- rpois(100, lambda  = media_pop)
ci_1pop_general(x)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1     48.6     51.0       0.95
```

## Hypothesis testing for one population (or group)

In the following, we will illustrate the use of this package to test
scientific hypothesis for one sample with examples. All methods are
already implemented in `R`, I have only made slightly modifications to
teaching purposes.

### Hypothesis testing for mean

In the examples below, `mean_null` is the mean in the null hypothesis
`H0`:

1.  `alternative == "two.sided"`: `H0: mu == mean_null` and
    `H1: mu != mean_null`. Default value.
2.  `alternative == "less"`: `H0: mu >= mean_null` and
    `H1: mu < mean_null`
3.  `alternative == "greater"`: `H0: mu =< mean_null` and
    `H1: mu > mean_null`

#### Normal distribution with known variance

``` r
library(tidyverse)
library(statBasics)
mean_null <- 5
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_mean(x, mu = mean_null, conf_level = 0.95, sd_pop = sd_pop, alternative = "two.sided")
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region   alternative    mu sig_level
#>       <dbl>   <dbl>          <dbl> <chr>             <chr>       <dbl>     <dbl>
#> 1      25.0       0           1.96 (-Inf,-1.960)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

#### Normal distribution with unknown variance

``` r
library(tidyverse)
library(statBasics)
mean_null <- 5
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_mean(x, mu = mean_null, conf_level = 0.95, alternative = "two.sided")
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region   alternative    mu sig_level
#>       <dbl>   <dbl>          <dbl> <chr>             <chr>       <dbl>     <dbl>
#> 1      23.8       0           1.98 (-Inf,-1.984)U(1… two.sided       5      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for variance

In the examples below, `sigma_null` is the standard deviation in the
null hypothesis `H0`:

1.  `alternative == "two.sided"`: `H0: sigma == sigma_null` and
    `H1: sigma != sigma_null`. Default value.
2.  `alternative == "less"`: `H0: sigma >= sigma_null` and
    `H1: sigma < sigma_null`
3.  `alternative == "greater"`: `H0: sigma =< sigma_null` and
    `H1: sigma > sigma_null`

``` r
library(tidyverse)
library(statBasics)
sigma_null <- 4
sd_pop <- 2
x <- rnorm(100, mean = 10, sd = sd_pop)
ht_1pop_var(x, sigma = sigma_null, conf_level = 0.95, alternative = "two.sided")
#> # A tibble: 2 × 10
#>   statistic  p_value critical_value critical_region  alternative sigma sig_level
#>       <dbl>    <dbl>          <dbl> <chr>            <chr>       <dbl>     <dbl>
#> 1      22.0 3.55e-17           73.4 (0,73.361)U(128… two.sided       4      0.05
#> 2      22.0 3.55e-17          128.  (0,73.361)U(128… two.sided       4      0.05
#> # … with 3 more variables: lower_ci <dbl>, upper_ci <dbl>, conf_level <dbl>
```

### Hypothesis testing for proportion

In the examples below, `proportion_null` is the standard deviation in
the null hypothesis `H0`:

1.  `alternative == "two.sided"`: `H0: proportion == proportion_null`
    and `H1: proportion != proportion_null`. Default value.
2.  `alternative == "less"`: `H0: proportion >= proportion_null` and
    `H1: proportion < proportion_null`
3.  `alternative == "greater"`: `H0: proportion =< proportion_null` and
    `H1: proportion > proportion_null`

#### Number of success

You can have the number of success (in a number of trials) as a scalar.

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.1
p0 <- 0.75
x <- rbinom(1, size = 1000, prob = p0)
ht_1pop_prop(x, 1000, proportion = p0, alternative = "two.sided", conf_level = 0.95)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region        alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>                  <chr>            <dbl>
#> 1     0.219   0.827           1.96 (-Inf,-1.960)U(1.960,… two.sided         0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Number of success in a vector

You can have the number of success as a vector. You must also give the
vector of number of trials.

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.9
p0 <- 0.75
n <- c(10, 20, 30)
x <- n |> map_int(~ rbinom(1, .x, prob = p0))
ht_1pop_prop(x, n, proportion = p0, alternative = "less", conf_level = 0.99)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>           <chr>            <dbl>
#> 1     0.894   0.814          -1.64 (-Inf,-1.645)   less              0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

#### Vector of success (0 or 1)

You can have the number of success (zero ou one).

``` r
library(tidyverse)
library(statBasics)
proportion_null <- 0.1
p0 <- 0.75
x <- rbinom(1000, 1, prob = p0)
ht_1pop_prop(x, proportion = p0, alternative = "greater", conf_level = 0.95)
#> # A tibble: 1 × 10
#>   statistic p_value critical_value critical_region alternative proportion
#>       <dbl>   <dbl>          <dbl> <chr>           <chr>            <dbl>
#> 1     0.657   0.256           1.64 (1.645, Inf)    greater           0.75
#> # … with 4 more variables: sig_level <dbl>, lower_ci <dbl>, upper_ci <dbl>,
#> #   conf_level <dbl>
```

## Confidence Interval for two population (or two group)

### Bernoulli distribution

There are two approaches to compute confidence interval for difference
of proportions in this package.

#### Confidence Interval – Number of success

In this case, we have the number of trials (`n_x` and `n_y`) and the
number of success (`x` and `y`) for both popuations or groups.

``` r
x <- 3
n_x <- 100
y <- 50
n_y <- 333
ci_2pop_bern(x, y, n_x, n_y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.232 -0.00840       0.95
```

#### Confidence Interval – vectors of 0 and 1

In this case, we have a vector of 0 and 1 (`x` and `y`) for both
populations or groups.

``` r
x <- rbinom(100, 1, 0.75)
y <- rbinom(500, 1, 0.75)
ci_2pop_bern(x, y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1  -0.0954    0.119       0.95
```

### Normal distribution

In this case, we can build the interval of the difference of means of
two populations (or groups) for known and unknown variances, and we can
build the ration of the variances of two populations (or groups).

#### Confidence Interval – comparing the mean and the unknown variances

In this case, we can build the difference of means of two populations
(or groups) when we do not know the variances.

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 1)
# unknown variance and confidence interval for difference of means
ci_2pop_norm(x, y)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1  -0.0401    0.233       0.95
```

#### Confidence Interval – comparing the mean and the unknown variances

In this case, we can build the difference of means of two populations
(or groups) for known variances.

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 3)
# known variance and confidence interval for difference of means
ci_2pop_norm(x, y, sd_pop_1 = 2, sd_pop_2 = 3)
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1   -0.184    0.263       0.95
```

#### Confidence Interval – comparing the variances

In this case, we can build the ration of variances of two populations
(or groups).

``` r
x <- rnorm(1000, mean = 0, sd = 2)
y <- rnorm(1000, mean = 0, sd = 3)
# confidence interval for the variance ratio of 2 populations
ci_2pop_norm(x, y, parameter = "variance")
#> # A tibble: 1 × 3
#>   lower_ci upper_ci conf_level
#>      <dbl>    <dbl>      <dbl>
#> 1    0.399    0.511       0.95
```

## Hypothesis Testing for two population (or two group)

### Comparing proportions of two populations (or groups)

There two approaches to compare the proportions of two populations (or
groups):

-   We have the numbers of sucecss (`x` and `y`) and the numbers of
    trials (`n_x` and `n_y`) for both populations (or groups);
-   We have vector of 1 (success) and 0 (failure) for both populations
    (or groups).

#### Vector of 1 and 0

In this case, we have a vector of 1 (success) and 0 (failure) for both
populations (or groups).

``` r
x <- rbinom(100, 1, 0.75)
y <- rbinom(500, 1, 0.75)
ht_2pop_prop(x, y)
#> [1] FALSE
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region           delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                     <dbl> <chr>      
#> 1     0.436   0.663           1.96 (-Inf,-1.960)U(1.960,Inf)     0 two.sided
```

#### Number of success

In this case, we have the number of success and the number of trials for
both populations (or groups).

``` r
x <- 3
n_x <- 100
y <- 50
n_y <- 333
ht_2pop_prop(x, y, n_x, n_y)
#> [1] FALSE
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region           delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                     <dbl> <chr>      
#> 1     -3.21 0.00131           1.96 (-Inf,-1.960)U(1.960,Inf)     0 two.sided
```

## Hypothesis Testing to compare mean of two populations (or groups)

There are three cases when comparing means of two populations:

1.  `t-test` when variances are unknown where the variances are equals;
2.  `t-test` when variances are unknown where the variances are
    differents;
3.  `z-test` when the variances are known.

### Comparing the means – variances unknown and differents (`t-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(500, mean = 5, sd = 1)
# H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
ht_2pop_mean(x, y, delta = -1)
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region            delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                      <dbl> <chr>      
#> 1      78.7       0           1.96 (-Inf,-1.962)U(1.962, Inf)    -1 two.sided
```

### Comparing the means – variances unknown and equal (`t-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 2)
y <- rnorm(500, mean = 5, sd = 2)
# H0: mu_1 - mu_2 == -1 versus H1: mu_1 - mu_2 != -1
ht_2pop_mean(x, y, delta = -1, var_equal = TRUE)
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region            delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>                      <dbl> <chr>      
#> 1      54.5       0           1.96 (-Inf,-1.962)U(1.962, Inf)    -1 two.sided
```

### Comparing the means – variances known (`z-test`)

``` r
x <- rnorm(1000, mean = 10, sd = 3)
x <- rnorm(500, mean = 5, sd = 1)
# H0: mu_1 - mu_2 >= 0 versus H1: mu_1 - mu_2 < 0
ht_2pop_mean(x, y, delta = 0, sd_pop_1 = 3, sd_pop_2 = 1, alternative = "less")
#> # A tibble: 1 × 6
#>   statistic p_value critical_value critical_region delta alternative
#>       <dbl>   <dbl>          <dbl> <chr>           <dbl> <chr>      
#> 1     -1.32  0.0929          -1.64 (-Inf, -1.645)      0 less
```

## Hypothesis Testing to compare variances of two populations (or groups)

``` r
x <- rnorm(100, sd = 2)
y <- rnorm(1000, sd = 10)
ht_2pop_var(x, y)
#> # A tibble: 2 × 7
#>   statistic  p_value critical_vale ratio alternative lower_ci upper_ci
#>       <dbl>    <dbl>         <dbl> <dbl> <chr>          <dbl>    <dbl>
#> 1    0.0362 2.36e-51         0.733     1 two.sided     0.0364   0.0364
#> 2    0.0362 2.36e-51         1.32      1 two.sided     0.0364   0.0364
```
