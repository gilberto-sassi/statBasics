size <- 1000
mean_pop <- 3
sd_pop <- 2
x <- rnorm(size, mean = mean_pop, sd = sd_pop)

###############################################################################
# type = 'two.sided'

lower <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(qnorm((1 - conf_level) / 2) * sd_pop / sqrt(size) + mean(x))
  } else if (type == 'left') {
    return(qnorm(1 - conf_level) * sd_pop / sqrt(size) + mean(x))
  } else if (type == 'right') {
    return(0)
  }
}

upper <- function(conf_level, type = 'two.sided') {
  if (type == 'two.sided') {
    return(qnorm((1 + conf_level) / 2) * sd_pop / sqrt(size) + mean(x))
  } else if (type == 'left') {
     return(Inf)
  } else if (type == 'right') {
      return(qnorm(conf_level) * sd_pop / sqrt(size) + mean(x))
  }
}

testthat::test_that("Comparing, type = 'two.sided', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90)$lower_ci, EnvStats::varTest(x, conf.level = 0.90)$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90)$upper_ci, EnvStats::varTest(x, conf.level = 0.90)$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95)$lower_ci, EnvStats::varTest(x, conf.level = 0.95)$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95)$upper_ci, EnvStats::varTest(x, conf.level = 0.95)$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'two.sided', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99)$lower_ci, EnvStats::varTest(x, conf.level = 0.99)$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99)$upper_ci, EnvStats::varTest(x, conf.level = 0.99)$conf.int[2] %>% unname())
})

###############################################################################
# type = 'left'

testthat::test_that("Comparing, type = 'left', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'left')$lower_ci, EnvStats::varTest(x, conf.level = 0.90, alternative = 'greater')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'left')$upper_ci, EnvStats::varTest(x, conf.level = 0.90, alternative = 'greater')$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'left')$lower_ci, EnvStats::varTest(x, conf.level = 0.95, alternative = 'greater')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'left')$upper_ci, EnvStats::varTest(x, conf.level = 0.95, alternative = 'greater')$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'two.sides', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'left')$lower_ci, EnvStats::varTest(x, conf.level = 0.99, alternative = 'greater')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'left')$upper_ci, EnvStats::varTest(x, conf.level = 0.99, alternative = 'greater')$conf.int[2] %>% unname())
})

###############################################################################
# type = 'right'

testthat::test_that("Comparing, type = 'right', conf_level = '90%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'right')$lower_ci, EnvStats::varTest(x, conf.level = 0.90, alternative = 'less')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.90, type = 'right')$upper_ci, EnvStats::varTest(x, conf.level = 0.90, alternative = 'less')$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'left', conf_level = '95%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'right')$lower_ci, EnvStats::varTest(x, conf.level = 0.95, alternative = 'less')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.95, type = 'right')$upper_ci, EnvStats::varTest(x, conf.level = 0.95, alternative = 'less')$conf.int[2] %>% unname())
})

testthat::test_that("Comparing, type = 'right', conf_level = '99%'", {
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'right')$lower_ci, EnvStats::varTest(x, conf.level = 0.99, alternative = 'less')$conf.int[1] %>% unname())
  testthat::expect_equal(ci_norm(x, parameter = 'variance', conf_level = 0.99, type = 'right')$upper_ci, EnvStats::varTest(x, conf.level = 0.99, alternative = 'less')$conf.int[2] %>% unname())
})
