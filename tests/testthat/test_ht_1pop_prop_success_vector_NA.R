prop_pop <- 0.65
size  <- c(10, 20, 30, NA, 40)
amostra <- c(c(10, 20, 30) %>% purrr::map_int(~ rbinom(1, .x, prob = prop_pop)), 10, NA)
x0 <- amostra[!(is.na(size) | is.na(amostra))]
n0 <- size[!(is.na(size) | is.na(amostra))]
p0_0 <- prop_pop
p0_1 <- 0.1
conf_level  <- 0.99
sig_level <- 0.01

prop_test <- function(amostra, n = NULL, p = 0.5, alternative = "two.sided", conf_level = conf_level) {
    ci <- ci_1pop_bern(amostra, n, conf_level = conf_level)
    output <- prop.test(sum(amostra), sum(n), p = p, alternative = alternative, conf.level = conf_level, correct = FALSE)
    list(
        statistic = base::unname(output$statistic),
        p_value = base::unname(output$p.value),
        lower_ci = ci$lower_ci,
        upper_ci = ci$upper_ci
    )
}

######################################################
# alternative: "two.sided"
# Decision: H0
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = p0_0, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test bilateral, decision H0", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = p0_0, alternative = "two.sided", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = p0_0, alternative = "two.sided", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = p0_0, alternative = "two.sided", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = p0_0, alternative = "two.sided", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level / 2))
})

######################################################
# alternative: "less"
# Decision: H0
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = p0_0, alternative = "less", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test unilateral, decision H0", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = p0_0, alternative = "less", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = p0_0, alternative = "less", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = p0_0, alternative = "less", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = p0_0, alternative = "less", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(sig_level))
})

######################################################
# alternative: "greater"
# Decision: H0
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = p0_0, alternative = "greater", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test unilateral, decision H0", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = p0_0, alternative = "greater", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = p0_0, alternative = "greater", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = p0_0, alternative = "greater", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = p0_0, alternative = "greater", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level))
})

######################################################
# alternative: "two.sided"
# Decision: H1
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = p0_1, alternative = "two.sided", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test bilateral, decision H1", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = p0_1, alternative = "two.sided", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = p0_1, alternative = "two.sided", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = p0_1, alternative = "two.sided", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = p0_1, alternative = "two.sided", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level / 2))
})

######################################################
# alternative: "less"
# Decision: H1
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = 1 - p0_1, alternative = "less", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test unilateral, decision H1", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = 1 - p0_1, alternative = "less", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = 1 - p0_1, alternative = "less", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = 1 - p0_1, alternative = "less", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = 1 - p0_1, alternative = "less", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(sig_level))
})

######################################################
# alternative: "greater"
# Decision: H1
# prop-test
output <- ht_1pop_prop(amostra, size, proportion = p0_1, alternative = "greater", conf_level = conf_level, sig_level = sig_level)
testthat::test_that("prop-test unilateral, decision H1", {
    testthat::expect_equal((output$statistic)^2, prop_test(x0, n0, p = p0_1, alternative = "greater", conf_level = conf_level)$statistic)
    testthat::expect_equal(output$p_value, prop_test(x0, n0, p = p0_1, alternative = "greater", conf_level = conf_level)$p_value)
    testthat::expect_equal(output$lower_ci, prop_test(x0, n0, p = p0_1, alternative = "greater", conf_level = conf_level)$lower_ci)
    testthat::expect_equal(output$upper_ci, prop_test(x0, n0, p = p0_1, alternative = "greater", conf_level = conf_level)$upper_ci)
    testthat::expect_equal(output$critical_value, qnorm(1 - sig_level))
})
