#' Confidence Interval for Proportion
#' 
#' @param x a vector of counts of successes.
#' @param n a vector of counts of trials.
#' @param conf_level confidence level of the returned confidence interval. Must be a single number between 0 and 1. 
#' @param type a character string specifying the type of confidence interval. Must be one of "two.sided" (default), "right" or "less".
#' 
#' @details \code{type} specifies the type of confidence interval. If \code{type} is "two.sided",  the returned confidence interval is \code{(lower_ci, upper_ci)). If \code{type} is "left", the returned confidence interval is \code{(lower_ci, Inf)}. And, finall, is \code{type} is "right", the returned confidence interval is \code{(-Inf, upper_ci))}.
#' 
#' @examples
#' heads <- rbinom(1, size = 100, prob = .5)
#' ci_binom(heads)
ci_binom <- function(x, n, conf_level = 0.95, type = "two.sided") {
    if (!(type %in% c("two.sided", "left", "right"))) {
        stop("'type' must be one of 'two.sided', 'left' or 'right'.")
    }

    if (conf_level < 0 | conf_level > 1) {
        stop("'conf_level' must be a number between 0 and 1.")
    }

    p_hat  <- sum(x) / sum(n)

    if (type == "two.sided") {
        lower_ci  <- max(qnorm(conf_level / 2) / (2 * sqrt(sum(n))) + p_hat, 0)
        upper_ci <- min(qnorm(1 - conf_level / 2) / (2 * sqrt(sum(n))) + p_hat, 1)
    } else if (type == "left") {
        lower_ci  <- max(qnorm(conf_level) / (2 * sqrt(sum(n))) + p_hat, 0)
        upper_ci  <- Inf
    } else if (type == "right") {
        lower_ci  <- -Inf
        upper_ci <- min(qnorm(1 - conf_level) / (2 * sqrt(sum(n))) + p_hat, 1)
    }

    tibble::tibble(lower_ci, upper_ci, conf_level)
}
