library(rstan)
library(abind)
library(readxl)
library(tidyverse)
library(ggplot2)

seq_array_ind <- function(d, col_major = FALSE) {
  #
  # Generate an array of indexes for an array parameter
  # in order of major or column.
  #
  # Args:
  #   d: the dimensions of an array parameter, for example,
  #     c(2, 3).
  #
  #   col_major: Determine what is the order of indexes.
  #   If col_major = TRUE, for d = c(2, 3), return
  #   [1, 1]
  #   [2, 1]
  #   [1, 2]
  #   [2, 2]
  #   [1, 3]
  #   [2, 3]
  #   If col_major = FALSE, for d = c(2, 3), return
  #   [1, 1]
  #   [1, 2]
  #   [1, 3]
  #   [2, 1]
  #   [2, 2]
  #   [2, 3]
  #
  # Returns:
  #   If length of d is 0, return empty vector.
  #   Otherwise, return an array of indexes, each
  #   row of which is an index.
  #
  # Note:
  #   R function arrayInd might be helpful sometimes.
  #
  if (length(d) == 0L)
    return(numeric(0L))

  total <- prod(d)
  if (total == 0L)
    return(array(0L, dim = 0L))

  len <- length(d)
  if (len == 1L)
    return(array(1:total, dim = c(total, 1)))

  res <- array(1L, dim = c(total, len))

  # Handle cases like 1x1 matrices
  if (total == 1)
    return(res)

  jidx <- if (col_major) 1L:len else len:1L
  for (i in 2L:total) {
    res[i, ] <- res[i - 1, ]
    for (j in jidx) {
      if (res[i - 1, j] < d[j]) {
        res[i, j] <- res[i - 1, j] + 1
        break
      }
      res[i, j] <- 1
    }
  }
  res
}

flat_one_par <- function(n, d, col_major = FALSE) {
  # Return all the elemenetwise parameters for a vector/array
  # parameter.
  #
  # Args:
  #  n: Name of the parameter. For example, n = "alpha"
  #  d: A vector indicates the dimensions of parameter n.
  #     For example, d = c(2, 3).  d could be empty
  #     as well when n is a scalar.
  #
  if (0 == length(d)) return(n)
  nameidx <- seq_array_ind(d, col_major)
  names <- apply(nameidx, 1, function(x) paste(n, "[", paste(x, collapse = ','), "]", sep = ''))
  as.vector(names)
}

extract_array <- function(theta, name, dim) {
    n <- flat_one_par(name, dim, col_major = TRUE)
    array(theta[n], dim=dim)
}

X <- read_excel("ReliefShiftData.xlsx", sheet="Relief") %>%
    dplyr::filter(Phase == "Extinction_phas") %>%
    dplyr::mutate(
        CSType = dplyr::recode_factor(CSType, "CSmin1" = "CSmin", "CSmin2" = "CSmin"),
        TimingCS = factor(TimingCS, levels = c(1, 0), labels = c("Onset", "Offset"))
    ) %>%
    dplyr::group_by(ID, CSType, TimingCS, TrialCS) %>%
        dplyr::summarize(
            ReliefRating = mean(ReliefRating),
            ReliefRatingOnly = mean(ReliefRatingOnly, na.rm = TRUE)
        ) %>%
    dplyr::ungroup()

outliers <- c("RS15")
X <- dplyr::filter(X, !(ID %in% outliers))

Z <- X %>%
    dplyr::group_by(CSType, TrialCS, TimingCS) %>%
    dplyr::summarize(
        mu = mean(ReliefRatingOnly, na.rm=TRUE),
        mu2 = mean(ReliefRating))
ggplot(Z, aes(x=TrialCS, y=mu2, color=CSType)) + geom_point() + facet_wrap(~TimingCS)

model <- stan_model('td.stan')
fit_subject <- function(Y) {
    Ncs <- length(levels(Y$CSType))
    O <- length(levels(Y$TimingCS))
    T <- nrow(Y) %/% O

    relief_onset <- dplyr::filter(Y, TimingCS == "Onset")$ReliefRating
    relief_offset <- dplyr::filter(Y, TimingCS == "Offset")$ReliefRating
    relief <- cbind(relief_onset, relief_offset)

    cs_type <- dplyr::filter(Y, TimingCS == "Onset")$CSType
    cs <- dplyr::recode(cs_type, "CSmin" = 1, "CSplus" = 2)

    rewards <- rep(1, Ncs) # EXT => reward always

    initial <- c("CSmin" = .5, "CSplus" = 0)
    initV <- matrix(initial, nrow = Ncs, ncol = O)

    d <- list(N = 2, T = T, O = O, Ncs = Ncs, relief = relief, cs = cs, reward = rewards, initV = initV)
    theta <- optimizing(model, data = d)$par

    y_pred <- extract_array(theta, "y_pred", c(T, O))
    colnames(y_pred) <- levels(Y$TimingCS)
    y_pred <- cbind(y_pred, cs_type)

    theta <- theta[!startsWith(names(theta), "y_pred")]
    list(theta = theta, y_pred = y_pred)
}
