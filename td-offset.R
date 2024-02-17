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
    array(theta[n], dim = dim)
}

rename_col_by_position <- function(df, position, new_name) {
  new_name <- dplyr::enquo(new_name)
  new_name <- quo_name(new_name)
  dplyr::select(df, !! new_name := !! quo(names(df)[[position]]), dplyr::everything())
}

X <- read_csv("Relief.csv", col_types = cols(
        CSType = col_factor(c("CSmin1", "CSmin2", "CSplus")),
        Phase = col_factor(c("Pavlovian", "Extinction")),
        TimingCS = col_factor(c("CS", "US")),
        Anxiety = col_factor(c("Low", "High")),
    )) %>%
    tidyr::extract(ID, regex = "RS([0-9]+)", into='id', remove = FALSE, convert = TRUE) %>%
    dplyr::mutate(
        # CSType = recode_factor(CSType, "CSmin1"="CSmin", "CSmin2"="CSmin"),
        TimingCS = factor(TimingCS, levels=c("CS","US"), labels=c("Onset", "Offset")),
    )

Ext <- dplyr::filter(X, Phase == "Extinction", TimingCS == "Offset") %>%
     dplyr::arrange(ID, Trial, TimingCS)

N <- length(unique(Ext$ID))
T <- 8 + 5 + 5
Ncs <- 3

relief <- dplyr::filter(Ext, TimingCS == "Offset")$Relief
relief <- aperm(array(relief, dim = c(T, N)), c(2, 1))

cs_type <- dplyr::filter(Ext, TimingCS == "Offset")$CSType
cs <- dplyr::recode(cs_type, "CSmin1" = 1, "CSmin2" = 2, "CSplus" = 3)
cs <- matrix(cs, ncol = T, byrow = TRUE)

# anxious <- (dplyr::group_by(Ext, ID) %>% dplyr::summarize(anxious = first(Anxiety) == "High"))$anxious

rewards <- rep(1, Ncs) # EXT => reward always

initV <- c("CSmin1" = .5, "CSmin2" = .5, "CSplus" = 0.2)

# Z <- X %>%
#     dplyr::group_by(CSType, TrialCS, TimingCS) %>%
#     dplyr::summarize(
#         mu = mean(ReliefRatingOnly, na.rm=TRUE),
#         mu2 = mean(ReliefRating))
# ggplot(Z, aes(x=TrialCS, y=mu2, color=CSType)) + geom_point() + facet_wrap(~TimingCS)

model <- stan_model('td-offset.stan')
d <- list(
    N = N, T = T, Ncs = Ncs, relief = relief, cs = cs,
    # anxious = anxious,
    reward = rewards, tau = 100)
theta <- sampling(model, data = d)

# y_pred <- extract_array(theta, "y_pred", c(T))
# colnames(y_pred) <- levels(Y$TimingCS)
# y_pred <- cbind(y_pred, cs_type)

y_pred <- rstan::extract(theta, pars = "y_pred")[[1]]
# x <- y_pred[1,,]
y_pred_sd <- apply(y_pred, c(2,3), sd)
y_pred_mu <- apply(y_pred, c(2,3), mean)

Y <- dplyr::bind_cols(Ext,
  mu=c(aperm(y_pred_mu, dim = c(3,2,1))),
  sd=c(aperm(y_pred_sd, dim = c(3,2,1))) )

ggplot(Y, aes(x = TrialCS, y = mu, color = CSType, group=CSType)) +
  geom_point(aes(x = TrialCS, y = Relief, color = CSType)) +
  geom_ribbon(aes(x = TrialCS, ymin = mu - sd, ymax=mu+sd, fill = CSType)) +
  geom_line() +
  ylim(c(0, 100)) +
  facet_wrap(ID ~ TimingCS)
