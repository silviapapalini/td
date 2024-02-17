library(nlme)
library(abind)
library(tidyverse)
library(ggplot2)

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

selection <- c("RS01", "RS05", "RS06", "RS07", "RS08", "RS11", "RS13", "RS14", "RS16", "RS17", "RS21", "RS22", "RS23", "RS27", "RS29", "RS31", "RS32", "RS33", "RS38", "RS39", "RS40", "RS42", "RS46", "RS48")
Ext <- dplyr::filter(X, ID != "RS34", Phase == "Extinction", CSType == "CSplus") %>%
     dplyr::arrange(ID, TrialCS, TimingCS) %>% dplyr::mutate(selected = (ID %in% selection))

temporal_difference_cs <- function(reward, trials, alpha, gamma, initial.onset, initial.offset) {
    Q.onset <- initial.onset
    Q.offset <- initial.offset

    PE <- matrix(NA, nrow = 2, ncol = length(trials)/2)
    # PE <- matrix(NA, nrow = 1, ncol = length(trials))

    for(i in seq_len(ncol(PE))) {
# V[t] <- V[t] + alpha * (r + gamma * V[t+1] -V[t])

        E.onset <- gamma * mean(Q.offset) - Q.onset
        Q.onset <- Q.onset + alpha * E.onset

        E.offset <- reward - Q.offset
        Q.offset <- Q.offset + alpha * E.offset

        # PE[,i] <- E.offset
        PE[,i] <- c(E.onset, E.offset)
    }

    return(PE)
}

temporal_difference_mix <- function(cs, rewards, alpha, gamma, initial) {
    Q.onset <- mean(initial)
    Q.offset <- initial

    PE <- matrix(NA, nrow = length(cs), ncol = 2)

    for(i in seq_along(trials)) {
        trial.type <- cs[i]
        reward <- rewards[trial.type]

# V[t] <- V[t] + alpha * (r + gamma * V[t+1] - V[t])

        E.onset <- gamma * mean(Q.offset) - Q.onset
        Q.onset <- Q.onset + alpha * E.onset

        E.offset <- reward - Q.offset
        Q.offset <- Q.offset + alpha * E.offset

        PE[i,] <- c(E.onset, E.offset)
    }

    return(PE)
}

first <- function(x) head(x, n=1)

td <- function(ID, Trial, alpha, gamma, init.onset, init.offset) {
    s <- split(1:length(ID), ID)
    PE <- rep(NA, length(ID))

    if(length(gamma) == 1)
        gamma <- rep(gamma, length(ID))
    if(length(init.onset) == 1)
        init.onset <- rep(init.onset, length(ID))

    for(subj in s) {
        f <- first(subj)
        PE[subj] <- temporal_difference_cs(
            1.0, Trial[subj],
            alpha[f], gamma[f],
            init.onset[f], init.offset[f])
    }

    return(c(PE) * 100)
}

partial <- function(f, x, eps = 1e-6) {
  n <- length(x)
  dx <- x + diag(eps, n, n)

  fx <- f(x)
  fdx <- vector(length = n)

  for(i in 1:n) {
    fdx[i] <- (f(dx[,i]) - fx) / eps
  }

  return(fdx)
}

td.print <- function(...) print(td(...))

fit <- nlme(
    Relief ~ td(ID, Trial, alpha, gamma, init.onset, init.offset),
    data = Ext,
    fixed = alpha + gamma + init.onset + init.offset ~ 1,
    random = pdDiag(alpha + init.offset ~ 1),
    groups = ~ ID,
    start = c(
        alpha=.7,
        gamma=.9,
        init.onset = .5,
        init.offset = .2
    ),
    verbose = TRUE
)

Ext$pred <- predict(fit)
ggplot(Ext, aes(x=TrialCS, color=TimingCS)) + geom_point(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(~ID)
