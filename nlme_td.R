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

selection <- c("RS01", "RS03", "RS05", "RS06", "RS07", "RS08", "RS10", "RS11", "RS13",
    "RS14", "RS16", "RS17", "RS20", "RS21", "RS22", "RS23", "RS26", "RS27", "RS29",
    "RS31", "RS32", "RS33", "RS38", "RS39", "RS40", "RS41", "RS42", "RS46", "RS48", "RS49", "RS50", "RS51")
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

logistic <- function(x) 1 / (1 + exp(-x))
logit <- function(p) log(p / (1 - p))

td <- function(ID, Trial, alpha, gamma, init.onset, init.offset, tau=100) {
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

    return(c(PE) * tau)
    # return(pmax(c(PE) * tau, 0))
}

td_bounded <- function(ID, Trial, alpha, gamma, init.onset, init.offset, tau=100) {
    alpha <- logistic(alpha)
    gamma <- logistic(gamma)
    init.onset <- logistic(init.onset)
    init.offset <- logistic(init.offset)
    td(ID, Trial, alpha, gamma, init.onset, init.offset, tau)
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

S <- dplyr::filter(Ext, selected)
# S <- Ext
fit <- nlme(
    Relief ~ td(ID, Trial, alpha, gamma, init.onset, init.offset),
    data = S,
    fixed = alpha + gamma + init.onset + init.offset ~ 1,
    random = pdDiag(alpha + gamma ~ 1),
    groups = ~ ID,
    start = c(
        alpha = .7,
        gamma = .9,
        init.onset = .5,
        init.offset = .2
    ),
    verbose = TRUE,
    control = nlmeControl(maxIter=100)
)

S$pred <- predict(fit)
ggplot(S, aes(x=TrialCS, color=TimingCS)) + geom_point(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(~ID)

resVar <- function(object) {
    rss <- sum(residuals(object)^2)
    n <- length(residuals(object))
    p <- length(coef(object))
    rss/(n - p)
}

estimate_subject <- function(S) {
    # one subject
    tryCatch({
        fit <- nls(
            Relief ~ td(ID, Trial, alpha, gamma, init.onset, init.offset),
            data = S,
            algorithm = "port",
            start = c(
                alpha = .7,
                gamma = .9,
                init.onset = .5,
                init.offset = .2
            ),
            lower = c(
                alpha = 0.0,
                gamma = 0.0,
                init.onset = 0.0,
                init.offset = 0.0
            ),
            upper = c(
                alpha = 1.0,
                gamma = 1.0,
                init.onset = 1.0,
                init.offset = 1.0
            ),
            trace = TRUE
        )
        pred <- predict(fit)
        cbind(as.data.frame(t(c(coef(fit), var = resVar(fit)))),
            Trial = S$Trial, TimingCS = S$TimingCS, pred = pred)
    }, error = function(e) {
        data.frame(alpha = NA, gamma = NA, init.onset = NA, init.offset = NA, var = NA)
    })
}

fits <- Ext %>% dplyr::group_by(ID) %>% dplyr::group_modify(~ estimate_subject(.x), .keep=TRUE)
Z <- dplyr::left_join(Ext, fits, by = c("ID","Trial", "TimingCS"))
ggplot(Z, aes(x=Trial, color=TimingCS)) + geom_jitter(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(Anxiety~ID)

estimate_learning <- function(S) {
    fit <- lm(Relief ~ TrialCS, data=S)
    pred <- predict(fit)
    cbind(as.data.frame(t(c(coef(fit), var = resVar(fit)))),
        Trial = S$Trial, TimingCS = S$TimingCS, pred = pred
    )
}
ols <- Ext %>% dplyr::group_by(ID) %>% dplyr::filter(TimingCS == "Offset") %>% dplyr::group_modify(~ estimate_learning(.x), .keep=TRUE)

Z <- dplyr::left_join(Ext, ols, by = c("ID","Trial", "TimingCS"))
Z <- Z %>% dplyr::filter(TimingCS == "Offset") %>% dplyr::mutate(Learner = ifelse(TrialCS.y >= -5, "Bad", "Good"))
ggplot(Z, aes(x=Trial, color=Learner)) + geom_jitter(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(Learner~ID)
