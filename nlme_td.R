library(nlme)
library(abind)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(dplyr)
library(patchwork)
library(ggeffects)
library(readr)
library(lme4)
library(lmerTest)
library(optimx)
library(hrbrthemes)
library(RESI)

setwd("C:/Users/u0121717/OneDrive - KU Leuven/Desktop/relief TD/github")

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

## HERE THE TD for the CS+

Ext <- dplyr::filter(X, ID != "RS34", Phase == "Extinction") %>%
     dplyr::arrange(ID, TrialCS, TimingCS) %>% dplyr::mutate(
       DTS = scale(DTS), STAI = scale(STAI),
       PANASp = scale(PANASp), PANASn = scale(PANASn),
       Distress = factor(ifelse(DTS >= median(DTS), 0, 1), levels=c(0,1), labels=c("Low", "High"))
     )

Ext_plus <- dplyr::filter(Ext, CSType == "CSplus")

resVar <- function(object) {
  rss <- sum(residuals(object)^2)
  n <- length(residuals(object))
  p <- length(coef(object))
  rss/(n - p)
}

# estimate learning with linear model fit. Slope >= -x => Good learner
good_bad_learning <- function(Ext) {
  estimate_learning <- function(S) {
    fit <- lm(Relief ~ TrialCS, data=S)
    pred <- predict(fit)
    cbind(as.data.frame(t(c(coef(fit), var = resVar(fit)))),
          Trial = S$Trial, TimingCS = S$TimingCS, pred = pred
    )
  }

  ols <- Ext %>% dplyr::group_by(ID) %>% dplyr::filter(TimingCS == "Offset") %>% dplyr::group_modify(~ estimate_learning(.x), .keep=TRUE)

  Z <- dplyr::left_join(Ext, ols, by = c("ID","Trial", "TimingCS"))
  Z <- Z %>% dplyr::filter(TimingCS == "Offset") %>% dplyr::mutate(Learner = ifelse(TrialCS.y >= -1, "Bad", "Good"))
  Z
}

Z <- good_bad_learning(Ext_plus)
ggplot(Z, aes(x=TrialCS.x, color=Learner)) + geom_jitter(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(Learner~ID)

Good <- unique((Z %>% dplyr::filter(Learner == "Good"))$ID)
Ext_plus <- dplyr::mutate(Ext_plus, selected = (ID %in% Good)) %>% dplyr::filter(selected)

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


#check number participants
dplyr::n_distinct(Ext_plus$ID)

# needs to be in the correct order
Ext_plus <- dplyr::arrange(Ext_plus, ID, TrialCS, TimingCS)

fit <- nlme(
    Relief ~ td(ID, Trial, alpha, gamma, init.onset, init.offset),
    data = Ext_plus,
    #fixed = alpha + gamma + init.onset + init.offset ~ 1,
    fixed = list(alpha ~ 1 + PANASp, gamma ~ 1 + PANASp, init.onset + init.offset ~ 1),
    random = pdDiag(alpha + gamma ~ 1),
    # random = pdLogChol(diag(1:2), nam=c("alpha", "gamma"), alpha + gamma ~ 1),
    groups = ~ ID,
    #start = c( alpha = .7, gamma = .9, init.onset = .5, init.offset = .2),
    start = c(.7, 0.0, .9, 0.0, .5, .2),
    # verbose = TRUE,
    control = nlmeControl(maxIter = 100, msMaxIter = 1000)
)
summary(fit)
TD_summary = summary(fit)


# adding random effects on init.onset and init.offset
# =>  BIC = 5744.336 and estimated stdev ~ 0

Ext_plus$pred <- predict(fit)
ggplot(Ext_plus, aes(x=TrialCS, color=TimingCS)) + geom_point(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(~ID)

# plotting
ff <- fixed.effects(fit)
r <- function(x) rep(x, nrow(S))
pred <- td(S$ID, S$TrialCS, r(ff["alpha"]), r(ff["gamma"]), r(ff["init.onset"]), r(ff["init.offset"]))
S$fixed <- pred

Q <- S %>% dplyr::group_by(TrialCS, TimingCS) %>% summarise_at(vars(Relief, pred),
               list(Q1=~quantile(., probs = 0.025),
                    median=median, Q3=~quantile(., probs = 0.975)))
ggplot(S, aes(x=TrialCS, color=TimingCS)) +
    geom_jitter(aes(y=Relief), wi) +
    geom_line(aes(y=fixed), color="black") +
    facet_wrap(~TimingCS) + geom_ribbon(aes(ymin=pred_Q1,ymax=pred_Q3), alpha=.3, data=Q)

#q <- ggpredict(fit, terms=c("TrialCS", "CSType")) %>% as.data.frame()%>% dplyr::rename(TrialCS=x, TimingCS=group)

# end plotting

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

#fits <- Ext %>% dplyr::group_by(ID) %>% dplyr::group_modify(~ estimate_subject(.x), .keep=TRUE)
#Z <- dplyr::left_join(Ext, fits, by = c("ID","Trial", "TimingCS"))
#ggplot(Z, aes(x=Trial, color=TimingCS)) + geom_jitter(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(Anxiety~ID)


## HERE THE TD for the CSmin2 and CSmin1 merged

# Ext_min <- Ext %>%
#   #dplyr::mutate(CSType=recode_factor(CSType, "CSmin1"="CSmin", "CSmin2"="CSmin")) %>%
#   dplyr::filter(CSType == "CSmin2") %>%
#   dplyr::group_by(ID, TimingCS, TrialCS) %>%
#   dplyr::summarize(
#     Trial = first(Trial),
#     ReliefRating = mean(ReliefRating, na.rm=TRUE),
#     Relief = mean(Relief, na.rm=TRUE),
#     ReliefChoice = mean(ReliefChoice, na.rm=TRUE),
#     PANASp = mean(PANASp, na.rm=TRUE),
#   ) %>% dplyr::ungroup()

Ext_min <- Ext %>%
  dplyr::mutate(CSType=recode_factor(CSType, "CSmin1"="CSmin", "CSmin2"="CSmin")) %>%
  dplyr::filter(CSType == "CSmin") %>%
  dplyr::group_by(ID, TimingCS, TrialCS, CSType) %>%
  dplyr::summarize(
    Trial = first(Trial),
    across(!Trial & !where(is.numeric), ~ first(.)),
    across(!Trial & where(is.numeric), ~ mean(., na.rm=TRUE))
  )

# comment the next 3 lines to use the good/bad from CSplus
Z <- good_bad_learning(Ext_min)
ggplot(Z, aes(x=TrialCS.x, color=Learner)) + geom_jitter(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(Learner~ID)
Good <- unique((Z %>% dplyr::filter(Learner == "Good"))$ID)

Ext_min <- dplyr::mutate(Ext_min, selected = (ID %in% Good)) %>% dplyr::filter(selected)

#check number participants
dplyr::n_distinct(Ext_min$ID)

# needs to be in the correct order
Ext_min <- dplyr::arrange(Ext_min, ID, TrialCS, TimingCS)

fit <- nlme(
  Relief ~ td(ID, Trial, alpha, gamma, init.onset, init.offset),
  data = Ext_min,
  #fixed = alpha + gamma + init.onset + init.offset ~ 1,
  fixed = list(alpha ~ 1 + PANASp, gamma ~ 1 + PANASp, init.onset + init.offset ~ 1),
  random = pdDiag(alpha + gamma ~ 1),
  groups = ~ ID,
  #start = c( alpha = .7, gamma = .9, init.onset = .5, init.offset = .8),
  start = c(.7, 0.0, .9, 0.0, .5, .8),
  # verbose = TRUE,
  control = nlmeControl(maxIter=100)
)
summary(fit)
TD_summary = summary(fit)


Ext_min$pred <- predict(fit)
ggplot(Ext_min, aes(x=TrialCS, color=TimingCS)) + geom_point(aes(y=Relief)) + geom_line(aes(y=pred)) + facet_wrap(~ID)

# plotting

q <- ggpredict(fit, terms=c("TrialCS", "CSType")) %>% as.data.frame()%>% dplyr::rename(TrialCS=x, TimingCS=group)


# end plotting
