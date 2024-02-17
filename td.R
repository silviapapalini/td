library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

trials <- factor(c("CSplus", "CSmin2", "CSmin2", "CSplus", "CSplus", "CSmin2", "CSmin1", "CSplus", "CSmin1", "CSmin2", "CSmin1", "CSplus", "CSmin2", "CSplus", "CSmin1", "CSplus", "CSmin1", "CSplus"))
trial_cs <- c(1,1,2,2,3,3,1,4,2,4,3,5,5,6,4,7,5,8)
# relief <- c(NA, 95, NA, 99, NA, 99, NA, 86, NA,89, NA, 97, NA, 99, 19, NA, 66, 77, NA, 56, 47, 61, NA, NA, NA, 35, NA, NA, NA, 26, NA, NA, NA,NA,NA,NA)
rewards <- c("CSplus" = 1, "CSmin1" = 1, "CSmin2" = 1)
initial <- c("CSplus" = 0, "CSmin1" = .8, "CSmin2" = .8)

S <- trials == "CSplus"
S <- seq(trials)
trials <- factor(trials[S])
rewards <- as.array(rewards[levels(trials), drop=FALSE])
initial <- as.array(initial[levels(trials), drop=FALSE])

temporal.difference <- function(trials, rewards, alpha.reward, gamma.reward, initial) {
    Qs <- array(dim = c(length(trials), length(rewards), 2),
                dimnames = list(trial = NULL, cs = names(rewards), timing = c("onset", "offset")))

    Es <- array(dim = c(length(trials), length(rewards), 2),
                dimnames = list(trial = NULL, cs = names(rewards), timing = c("onset", "offset")))

    Q.onset <- mean(initial)
    Q.offset <- initial

    for(i in seq_along(trials)) {
        trial.type <- as.character(trials[i])
        reward <- rewards[trial.type]

# V[t] <- V[t] + alpha * (r + gamma * V[t+1] - V[t])

        E.onset <- gamma.reward * mean(Q.offset) - Q.onset
        Q.onset <- Q.onset + alpha.reward * E.onset

        E.offset <- reward - Q.offset
        Q.offset <- Q.offset + alpha.reward * E.offset

        Qs[i, trial.type, "onset"] <- Q.onset
        Qs[i, trial.type, "offset"] <- Q.offset[trial.type]
        Es[i, trial.type, "onset"] <- E.onset
        Es[i, trial.type, "offset"] <- E.offset[trial.type]
    }

    list(Qs = Qs, Es = Es)
}

x <- lapply(c(.7, .8, .90, .94), function(gamma) {
    QE <- temporal.difference(trials, rewards, 0.7, gamma, initial)
    melt(QE$Qs)
})
names(x) <- c(.7, .8, .91, .94)
X <- dplyr::bind_rows(x, .id="gamma")

# ggplot(melt(QE$Qs), aes(x=trial, y=value, color=cs)) + geom_point() + facet_wrap(~timing)
ggplot(X, aes(x=trial, y=value, color=gamma)) + geom_point() + facet_wrap(~timing)
