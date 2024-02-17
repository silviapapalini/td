temporal.difference <- function(cs, rewards, alpha, gamma, initial) {
    Q.onset <- mean(initial)
    Q.offset <- initial

    PE <- matrix(NA, nrow = length(cs), ncol = 2)

    for(i in seq_along(trials)) {
        trial.type <- cs[i]
        reward <- rewards[trial.type]

# V[t] <- V[t] + alpha * (r + gamma * V[t+1] - V[t])

        E.onset <- gamma.reward * mean(Q.offset) - Q.onset
        Q.onset <- Q.onset + alpha.reward * E.onset

        E.offset <- reward - Q.offset
        Q.offset <- Q.offset + alpha * E.offset

        PE[i,] <- c(E.onset, E.offset)
    }

    return(PE)
}

td <- function(ID, CSType, Relief, alpha, gamma, initV) {
    s <- split(1:length(ID), ID)
    PE <- matrix(NA, nrow = length(ID), ncol = 2)
    rewards <- rep(1, length(unique(CSType)))
    for(subj in s) {
        PE[s,] <- temporal.difference(cs[s], rewards, alpha, gamma, initV)
    }

    return(PE)
}
