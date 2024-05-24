library(tidyverse)
library(ggplot2)

X <- read_csv("Relief.csv") %>% dplyr::mutate(Relief = ifelse(is.na(ReliefRating), 0, ReliefRating))
Y <- dplyr::filter(X, Phase == "Extinction") %>%
    dplyr::mutate(
        CSType=recode_factor(CSType, "CSmin1"="CSmin", "CSmin2"="CSmin")) %>%
    dplyr::group_by(TimingCS, TrialCS, CSType, Anxiety) %>%
    dplyr::summarize(
        mu_only = mean(ReliefRating, na.rm=TRUE),
        mu = mean(Relief, na.rm=TRUE),
        prop_yes = mean(ReliefChoice, na.rm=TRUE))

p <- ggplot(Y, aes(x=TrialCS, y=mu, group=interaction(Anxiety,CSType), color=CSType, linetype=Anxiety)) +
    geom_line(linewidth=1) +
    #xlim(c(1,5)) +
    ylim(c(0,100)) +
    ggtitle("Fear Extinction") +
    facet_grid(~TimingCS, labeller = as_labeller(c("Onset" = "CS Onset", "Offset" = "CS Offset"))) +
    scale_color_manual(labels = c(CSmin="CS-", CSplus="CS+"), values = c(CSmin = "#7f7f7f", CSplus = "#61a3bf")) +
    theme(
        legend.text=element_text(size=10),
        legend.title=element_text(size=12, face='bold')) +
    ylab("Average Magnitude Relief") +
    guides(color=guide_legend("CS"), linetype=guide_legend("Anxiety (STAI)")) +
    facet_wrap(~TimingCS)

q <- ggplot(Y, aes(x=TrialCS, y=prop_yes, group=interaction(Anxiety,CSType), color=CSType, linetype=Anxiety)) +
    geom_line(linewidth=1) +
    #xlim(c(1,5)) +
    ggtitle("Fear Extinction") +
    facet_grid(~TimingCS, labeller = as_labeller(c("Onset" = "CS Onset", "Offset" = "CS Offset"))) +
    scale_color_manual(labels = c(CSmin="CS-", CSplus="CS+"), values = c(CSmin = "#7f7f7f", CSplus = "#61a3bf")) +
    theme(
        legend.text=element_text(size=10),
        legend.title=element_text(size=12, face='bold')) +
    ylab("Average Choice") +
    guides(color=guide_legend("CS"), linetype=guide_legend("Anxiety (STAI)"))


ggplot(X %>% dplyr::filter(Phase == "Extinction", TimingCS == "US") %>% dplyr::mutate(ReliefRating = ifelse(is.na(ReliefRating), 0, ReliefRating)), aes(x=TrialCS, y=ReliefRating, group=CSType, color=CSType)) + geom_line() + facet_wrap(Anxiety~ID)


Y <- dplyr::filter(X, Phase == "Extinction") %>% dplyr::group_by(`CS+`, TimingCS, TrialCS) %>% dplyr::summarize(mu=mean(ReliefRating,na.rm=TRUE), p=mean(ReliefChoice, na.rm=TRUE))
ggplot(X, aes(x=TrialCS, y=ReliefRating, color=TimingCS)) + geom_point() + ylim(c(0,100)) + facet_wrap(TimingCS~`CS+`)
ggplot(Y, aes(x=TrialCS, y=p, color=TimingCS)) + geom_line() + ylim(c(0,1)) + facet_wrap(~`CS+`)
