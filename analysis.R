library(tidyverse)
library(ggplot2)
library(lme4)
library(lcmm)
library(lmerTest)
library(ggeffects)

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

Ext <- dplyr::filter(X, Phase == "Extinction", TimingCS == "Offset")
fit_choice <- glmer(ReliefChoice ~ (1|ID) + TrialCS * CSType + Anxiety, Ext, family=binomial(link="logit"))

fit_lcmm <- lcmm(ReliefChoice ~ TrialCS * CSType, subject='id',
                #  random = ~1,
                #  mixture = ~ TrialCS, classmb = ~STAI + DTS,
                 ng = 1, data = Ext %>% dplyr::select(TrialCS, ReliefChoice, id, CSType))

X <- read_csv("Expectancy.csv", col_types = cols(
        CSType = col_factor(c("CSmin1", "CSmin2", "CSplus")),
        Phase = col_factor(c("Pavlovian", "Extinction")),
        Time = col_factor(c("first", "last"))
    ))
dplyr::group_by(X, Phase, CSType, Time) %>% dplyr::summarize(mu = (mean(value)-1)/7)
