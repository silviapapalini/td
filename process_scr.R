library(tidyverse)
library(signal)

find_onset <- function(x) {
    idx <- which(c(NA, diff(x$startstim)) == 1)
    ifelse(length(idx) > 0, idx, NA)
}
find_offset <- function(x) {
    idx <- which(c(NA, diff(x$shock + x$no_shock)) == 1)
    ifelse(length(idx) > 0, idx, NA)
}

calc_trial_number = function(CSmin1, CSmin2, CSplus) {
  csmin1 = ifelse(CSmin1, cumsum(CSmin1), 0)
  csmin2 = ifelse(CSmin2, cumsum(CSmin2), 0)
  csplus = ifelse(CSplus, cumsum(CSplus), 0)
  as.integer(csmin1 + csmin2 + csplus)
}

Fc <- c(0.0159, 5)
Fs <- 1000
bp <- butter(1, Fc / (Fs / 2), type = "pass")

filtfilt <- function(filt, x) {
    library(reticulate)
    scipy <- import("scipy.signal")
    scipy$filtfilt(filt$b, filt$a, x)
}

calculate_scr <- function(skin, start, stop, before, threshold = 0.01) {
    tryCatch(
        {
            baseline <- mean(skin[(start - before):start])
            scr <- max(skin[start:stop]) - baseline
            ifelse(scr < threshold, 0.0, scr)
        },
        error = function(e) NA
    )
}

process_trial_scr <- function(x) {
    sample_rate <- 1000
    onset <- find_onset(x)
    offset <- find_offset(x)
    onset_scr <- calculate_scr(x$skin_stream, onset, onset + 3 * sample_rate, 1 * sample_rate)
    offset_scr <- calculate_scr(x$skin_stream, offset, offset + 3 * sample_rate, 1 * sample_rate)
    data.frame(
        Onset_Time = onset,
        Offset_Time = offset,
        Onset_SCR = onset_scr,
        Offset_SCR = offset_scr
    )
}

process_scr <- function(fname) {
    print(fname)
    read_delim(fname) %>%
        dplyr::mutate(skin_stream = filtfilt(bp, skin_stream)) %>%
        # combine CSs into single CS column
        tidyr::unite(`T.CSM`,`T.CS_UU`, `T.CS_EE`, col = "CSType", remove=FALSE) %>%
        dplyr::rename(`CS-` = `T.CSM`,`CS--` = `T.CS_UU`, `CS+` = `T.CS_EE`) %>%
        tidyr::extract(`#TRIAL_POOL`, regex="_([0-9])[0-9]?", into="Phase") %>%
        dplyr::arrange(SUBJ_ID, Phase, time) %>%
        dplyr::mutate(Trial = cumsum(c(TRUE,diff(`#TRIAL_NUMBER`)))) %>%

        # calculate SCR
        dplyr::group_by(SUBJ_ID, Phase, Trial, CSType, `CS-`, `CS--`, `CS+`) %>%
            dplyr::group_modify( ~ process_trial_scr(.x) ) %>%
        dplyr::ungroup() %>%

        #calculate trial numbers per CS
        dplyr::group_by(SUBJ_ID, Phase) %>%
            dplyr::mutate(
                TrialCS = calc_trial_number(`CS-`, `CS--`, `CS+`),
                .keep = "unused") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            CSType = factor(CSType,
                levels = c("1_0_0", "0_1_0", "0_0_1"),
                labels = c("CSmin1","CSmin2", "CSplus")),
            Phase = factor(Phase, levels = c("1", "3"), labels = c("Pavlovian", "Extinction"))
        ) %>%
        tidyr::pivot_longer(c(Onset_Time, Offset_Time, Onset_SCR,Offset_SCR), names_to=c("TimingCS", ".value"), names_sep="_")
}

X <- do.call(rbind, lapply(list.files(path="./data", pattern="skin_RS[0-9]+.tab", full.names=TRUE), process_scr))
write.table(X, file='SCR.csv', quote=FALSE, row.names=FALSE, sep=",")

Y <- dplyr::filter(X, Phase == "Extinction") %>% dplyr::group_by(TimingCS, TrialCS, CSType) %>% dplyr::summarize(mu = mean(SCR, na.rm=TRUE))
ggplot(Y, aes(x=TrialCS, y=mu, color=CSType, group=CSType)) + geom_line() + facet_wrap(~TimingCS)
