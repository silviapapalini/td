library(tidyverse)

Sys.setlocale("LC_ALL", "C")

# CB   CS_UU   CS_EE   CS_MIN
#  1     red    blue   yellow
#  2    blue     red   yellow
#  3  yellow     red     blue
#  4     red  yellow     blue
#  5  yellow    blue      red
#  6    blue  yellow      red

calc_trial_number = function(CSmin1, CSmin2, CSplus) {
  csmin1 = ifelse(CSmin1, cumsum(CSmin1), 0)
  csmin2 = ifelse(CSmin2, cumsum(CSmin2), 0)
  csplus = ifelse(CSplus, cumsum(CSplus), 0)
  as.integer(csmin1 + csmin2 + csplus)
}

STAI <- read_excel("ReliefShiftData.xlsx", sheet="STAI") %>%
    dplyr::rename(ID = SUBJ_ID, STAI = `...23`) %>%
    dplyr::select(ID, STAI)

DTS <- read_excel("ReliefShiftData.xlsx", sheet="DTS") %>%
    dplyr::rename(ID = SUBJ_ID, DTS = `...18`) %>%
    dplyr::select(ID, DTS)

PANAS <- read_excel("ReliefShiftData.xlsx", sheet="PANAS") %>%
    dplyr::rename(ID = SUBJ_ID) %>%
    dplyr::select(ID, PANASp, PANASn)

X <- dplyr::bind_rows(
    CS = read_delim("data/response_Pav_Ext_CS.tab") %>%
        dplyr::rename(ReliefRating = ReliefCS, ReliefChoice = ReliefChoiceCS),
    US = read_delim("data/response_Pav_Ext_US.tab") %>%
        dplyr::rename(ReliefRating = ReliefUS, ReliefChoice = ReliefChoiceUS),
    .id = "TimingCS") %>%
    # combine CSs into single CS column
    tidyr::unite(`T.CSM`,`T.CS_UU`, `T.CS_EE`, col = "CSType", remove=FALSE) %>%
    dplyr::rename(`CS-` = `T.CSM`,`CS--` = `T.CS_UU`, `CS+` = `T.CS_EE`) %>%
    dplyr::rename(ID = SUBJ_ID, Phase = `#TRIAL_FLOW`) %>%
    dplyr::arrange(ID, Phase, trial_counter) %>%
    # calculate trial numbers per CS
    dplyr::group_by(ID, Phase, TimingCS) %>%
        dplyr::mutate(
            TrialCS = calc_trial_number(`CS-`, `CS--`, `CS+`),
            `Trial#` = trial_counter - min(trial_counter) + 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        Phase = gsub(x = Phase, pattern = "_phase", replacement = ""),
        ReliefChoice = as.integer(ReliefChoice - 1),
        ReliefRating = ifelse(ReliefChoice == 1, ReliefRating, NA),
        CSType = factor(CSType,
            levels = c("1_0_0", "0_1_0", "0_0_1"),
            labels = c("CSmin1","CSmin2", "CSplus")),
        TimingCS = factor(TimingCS, levels = c("CS", "US")),
        Phase = factor(Phase, levels = c("Pavlovian", "Extinction")),
    ) %>%
    dplyr::mutate(
        ReliefChoice = ifelse(shock & (TimingCS == "US"), NA_integer_, ReliefChoice),
        ReliefRating = ifelse(shock & (TimingCS == "US"), NA_real_, ReliefRating),
    ) %>%
    dplyr::arrange(ID, Phase, `Trial#`) %>%
    dplyr::relocate(ID, Phase, `Trial#`, TrialCS, ReliefChoice, ReliefRating, `CS+`, `CS-`, `CS--`, CSType, TimingCS, shock) %>%
    # remove unused
    dplyr::select(-trial_counter)

X <- purrr::reduce(list(X, STAI, DTS, PANAS), dplyr::left_join, by = "ID") %>%
    (function(X) {write.table(X, file="Relief.csv", sep=",", row.names=FALSE, quote=FALSE); X})

