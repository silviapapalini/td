library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)

if(!require(gghalves)) {
    devtools::install_github('erocoar/gghalves')
    library(gghalves)
}

Expectancy <- read_csv(file="Expectancy.csv", col_types=cols(
        Phase=col_factor(c("Pavlovian","Extinction")),
        CSType=col_factor(c("CSplus", "CSmin1", "CSmin2")),
        Time=col_factor(c("first", "last"))))

is_outlier <- function(x) {
    r <- quantile(x,c(0.25, 0.75), na.rm=TRUE, type=7)
    return((x < r[1] - 1.5 * (r[2] - r[1])) | (x > r[2] + 1.5 * (r[2] - r[1])))
}

Expectancy <- Expectancy %>% dplyr::group_by(Phase, CSType, Time) %>%
    dplyr::mutate(outlier = ifelse(is_outlier(value), ID, NA)) %>%
    dplyr::ungroup()

p <- ggplot(Expectancy, aes(y=value, x=Time, fill=CSType,color=CSType)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, notch = FALSE) +
#  geom_half_violin(position = position_nudge(x = .2, y =0), alpha = .6, trim=F, side="r", adjust=2)+
  geom_point(aes(y = , color=CSType), position = position_jitterdodge(seed = 1), shape=21, size = 3, alpha = .8) +
#   geom_text_repel(aes(label=ID), position = position_jitterdodge(seed = 1)) +
  #geom_point(aes(y = , color=cs), position = position_jitter(width = .05), size = 3, alpha = .8) +
  geom_text(aes(label=outlier),na.rm = T,hjust=-0.3, position = position_jitterdodge(seed = 1)) +
#  geom_half_boxplot(width = .3, outlier.shape = NA, alpha = 0, notch = FALSE) +
#  stat_summary(fun= mean, geom = "point", shape = 25, size = 3, position=position_dodge(width=0.05)) +
  scale_y_continuous (limits = c(0,10),breaks=seq(0,10,1)) +
  ylab("Expectancy ratings") + xlab("Time") +
  facet_wrap(~Phase)
