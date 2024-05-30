library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(gtsummary)
library(rstatix)
library(sjstats)


setwd("C:/Users/u0121717/OneDrive - KU Leuven/Desktop/relief TD/github")
my_theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent", color="gray"),
  axis.text=element_text(size=rel(1.5), family="Arial"),
  axis.title=element_text(size=rel(1.5), family="Arial"),
  plot.title=element_text(size=rel(1.5), hjust = 0, family="Arial"),
  legend.text=element_text(size=rel(1.5), colour = "black", family="Arial"),
  legend.title=element_text(size=rel(1.5), face='bold', family="Arial"),
  strip.text = element_text(size=rel(1.5), family="Arial"),
  strip.background = element_rect(fill="transparent"))


if(!require(gghalves)) {
    devtools::install_github('erocoar/gghalves')
    library(gghalves)
}

Expectancy <- read_csv(file="Expectancy.csv", col_types=cols(
        Phase=col_factor(c("Pavlovian","Extinction")),
        CSType=col_factor(c("CSplus", "CSmin1", "CSmin2")),
        Time=col_factor(c("first", "last")))) %>%
        dplyr::filter(ID != "RS34")

is_outlier <- function(x) {
    r <- quantile(x,c(0.25, 0.75), na.rm=TRUE, type=7)
    return((x < r[1] - 1.5 * (r[2] - r[1])) | (x > r[2] + 1.5 * (r[2] - r[1])))
}

Expectancy <- Expectancy %>% dplyr::group_by(Phase, CSType, Time) %>%
    dplyr::mutate(outlier = ifelse(is_outlier(value), ID, NA)) %>%
    dplyr::ungroup()


Expectancy <- Expectancy %>% dplyr::mutate(value = 7 - value) # mutate the rating's value (flip)

  
p <- ggplot(Expectancy, aes(y=value, x=Time, fill=CSType)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, notch = FALSE) +
  scale_fill_brewer(palette="RdBu",name="CS Type", labels=c("CS+","CS1-","CS2-")) + 
  my_theme +theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 12),
    axis.ticks.length = unit(-0.05, "in"),
    axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
    axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
    axis.ticks.x = element_blank(),
    aspect.ratio = 1,
    legend.background = element_rect(color = "black", fill = "white")
  )+
  #geom_half_violin(position = position_dodge(width=.5), nudge=.0, alpha = .6, trim=F, side="r", adjust=2)+
  geom_point( position = position_jitterdodge(seed = 2), shape=21, size = 2, alpha = .8) +
  scale_y_continuous (limits = c(0,6),breaks=seq(0,10,1)) +
  ylab("US Expectancy Ratings") + xlab("Time") +
  facet_wrap(~Phase)
p
ggsave(file="Expectancies.pdf", p, width=10, height=10)

#PAVLOVIAN

Expectancy_PAV <- Expectancy %>% dplyr::filter(Phase == "Pavlovian")

#LMM exp Pavlovian
fit_exp_pav = lmerTest::lmer(value ~ (1|ID) + Time*CSType,data=Expectancy_PAV)
summary(fit_exp_pav)
AIC(fit_exp_pav)
residuals(fit_exp_pav)
plot(residuals(fit_exp_pav))
hist(residuals(fit_exp_pav), breaks = 80)
shapiro.test(residuals(fit_exp_pav))
#post hoc LM

#by CS
fit_exp_pav_csminus1 = lm(value ~ Time,data=Expectancy_PAV %>% dplyr::filter(CSType=="CSmin1"))
summary(fit_exp_pav_csminus1)
AIC(fit_exp_pav_csminus1)

fit_exp_pav_csminus2 = lm(value ~ Time,data=Expectancy_PAV %>% dplyr::filter(CSType=="CSmin2"))
summary(fit_exp_pav_csminus2)
AIC(fit_exp_pav_csminus2)

fit_exp_pav_csplus = lm(value ~ Time,data=Expectancy_PAV %>% dplyr::filter(CSType=="CSplus"))
summary(fit_exp_pav_csplus)
AIC(fit_exp_pav_csplus)


#anova
#PAV_aov2 <- aov(Expectancy_PAV$value ~ factor(Expectancy_PAV$CSType) * factor(Expectancy_PAV$Time), effect.size = "pes") 
#summary(PAV_aov2) 
#anova_stats(car::Anova(PAV_aov2, type = 3))

#EXTINCTION

Expectancy_EXT <- Expectancy %>% dplyr::filter(Phase == "Extinction")
shapiro.test(Expectancy_EXT$value)

#LMM exp Extinction
fit_exp_ext = lmerTest::lmer(value ~ (1|ID) + Time*CSType,data=Expectancy_EXT)
summary(fit_exp_ext)
AIC(fit_exp_ext)

#post hoc LM

#by CS
fit_exp_ext_csminus1 = lm(value ~ Time,data=Expectancy_EXT %>% dplyr::filter(CSType=="CSmin1"))
summary(fit_exp_ext_csminus1)
AIC(fit_exp_ext_csminus1)

fit_exp_ext_csminus2 = lm(value ~ Time,data=Expectancy_EXT %>% dplyr::filter(CSType=="CSmin2"))
summary(fit_exp_ext_csminus2)
AIC(fit_exp_ext_csminus2)

fit_exp_ext_csplus = lm(value ~ Time,data=Expectancy_EXT %>% dplyr::filter(CSType=="CSplus"))
summary(fit_exp_ext_csplus)
AIC(fit_exp_ext_csplus)

#anova
#EXT_aov2 <- aov(Expectancy_EXT$value ~ factor(Expectancy_EXT$CSType) * 
                 # factor(Expectancy_EXT$Time), effect.size = "pes") 
#summary(EXT_aov2) 
#effectsize::eta_squared(EXT_aov2)

