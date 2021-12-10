
# Load packages -----------------------------------------------------------

library(tidyverse)

TemplatePerformance <- readRDS("outputs/TemplatePerformance.rds")

#Handling of edge cases for per recording performance measures:
#if TP + FN == 0 then Recall == NA
#if FP == 0 && TP == 0 then FDR == 0
#if TP + FP == 0 then Precision == 1
TemplatePerformance <- TemplatePerformance %>% mutate(Recall = ifelse(TruePos+FalseNeg == 0, NA, TruePos/(TruePos+FalseNeg)),
                                                      FDR = ifelse(FalsePos == 0 & TruePos == 0, 0, FalsePos/(TruePos+FalsePos)),
                                                      Precision = ifelse(TruePos+FalsePos == 0, 1, TruePos/(TruePos+FalsePos)))



#is.nan.data.frame <- function(x) {
#  do.call(cbind, lapply(x, is.nan))
#}

#TemplatePerformance[is.nan(TemplatePerformance)] <- NA

PerformanceSummary <- TemplatePerformance %>% 
  group_by(Template, ScoreCutoff) %>% 
  summarise(Recall = mean(Recall, na.rm = T),
            FDR = mean(FDR),
            Precision = mean(Precision))


ggplot(data = PerformanceSummary[PerformanceSummary$Template == '1,2,3',], aes(x = Recall, y = Precision)) + 
  geom_point() +
  scale_x_continuous(limits = c(0,1)) +
  theme_bw()