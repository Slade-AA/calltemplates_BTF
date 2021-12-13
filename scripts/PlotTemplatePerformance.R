
# Load packages -----------------------------------------------------------

library(tidyverse)

TemplatePerformance <- readRDS("outputs/TemplatePerformance.rds")


# Template performance per recording --------------------------------------

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


# Performance over all recordings -----------------------------------------

PerformanceSummary <- TemplatePerformance %>% 
  group_by(Template, ScoreCutoff) %>%
  summarise(TruePos = sum(TruePos),
            TrueNeg = sum(TrueNeg),
            FalsePos = sum(FalsePos),
            FalseNeg = sum(FalseNeg)) %>% 
  mutate(Recall = TruePos/(TruePos+FalseNeg),
         FDR = FalsePos/(TruePos+FalsePos),
         Precision = TruePos/(TruePos+FalsePos))

individualTemplates <- grep("template*", levels(PerformanceSummary$Template), value = TRUE)

Plot_SingleTemplates <- ggplot(data = PerformanceSummary[PerformanceSummary$Template %in% individualTemplates,], aes(x = Recall, y = Precision, 
                                                                                                                     group = Template, colour = Template)) + 
  geom_line(lwd = 1) +
  scale_x_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 3))

ggsave(filename = "outputs/figures/Plot_2018_SingleTemplates.png",
       plot = Plot_SingleTemplates,
       height = 15, width = 20, units = "cm", dpi = 800)


combinationTemplates <- grep("template*", levels(PerformanceSummary$Template), value = TRUE, invert = TRUE)

Plot_MultipleTemplates <- ggplot(data = PerformanceSummary[PerformanceSummary$Template %in% combinationTemplates,], aes(x = Recall, y = Precision, 
                                                                                                                        group = Template, colour = Template)) + 
  geom_line(lwd = 1) +
  scale_x_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 3))

ggsave(filename = "outputs/figures/Plot_2018_MultipleTemplates.png",
       plot = Plot_MultipleTemplates,
       height = 15, width = 20, units = "cm", dpi = 800)