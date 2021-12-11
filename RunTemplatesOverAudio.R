
# Load packages -----------------------------------------------------------

library(tidyverse)
library(monitoR) #used for making detections
library(seewave) #used for calculating acoustic features of detections


# Set-up ------------------------------------------------------------------

# load templates
templates <- readBinTemplates(dir = "./templates_BTF/binaryPoint/")

# list audio files to run templates over
audio_to_analyse <- list.files(path = "E:/BTF_Recordings/BTF_RoadNoise2020_44.1kHz", 
                               pattern = "*.wav$", full.names = TRUE)

# labelled data to assess template performance
labelledData <- list.files(path = "selectionTables_BTF/2020_RoadNoiseSurveys",
                           pattern = "*.txt$", full.names = TRUE)

# subset audio to match those we have selection tables for
audio_to_analyse <- audio_to_analyse[gsub("wav", "", basename(audio_to_analyse)) %in% gsub("Table.*", "", basename(labelledData))]


# Data frame for results --------------------------------------------------

#create an empty dataframe to store results
TemplatePerformance <- data.frame(Recording = character(),
                                  Template = character(),
                                  ScoreCutoff = numeric(),
                                  #WindowLength = numeric(),
                                  TruePos = numeric(),
                                  TrueNeg = numeric(),
                                  FalsePos = numeric(),
                                  FalseNeg = numeric())


# Run templates over audio ------------------------------------------------

#TemplateDetections <- list()
for (n in (1:length(audio_to_analyse))[11:49]) {
  
  #run template over audio, find peaks, and extract detections
  scores <- binMatch(survey = audio_to_analyse[n],
                     templates = templates,
                     show.prog = FALSE)
  peaks <- findPeaks(scores)
  detections <- getDetections(peaks)
  
  
  templateCombinations <- combn(1:length(templates@templates), 3, simplify = F)
  
  # align detections of multiple templates
  alignedDetections <- data.frame(template = character(),
                                  date.time = numeric(),
                                  time = numeric(),
                                  score = numeric())
  for (combination in templateCombinations) {
    temp <- timeAlign(x = peaks[combination], tol = 0.672, what = "detections")
    temp$template <- as.character(paste(combination, collapse = ","))
    
    alignedDetections <- rbind(alignedDetections, temp)
  }
  
  alldetections <- rbind(detections, alignedDetections)
  alldetections$template <- factor(alldetections$template)
  
  
  #read in selection table and rename columns to be useable with the 'eventEval' function
  selectionTable <- read.table(labelledData[n], sep = "\t", header = TRUE)
  
  colnames(selectionTable)[which(colnames(selectionTable) == "Begin.Time..s.")] <- "start.time"
  colnames(selectionTable)[which(colnames(selectionTable) == "End.Time..s.")] <- "end.time"
  colnames(selectionTable)[which(colnames(selectionTable) == "Low.Freq..Hz.")] <- "min.frq"
  colnames(selectionTable)[which(colnames(selectionTable) == "High.Freq..Hz.")] <- "max.frq"
  
  if (nrow(selectionTable) == 0) {
    selectionTable[1,] <- c(1, 1, 1, -10, -10, 0, 0, 'Neg')
    selectionTable$start.time <- as.numeric(selectionTable$start.time)
    selectionTable$end.time <- as.numeric(selectionTable$end.time)
    selectionTable$name <- "Neg"
  } else {
    selectionTable$name <- "BTF"
  }
  
  scoreCutoffs <- seq(2, 20, 1)
  for (template in levels(alldetections$template)) {
    for (score in scoreCutoffs) {
      #evaluate detections using eventEval
      evaluation <- eventEval(detections = alldetections[alldetections$template == template,],
                              standard = selectionTable,
                              score.cutoff = score,
                              tol = 0.6)
      evaluation <- evaluation[evaluation$template != "Neg", ]
      
      detectionSummary <- summary(factor(evaluation$outcome, levels = c('TRUE +', 'TRUE -', 'FALSE +', 'FALSE -')))
      
      #if there were no detections set 'FALSE -' value to number of rows in 'selectionTable'
      if (nrow(detections) == 0) {
        detectionSummary['FALSE -'] <- nrow(selectionTable)
      }
      
      TemplatePerformance <- rbind(TemplatePerformance,
                                   data.frame(Recording = audio_to_analyse[n],
                                              Template = template,
                                              ScoreCutoff = score,
                                              #WindowLength = windowLength,
                                              TruePos = detectionSummary[['TRUE +']],
                                              TrueNeg = detectionSummary[['TRUE -']],
                                              FalsePos = detectionSummary[['FALSE +']],
                                              FalseNeg = detectionSummary[['FALSE -']]))
    }
  }
}

saveRDS(TemplatePerformance, "outputs/TemplatePerformance.rds")

save(templates, TemplatePerformance, file = paste0("outputs/backups/", Sys.Date(), "_templates&performance.RData"))


# Wrap-up -----------------------------------------------------------------

# clear environment
rm(list = ls())

# run script to plot performance metrics - also creates new readme
source("scripts/PlotTemplatePerformance.R")