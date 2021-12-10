
# Load packages -----------------------------------------------------------

library(tidyverse)
library(monitoR) #used for making detections
library(seewave) #used for calculating acoustic features of detections


# Set-up ------------------------------------------------------------------

audioDirectory <- "E:/BTF_Recordings/BTF_RoadNoise2020_44.1kHz"

templateDirectory <- "templates_BTF/binaryPoint"

# load templates

# list audio files to run templates over
audio_to_analyse <- list.files(path = audioDirectory, pattern = "*.wav$", full.names = TRUE)


# Run templates over audio ------------------------------------------------

TemplateDetections <- list()
for (n in 1:length(audio_to_analyse)) {
  
  #loop over each collection of templates - may be different template settings (e.g. window length), or different species
  for (templateSet in 1:length(templatesToRun)) {
    
    Template_Info <- data.frame(Template = basename(names(templatesToRun[[templateSet]]@templates)))
    Template_Info$Duration <- sapply(templatesToRun[[templateSet]]@templates, function(x) x@duration)
    
    #Calculate scores and find peaks
    cscores <- binMatch(survey = audio_to_analyse[[n]], #gsub("*.flac$", ".wav", audio_to_analyse[[n]])
                        templates = templatesToRun[[templateSet]],
                        quiet = TRUE, #supresses status of which template being matched
                        time.source = "filename", #fileinfo has wrong datetime but supresses warning message, use filename when named correctly
                        write.wav = FALSE) #need to write wave file when using 'wave object'
    
    peaks <- findPeaks(cscores)
    
    #Create 'for loop' to save detections at different cut-off values
    scoreCutoffs <- seq(2, 20, 1)
    for (cutoff in scoreCutoffs) {
      templateCutoff(peaks) <- rep(cutoff, length(templatesToRun[[templateSet]]@templates))
      
      
      #Check for no detections which can cause errors
      if (max(sapply(peaks@detections, function(x) nrow(x))) == 0) { #if no detections, move on
        next
      }
      
      
      detections <- getDetections(peaks)
      
      detections$template <- basename(detections$template)
      detections$template <- factor(detections$template)
      
      detections$Recording <- basename(audio_to_analyse[[n]])
      detections$cutoff <- cutoff
      
      TemplateDetections[[paste0(basename(audio_to_analyse[[n]]), "_", cutoff, "_indtemplates")]] <- detections
      
      #Combine detections from templates
      
      if (sum(sapply(peaks@detections, function(x) nrow(x)) > 1) > 1) { #if a max of one detection per template
        #merge peaks within 0.4
        alignedDetections <- timeAlign(x = peaks, tol = templateTolerance, what = "detections")
        
        alignedDetections$template <- "combined"
        alignedDetections$template <- factor(alignedDetections$template)
      } else {
        #timeAlign fails with only 1 template having a detection and if multiple templates have only a max of 1 detection
        
        alignedDetections <- do.call(rbind, peaks@detections)
        alignedDetections <- rownames_to_column(alignedDetections, var = "template")
        alignedDetections <- alignedDetections[order(alignedDetections$time),] 
        
        d <- c(NA, diff(alignedDetections$time, differences = 1))
        
        tolerance <- templateTolerance
        
        if (min(d, na.rm = TRUE) < tolerance) {
          alignedDetections <- alignedDetections[-c(which(d < tolerance)[which(alignedDetections$score[which(d < tolerance)] - alignedDetections$score[which(d < tolerance)-1] < 0)],
                                                    (which(d < tolerance)-1)[which(alignedDetections$score[which(d < tolerance)] - alignedDetections$score[which(d < tolerance)-1] > 0)]),]
          
        }
      }
      
      alignedDetections$template <- "combined"
      alignedDetections$template <- factor(alignedDetections$template)
      
      alignedDetections$Recording <- basename(audio_to_analyse[[n]])
      alignedDetections$cutoff <- cutoff
      
      TemplateDetections[[paste0(basename(audio_to_analyse[[n]]), "_", cutoff, "_alignedtemplates")]] <- alignedDetections
    }
  }
}


TemplateDetections <- do.call(rbind, TemplateDetections)
rownames(TemplateDetections) <- NULL

saveRDS(TemplateDetections, file = paste0(getwd(), "/_TestAudio_Wambiana_45-178_Labelled_New/TemplateDetections_Lrubella.rds"))

