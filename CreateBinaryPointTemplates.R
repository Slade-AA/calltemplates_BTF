
# Load packages -----------------------------------------------------------

library(tidyverse)
library(monitoR) #used for making detections
library(seewave) #used for calculating acoustic features of detections


# Set-up ------------------------------------------------------------------



# Create Template ---------------------------------------------------------

templateCutoffValue <- 0.2 #set the value you are going to use for the template cutoff here - this will be used when making the template and also in 'eventeval'
windowLength <- 512 #set the value you are going to use for the template cutoff here - this will be used when making the template

templateCor <- makeCorTemplate(clip = templateFile,
                               frq.lim = c(0.7, 2.3), #frequency limits for template
                               score.cutoff = templateCutoffValue,
                               wl = windowLength,
                               name = "template_01")