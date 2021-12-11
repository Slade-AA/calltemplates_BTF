
# Load packages -----------------------------------------------------------

library(monitoR) #used for making detections


# Set-up ------------------------------------------------------------------

templateFiles <- list.files(path = "./templates_BTF/rawAudio",
                            pattern = "*.wav",
                            full.names = TRUE)

# Create Template ---------------------------------------------------------

templateCutoffValue <- 2 #set the value you are going to use for the template cutoff here - this will be used when making the template and also in 'eventeval'
windowLength <- 512 #set the value you are going to use for the template cutoff here - this will be used when making the template
bufferValue <- 1


# loop through audio and create templates
templateBin <- list()
for (templateFile in 1:length(templateFiles)) {
  
  templateName <- paste0("template_", 
                         sprintf("%02d", templateFile), 
                         "_buffer", bufferValue,
                         "_wl", windowLength)
  
  templateBin[[templateFile]] <- makeBinTemplate(clip = templateFiles[templateFile],
                                                 frq.lim = c(1.8, 4), #frequency limits for template
                                                 score.cutoff = templateCutoffValue,
                                                 wl = windowLength,
                                                 select = "rectangle",
                                                 buffer = bufferValue,
                                                 name = templateName)
  
  png(filename = paste0("./templates_BTF/binaryPoint/", templateName, ".png"))
  plot(templateBin[[templateFile]])
  dev.off()
}


# Write templates to file -------------------------------------------------

templates <- do.call(combineBinTemplates, templateBin)

writeBinTemplates(templates,
                  dir = "./templates_BTF/binaryPoint/")