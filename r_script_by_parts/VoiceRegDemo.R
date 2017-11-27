# packages <- c('tuneR', 'seewave', 'gbm', 'xgboost')
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }
#install.packages("pbapply")
#install.packages("fftw")
setwd("/Users/MandyZhang/Desktop/DATS6101-group-project-2")
options(warn=-1)

# Function Definitions
checkValidArg <- function(arg) {
  if (!grepl("file=", arg)) {
    stop("Invalid Input. Expects: file=%filepath%")
  }
}

#check input arguements
args <- commandArgs(trailingOnly = TRUE);
fileArg <- args[1];
checkValidArg(fileArg)

# Run Recognition
load("models/rfModel.rda")
filepath = unlist(strsplit(fileArg, "="))[2]
print(paste("Running Recognition on: ", filepath))
source("r_script_by_parts/WavParser.R")
data <- data.frame()
row <- data.frame(filepath, 0, 0, 10)
#row <- data.frame("Track.wav", 0, 0, 10)
data <- rbind(data, row)
names(data) <- c('sound.files', 'selec', 'start', 'end')
setwd("data")
result <- specan3(data)
setwd("..")
print(result)

selected = c( "meanfun","IQR","Q25","sd", "sp.ent","sfm", "meanfreq", "label" )
acoustics = result$acoustics
sample = acoustics[ , names(acoustics) %in% selected]

sample.pred = predict(rfModel,sample, type = "response")
print("Voice Recognition Results: ")
print(sample.pred[[1]])

options(warn=0)
