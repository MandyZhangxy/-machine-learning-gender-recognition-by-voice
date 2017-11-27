# packages <- c('tuneR', 'seewave', 'gbm', 'xgboost')
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }
# install.packages("pbapply", repos = "https://cran.rstudio.com/bin/macosx/mavericks/contrib/3.3/pbapply_1.3-3.tgz")
# install.packages("fftw", repos = 'https://cran.rstudio.com/bin/macosx/mavericks/contrib/3.3/fftw_1.0-4.tgz')
setwd("/Users/jahuang/mandyproject/machine-learning-gender-recognition-by-voice")
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
