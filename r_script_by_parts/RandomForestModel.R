install.packages("caTools")
install.packages("caret")
install.packages("e1071")
install.packages("pROC")
install.packages("randomForest", dependencies = FALSE)
library(randomForest)

# loading data
setwd("/Users/jahuang/mandyproject/machine-learning-gender-recognition-by-voice/data")
scaled_voice = read.csv("../data/scaled_voice.csv")
head(scaled_voice)
str(scaled_voice)

####
#### Random Forest Model Prediction
####

# Seperating training and testing dataset: 
# randomly select 70% train and 30% test groups:
library(caTools)
ind = sample.split(Y = scaled_voice$label, SplitRatio = 0.7)
train = scaled_voice[ind, ]
test = scaled_voice[!ind, ]

#high strength of tree = low error rate of individual tree classifier
# mtry = number of variabels selected at each split.
# default(
# Regression = floor(number of variables / 3)
# categorical = floor(sqrt(no. of indipendnet variabels))
#)
# lower mtry means Less correlation betwee trees (good thing); decreases strength of each tree (bad thing)
# so you need to optimized between these good and bad by optimzing

#ntree = no. of trees to grow
#nodesize = minimun size of terminal nodes
#(larger the number smaller the trees)

# Fitting the model:

# to find the best mtry:
bestmry = tuneRF(x = train[, -21], train$label, 
                 ntreeTry = 300, stepFactor = 1.5, improve = 0.001,
                 trace = TRUE, plot = TRUE, importance = TRUE)

bestmry = data.frame(bestmry)
bestmry = bestmry$mtry[which.min(bestmry$OOBError)]
bestmry

trainingmodel = randomForest(label~., data = train, mtry = bestmry, ntree = 350)

# forest error rate 
# OOB(out of bag rate)/misclassification rate
# each tree is tested on 1/3rd of the number of observations
# not used in building the tree

#1. high strength of tree will have lower error (depends on mtry)
#2 hight correlation between trees increases the error(depends on mtry)

# plot hte importance of each vairables:
# mean decrease accuracy = how much of the model accuracy decreases
# if we drop taht vairbale, high value of mean decrease accuracy or mean decrease gini score
# higher the importance of the variable in the model.

importance = data.frame(importance(trainingmodel))
variables = rownames(importance)
importance$variables = variables

pdf("../figures/importance.pdf", width = 16.5, height = 9.5)
varImpPlot(trainingmodel)
dev.off()

library("ggplot2")
pdf("../figures/ggplotimportance.pdf", width = 16.5, height = 9.5)
ggplot(data = importance, aes(
  x = reorder(variables, -MeanDecreaseGini),
  y = MeanDecreaseGini)) + geom_bar(stat = 'identity',fill = "#FF6666",alpha=0.75) + 
  theme_light() + xlab("Variabels")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = round(MeanDecreaseGini)), vjust=-0.3, size = 3)+
  ggtitle("Importance of Variables in descending order") + 
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15))

dev.off()

# retraining model with selected variabels:
selected = importance$variables[order(importance$MeanDecreaseGini, decreasing = T)[1:7]]
selected = append(selected, "label")
train = train[ , names(train) %in% selected]
test = test[ , names(test) %in% selected]
trainingmodel = randomForest(label~., data = train, mtry = bestmry, ntree = 350)  

# save data with selected variables for other models: KNN and Logistic regression:
write.csv(test, file = "test.csv",row.names = FALSE)
write.csv(train, file = "train.csv")
 
# predictions:
predictionWithClass = predict(trainingmodel, test, type = "class")
t = table(predictions = predictionWithClass, actual = test$label)
t
# Accuracy metric/rate
sum(diag(t))/sum(t)


# ploting ROC curve and calculating AUC metric
library("pROC")
predictionWithProbs = predict(trainingmodel, test, type = "prob")

pdf("../figures/roc_curve.pdf", width = 10.5, height = 9.5)
plot(roc(test$label, predictionWithProbs[ ,2]), main = "AUC/ROC")
dev.off()


# ploting confusion matrix in ggplot:
library("caret")
CM = confusionMatrix(predictionWithClass, test$label)
cm = as.data.frame(CM$table)
pdf("../figures/confusion_matrix.pdf", width = 16.5, height = 9.5)
ggplot(data = cm, mapping = aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq), color = "white") + 
  scale_fill_gradient(low = "white", high = "#ffb5e9")  + 
  geom_text(aes(label = Freq),size = 7) + theme(legend.position = "none") + 
  ggtitle(paste("Confusion Matrix with Accuracy rate ", round(100*CM$overall[1],2), "%"))+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15))
dev.off()

### Save Training Model For Later
rfModel = trainingmodel;
save(rfModel, file = "../models/rfModel.rda");

### Demo of recognition during presentation:
# sample is the data recorded in class.
# Want to know if the voice is male or female.
sample = test[1,-8]
sample.pred = predict(trainingmodel,sample, type = "response")
sample.pred


sample2 = test[476,]
sample.pred = predict(trainingmodel,sample2, type = "response")
sample.pred


which(test[,8] == "female")









