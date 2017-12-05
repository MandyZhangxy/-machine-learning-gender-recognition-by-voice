library("stats")
library("ggplot2")
library("caret")

numeric = ifelse(train$label == 'male',1,0)
logitmodel<- glm(label~.,family = binomial(link =  "logit"), train,control = list(maxit = 50))
summary(logitmodel)

logitmodel2<- glm(label~IQR+sp.ent+sfm+meanfun+mode,family = binomial(link =  "logit"), train,control = list(maxit = 50))
summary(logitmodel2)

predictionWithClass_lg = predict(logitmodel2, test[-8],  type='response')
predictionWithClass_lg = ifelse(predictionWithClass_lg>0.5,1,0)
t = table(predictions= predictionWithClass_lg, actual = test$label)
t
# Accuracy metric/rate
sum(diag(t))/sum(t)

# ploting ROC curve and calculating AUC metric
pdf("../figures/logistic_roc.pdf", width = 16.5, height = 9.5)
plot(roc(test$label, predictionWithClass_lg), main = "AUC/ROC")
dev.off()

# plot confusion matrix
lg = predictionWithClass_lg
lg = factor(lg, levels = c(0,1), labels = c("female", "male"))
CM = confusionMatrix(lg, test$label)
print(CM)
cm = as.data.frame(CM$table)

pdf("../figures/logistic_confusion_matrix.pdf", width = 16.5, height = 9.5)
ggplot(data = cm, mapping = aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq), color = "white") + 
  scale_fill_gradient(low = "white", high = "#ffb5e9")  + 
  geom_text(aes(label = Freq),size = 7) + theme(legend.position = "none") + 
  ggtitle(paste("Confusion Matrix with Accuracy rate ", round(100*CM$overall[1],2), "%"))+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15))
dev.off()




