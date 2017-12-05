library("class")
train = read.csv("../data/train.csv")
voice = read.csv("../data/voice.csv")
test = read.csv("../data/test.csv")
knn_pred = knn(train = train[, -8], test = test[ ,-8], cl=train[, "label"], k=7, prob = TRUE)

#plot ROC
pdf("../figures/knn_roc.pdf", width = 16.5, height = 9.5)
plot(roc(test$label,as.numeric(knn_pred)), main = "KNN AUC/ROC")
dev.off()


print("The accuracy rate in KNN is:")
sum(knn_pred == test[, 8])/length(test[,8])

CM = confusionMatrix(knn_pred, test$label)
print(CM)
cm = as.data.frame(CM$table)

pdf("../figures/knn_confusion_matrix.pdf", width = 16.5, height = 9.5)
ggplot(data = cm, mapping = aes(x = Reference, y = Prediction)) + 
  geom_tile(aes(fill = Freq), color = "white") + 
  scale_fill_gradient(low = "white", high = "#ffb5e9")  + 
  geom_text(aes(label = Freq),size = 7) + theme(legend.position = "none") + 
  ggtitle(paste("Confusion Matrix with Accuracy rate ", round(100*CM$overall[1],2), "%"))+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15))
dev.off()