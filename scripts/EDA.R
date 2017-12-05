setwd("/Users/MandyZhang/Desktop/DATS 6101 - group project 2/")
voice = read.csv("../data/voice.csv",stringsAsFactors = FALSE)

str(voice)

# use group_by function to count in gender
library(dplyr)
gender = voice %>% group_by(label)
gender = gender %>% summarise(n=n())
# bar plot
pdf("../figures/count in gender.pdf", width = 16.5, height = 9.5)
ylim <- c(0, 1.1*max(gender$n))
g = barplot(gender$n, ylab = "Count", xlab = "gender",
        names.arg = gender$label, main = "Number of People in Each Gender",border="red",
        col="blue",ylim = c(0, 1.1*max(gender$n)),width = 0.1,
        density=10)
text(x = g, y = gender$n, label = gender$n, pos = 3, cex = 1, col = "red")
dev.off()

pdf("../figures/count in gender.pdf", width = 16.5, height = 9.5)
ggplot(gender, aes(label, n, color = label, fill = label)) + geom_bar(stat = 'identity',alpha=0.3,width=0.5)+
  geom_text(aes(label = n), vjust=-0.3, size = 5) +
  labs(x = "Gender", y = "Count", size = 3) + ggtitle("Number of People in Each Gender") + theme_light()
dev.off()


# distribution of feature:
# install.packages("reshape")
# https://www.r-bloggers.com/quick-plot-of-all-variables/
library(tidyr)
library(ggplot2)
# Convert dataframe to key-value pairs
pair = gather(voice[, 1:20])
#plot
pdf("../figures/all variables density.pdf", width = 16.5, height = 9.5)
ggplot(pair, aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()+
  labs(x = "feature in each column") + ggtitle("Density of all variables")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) 
dev.off()


# compare two variables:
pdf("../figures/Modulation Index vs. Mean Frequency.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=modindx, y=meanfreq, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "Modulation Index", y = "Mean Frequency") + ggtitle("Modulation Index vs. Mean Frequency")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

pdf("../figures/Skew vs. Kurtosis.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(skew, kurt, group=label)) + geom_line(aes(colour = label), size = 0.7,alpha=0.9) +
  labs(x = "Skew", y = "Kurtosis", size = 3) + ggtitle("Skew vs. Kurtosis")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold",size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() + scale_color_hue(l=38, c=65)
dev.off()

# we can plot more ggplot about two variables comparision after feature selection.
# SD vs. Mean Frequency
pdf("../figures/sd_vs_meanfreq.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=sd, y=meanfreq, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "Standard Deviation of Frequency", y = "Mean Frequency") + ggtitle("Standard Deviation vs. Mean Frequency")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

#Avg of Fundmental Frequency vs Mean Frequency
pdf("../figures/meafun_vs_meanfreq.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=meanfun, y=meanfreq, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "Average of Fundmental Frequency", y = "Mean Frequency") + ggtitle("Average of Fundmental Frequency vs. Mean Frequency")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

#SD vs Avg of Fundamental Frenquency
pdf("../figures/meanfun_vs_sd.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=meanfun, y=sd, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "Average of Fundmental Frequency", y = "Standard Deviation") + ggtitle("Average of Fundmental Frequency vs. Standard Deviation")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

#Q25 vs IQR
pdf("../figures/1stquant_vs_IRQ.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=Q25, y=IQR, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "First Quantile", y = "Interquantile") + ggtitle("First Quantile vs. Interquantile")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

#spectral entropy vs spectral flatness
pdf("../figures/Entropy_vs_flatness.pdf", width = 16.5, height = 9.5)
ggplot(voice, aes(x=sp.ent, y=sfm, group=label)) + geom_point(aes(colour = label), size = 1,alpha=0.55) +
  labs(x = "Spectral Entropy", y = "Spectral Flatness") + ggtitle("Spectral Entropy vs. Spectral Flatness")+
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 15, vjust = 1),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(vjust = 0.1, hjust = 0.1)) + theme_light() +  scale_color_hue(l=38, c=65)
dev.off()

## Mean clustering and Standardizing
variables = voice[ ,-21]
scaled_voice = scale(variables, center = TRUE, scale = TRUE)
scaled_voice = cbind(scaled_voice, label = voice$label)

write.csv(scaled_voice, file = "../data/scaled_voice.csv",row.names = FALSE)




