setwd('~/Projects/bankruptcy_prediction')
library(foreign)
library(class)
library(datasets)
library(caret)
library(gam)
library(Amelia)
library(corrplot)
library(pROC)

year1 = read.arff("1year.arff")

year1 <- year1[,c(-37,-21,-27)]

x = ncol(year1) - 1
for(i in 1:x){
  year1[[i]][is.na(year1[i])] = mean(year1[[i]], na.rm = TRUE)
  
}

normalization 
for (attr in names(year1)){
  if(attr != "class"){
    year1[attr] <- (year1[attr] - min(year1[attr])) / (max(year1[attr]) - min(year1[attr]))
  }
}

x <- year1[1:61]
y <- year1[62]
y
cor(x)
corrplot(cor(x))
year1$merged_7 <- year1$Attr7 + year1$Attr11 + year1$Attr14 + year1$Attr18
year1$merged_13 <- year1$Attr13 + year1$Attr42 + year1$Attr44 + year1$Attr58

set.seed(3033)
intrain <- createDataPartition(y = year1$class, p= 0.7, list = FALSE)
training <- year1[intrain,]
training$obs_weights = 1
training[training$class == 1,]$obs_weights = 14.5
testing <- year1[-intrain,]

sapply(training,function(x) sum(is.na(x)))
sapply(training, function(x) length(unique(x)))


# missmap(training, main = "Missing values vs observed")

fit_glm <- glm(class ~ Attr1 + Attr2 + Attr3 + Attr4 + Attr5 + Attr6 + merged_7 + Attr8 + 
                 Attr9 + Attr10 + Attr12 + merged_13 + Attr15 + Attr16 +
                 Attr17 + Attr19 + Attr20 + Attr22 + Attr23 + Attr24 + 
                 Attr25 + Attr26 + Attr28 + Attr29 + Attr30 + Attr31 + Attr32 +
                 Attr33 + Attr34 + Attr35 + Attr36  + Attr38 + Attr39 + Attr40 + 
                 Attr41 + Attr45 + Attr46 + Attr47 + Attr48 +
                 Attr49 + Attr50 + Attr52 + Attr53 + Attr54 + Attr55 + Attr56 + 
                 Attr57 + Attr59 + Attr61 + Attr62 + Attr63 + Attr64,
               data = training, family = "binomial", weights = obs_weights)
summary (fit_glm)
fitted_results <- predict (fit_glm,testing, type="response")
tab = table(testing$class, fitted_results > 0.5)
tab
TP = tab[2,2]
FP = tab[1,2]
TN = tab[1,1]
FN = tab[2,1]
recall = TP / (TP + FN)
recall
precission = TP / (TP + FP)
precission
precission * recall
accuracy = (TP + TN) / (TP + TN + FP + FN)
accuracy
# plot(residuals.glm(fit_glm, type = 'response'))
prob=predict(fit_glm,type=c("response"))
testing$prob=fitted_results
g3 <- roc(class ~ prob, data = testing)
plot(g3)   
