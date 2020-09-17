install.packages('dplyr')
install.packages('ggplot2')
install.packages("MASS")
library(MASS)


library(ggplot2)
library(dplyr)
setwd('C:/Users/WilsonLi/Desktop/000R Programming UTD')
getwd()
youtube<-read.csv('USvideos.csv')

#category videos based on views count
summary(youtube$views)
#3rd quartile is 1823157 views,so
youtube$Popular_Video[youtube$views>=1823157]<-'Yes'
youtube$Popular_Video[youtube$views<1823157]<-'No'
youtube$Popular_Video<-as.factor(youtube$Popular_Video)

#fit logistic regression, y= popular_video
#split the youtube dataset to 80% training and 20% test
install.packages('caret')
library(caret)
## 80% of the sample size
smp_size<- floor(0.8 * nrow(youtube))
## set the seed to make your partition reproducible,We start the list of random numbers at the position 123
set.seed(123)
train_ind <- sample(seq_len(nrow(youtube)), size = smp_size)

Youtube_train <- youtube[train_ind, ]
Youtube_test <- youtube[-train_ind, ]

glm.fit.d <- glm(Youtube_train$Popular_Video ~ category_id + likes + dislikes  + comment_count + comments_disabled + ratings_disabled, data=Youtube_train, family="binomial")
glm.probs.d <- predict(glm.fit.d, type="response", newdata=Youtube_test)
summary(glm.fit.d)# p <0.05
summary(glm.probs.d)
glm.preds.d <- ifelse(glm.probs.d>.5, "Yes", "No")
cm.d <- table(glm.preds.d,Youtube_test$Popular_Video)
cm.d
acc.d <- (cm.d["Yes", "Yes"] + cm.d["No", "No"])/sum(cm.d)
acc.d
#0.89 trainng fit testing

#fit lda

lda.fit.e <- lda(Youtube_train$Popular_Video ~ category_id + likes + dislikes  + comment_count + comments_disabled + ratings_disabled, data=Youtube_train)
lda.preds.e <- predict(lda.fit.e, newdata=Youtube_test) 
cm.e <- table(lda.preds.e$class,Youtube_test$Popular_Video)
cm.e
acc.e<- (cm.e["No", "No"] + cm.e["Yes", "Yes"])/sum(cm.e)
acc.e
#0.79

#fit qda
qda.fit.f <- qda(Youtube_train$Popular_Video ~ category_id + likes + dislikes  + comment_count + comments_disabled + ratings_disabled, data=Youtube_train)
qda.preds.f <- predict(qda.fit.f, newdata=Youtube_test)
cm.f <- table(qda.preds.f$class,Youtube_test$Popular_Video)
cm.f
acc.f<- (cm.f["No", "No"] + cm.f["Yes", "Yes"])/sum(cm.f)
acc.f
#0.87

#KNN
summary(Youtube_train)
Youtube_train$comments_disabled_num[Youtube_train$comments_disabled=='FALSE']<-0
Youtube_train$comments_disabled_num[Youtube_train$comments_disabled=='TRUE']<-1
Youtube_train$ratings_disabled_num[Youtube_train$ratings_disabled=='FALSE']<-0
Youtube_train$ratings_disabled_num[Youtube_train$ratings_disabled=='TRUE']<-1
Youtube_train$Popular_Video_num[Youtube_train$Popular_Video=='No']<-0
Youtube_train$Popular_Video_num[Youtube_train$Popular_Video=='Yes']<-1


Youtube_test$comments_disabled_num[Youtube_test$comments_disabled=='FALSE']<-0
Youtube_test$comments_disabled_num[Youtube_test$comments_disabled=='TRUE']<-1
Youtube_test$ratings_disabled_num[Youtube_test$ratings_disabled=='FALSE']<-0
Youtube_test$ratings_disabled_num[Youtube_test$ratings_disabled=='TRUE']<-1
Youtube_test$Popular_Video_num[Youtube_test$Popular_Video=='No']<-0
Youtube_test$Popular_Video_num[Youtube_test$Popular_Video=='Yes']<-1

library(class)
set.seed(1)

train.g =Youtube_train[c("category_id","likes","dislikes","comment_count","comments_disabled_num","ratings_disabled_num","Popular_Video_num")]
test.g =Youtube_test[c("category_id","likes","dislikes","comment_count","comments_disabled_num","ratings_disabled_num","Popular_Video_num")]
knn.pred = knn(data.frame(train.g), data.frame(test.g),train.g$Popular_Video, k=1)
cm.g <- table(knn.pred,Youtube_test$Popular_Video_num)
cm.g
acc.g<- (cm.g["0", "0"] + cm.g["1", "1"])/sum(cm.g)
acc.g
#0.90

class(category_id)
category_id =  as.factor(category_id)
model1 = lm(views ~ comment_count+likes+dislikes+category_id+comments_disabled+ratings_disabled,
            data = youtube)
summary(model1)

model2 = lm(views ~ comment_count+likes+dislikes+ratings_disabled,data = youtube)
summary(model2)

Errortrue <- filter(youtube,youtube$video_error_or_removed == "True")
hist(Errortrue$category_id)

