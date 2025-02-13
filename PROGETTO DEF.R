getwd()
setwd("C:/Users/Agata/Desktop/Statistica")
data_DEF = read.csv("Florida_survey.csv")
View(data_DEF)
data_DEF$life_after_death
print(data_DEF$life_after_death)
table(data_DEF$life_after_death)
table(data_DEF$affirmative_action_support)
data_DEF$affirmative_action_support[17]
data_DEF$affirmative_action_support[17] <- "y"
data_DEF$life_after_death[2:3]
data_DEF$life_after_death[2:3] <- "y"
data_DEF$life_after_death[6] <- "y"
data_DEF$life_after_death[c(9,12,27,29,30)] <- "y"
data_DEF$life_after_death[c(34,35,37,40,42,54,55,57)] <- "n"
View(data_DEF)
table(data_DEF$life_after_death)
table(data_DEF$affirmative_action_support)
data_DEF = na.omit(data_DEF)
data_DEF
sum(is.na(data_DEF))
### exercise 1
modello_linear_regression = lm(data_DEF$sports ~ ., data = data_DEF)
summary(modello_linear_regression)
plot(modello_linear_regression)
install.packages("car")
library(car)
library(dplyr)
vif(modello_linear_regression)
### exercise 2 
library(glmnet)
x <- model.matrix(data_DEF$sports ~., data = data_DEF)[,-9]
x
y <- data_DEF$sports
y
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(data_DEF),replace = TRUE)
test <- (!train)
set.seed(1)
grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
summary(lasso.mod)
plot(lasso.mod)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
cv.out
summary(cv.out)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
best_coefs <- coef(cv.out, s = bestlam)
print(best_coefs)
### exercise 3 
library("leaps")
library("margins")
library(dplyr)
data_DEF <- data_DEF %>% mutate_if(is.character, as.factor)
k=10
set.seed(11)
folds=sample(rep(1:k,length=nrow(data_DEF)))
folds
table(folds)
folds <- as.factor(folds)
cv.errors=matrix(NA,k,26)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}
data= data_DEF[folds!=h,]
View(data)
for(h in 1:k){
  for(i in 1:26){
  best.fit=regsubsets(sports ~.,data= data_DEF[folds!=h,],nvmax=26,method="forward")
    pred=predict.regsubsets(best.fit,data_DEF[folds==h,],id=i)
    cv.errors[h,i]=mean( (data_DEF$sports[folds==h]-pred)^2)
  }
}
summary(best.fit)
rmse.cv=sqrt(apply(cv.errors,2,mean))
rmse.cv
summary(rmse.cv)
plot(1:26, rmse.cv,pch=19,type="b")
which.min(rmse.cv)
points(3,rmse.cv[3],pch=20,col="red")
reg.best <- regsubsets(sports ~ ., data = data_DEF, nvmax = 26)
reg.best
summary(reg.best)
coef(reg.best, 3)
### exercise 4 
require(gam)
modello_GAM = gam(data_DEF$sports~s(high_sch_GPA)+ gender,data=data_DEF)
summary(modello_GAM)
plot.Gam(modello_GAM, se= TRUE, col ="blue")
require(splines)
best_spline =smooth.spline(data_DEF$high_sch_GPA, data_DEF$sports,cv=TRUE)
best_spline
plot(best_spline)
bestspline_model = gam(data_DEF$sports~s(high_sch_GPA, sp= best_spline$spar)+ gender,data=data_DEF)
summary(bestspline_model)
### exercise 5
### Logistic model 
x <- model.matrix(data_DEF$abortion_legalize ~., data = data_DEF)[,-16]
View(x)
x = x[,-1]
View(x)
y <- data_DEF$abortion_legalize
y_train <- y[c(1:40)]
View(y_train)
X_train = x[c(1:40),]
View(X_train)
y_train <- ifelse(y_train == "y",1,0)
y_train
DATA_training <- cbind(X_train, y_train)
DATA_training
DATA_training <- as.data.frame(DATA_training)
logistic_model <- glm(DATA_training$y_train ~., data = DATA_training, family = binomial)
summary(logistic_model)
coef(logistic_model)
summary(logistic_model)$coef
plot(logistic_model)
### Linear discriminant analysis 
library(MASS)
LDA_model <-lda(DATA_training$y_train ~., data = DATA_training)
LDA_model
plot(LDA_model)
### Knn algorithm
X_test = x[c(41:60),]
View(X_test)
y <- ifelse(y == "y",1,0)
Y_test = y[c(41:60)]
Y_test
library(class)
set.seed(1)
knn_1 <- knn(X_train, X_test, y_train, k = 1)
summary(knn_1)
knn_3 <- knn(X_train, X_test, y_train, k=3)
summary(knn_3)
knn_5 <- knn(X_train, X_test, y_train, k=5)
summary(knn_5)
knn_7 <- knn(X_train, X_test, y_train, k=7)
summary(knn_7)
### Exercise 6 
### Logistic predictions 
DATA_testing = cbind(X_test, Y_test)
DATA_testing = as.data.frame(DATA_testing)
View(DATA_testing)
View(DATA_testing)
logistic_probs <- predict(logistic_model, DATA_testing, type = "response")
logistic_pred <- rep("No", 20)
logistic_pred[logistic_probs > .5] <- "Yes"
table(logistic_pred, Y_test)
### Accuracy
14/20
### Precision
12/16
### sensitivity
12/14
### specificity 
2/6
install.packages("pROC")
install.packages("ggplot2")  
library(pROC)
library(ggplot2)
roc_obj <- roc(Y_test, logistic_probs)
plot(roc_obj, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

### LDA predictions 
LDA_predictions <- predict(LDA_model, DATA_testing)
names(LDA_predictions)
lda_class <- LDA_predictions$class
lda_class
table(lda_class, Y_test)
roc_obj <- roc(Y_test, LDA_predictions$class)
plot(roc_obj, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))
mean(lda_class == Y_test)
sum(LDA_predictions$posterior[, 1] >= .5)
sum(LDA_predictions$posterior[, 1] < .5)
### Knn predictions 
table(knn_1, Y_test)
### Accuracy
15/20
### Precision 
14/14+0
### Sensitivity 
14/19
### Specificity 
1/1+0
table(knn_3, Y_test)
### Accuracy 
12/20
### Precision 
12/14
### Sensitivty 
12/18
### Specificity 
0/0+2
table(knn_5, Y_test)
### Accuracy
14/20
### Precision 
14/14
### Sensitivity 
14/20
### Specificity
0/0 
table(knn_7, Y_test)
### Accuracy 
14/20
### Precision 
14/14
### Sensitivity 
14/20
### Specificity 
0/0
knn_1_prob <- as.numeric(knn_1 == 1)
roc_obj <- roc(Y_test, knn_1_prob)
plot(roc_obj, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))
