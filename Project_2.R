library(ggplot2)
library(GGally)
library(caret)
library(MASS)

wine = read.table(file = "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",  
                  sep = ",", head=F)
colnames(wine) = c("class", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", "Magnesium", 
                   "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins", 
                   "Color_intensity", "Hue", "OD280/OD315_of_diluted wines", "Proline")
wine = wine[which(wine$class!=1),c(1,5,11)]
wine$class=as.factor(wine$class-2)
colnames(wine)=c("y","x1","x2")
ggpairs(wine, ggplot2::aes(color=y)) + theme_bw(18)

n <- nrow(wine)
set.seed(123)
reorder = sample(1:n)
test = wine[reorder[1:round(n/2,0)],] 
train = wine[reorder[(round(n/2,0)+1):n],]

## Question #1
LR.Wine <- glm(y~x1+x2, data=train, family="binomial")
summary(LR.Wine)

## Question #2
## B1 is coefficient of x1, B2 is the coefficient of x2

## Question #3
## -6.1407194 - 0.1054428 *x1 + 1.5974865 *x2 = 0
## x2 = 3.84439 + 0.0660 *x2

g1 = ggplot(data=train, aes(x=x1, y=x2, colour=y)) + 
  geom_point(pch = 1) + theme_bw(18) # Plot the training data
g1 + geom_abline(slope = 3.84439,intercept=0.066) + 
  ggtitle("Trainning data and logistic boundary") + 
  theme_bw(18)

## Question #4
LR.pred.prob <- predict(LR.Wine,newdata = test, type = "response")

## Question #5
## If P(Y=1)≥0.5, predict class 1.
## If P(Y=1)<0.5, predict class 0.
LR.pred <- ifelse(LR.pred.prob > 0.5, 1, 0)

## Question #6
LR.pred.prob <- predict(LR.Wine,newdata = train, type = "response")
LR.pred <- ifelse(LR.pred.prob > 0.5, 1, 0)
cm <- confusionMatrix(as.factor(LR.pred), train$y)
print(cm)

accuracy <- cm$overall["Accuracy"]
test_error_rate <- 1 - accuracy

print(test_error_rate)

## Question #7
LDA.Wine <- lda(y ~ x1 + x2, data=train)
ggplot(train, aes(x=x1, y=x2, color=y)) + 
  geom_point() +
  geom_abline(slope=LDA.Wine$scaling[2]/LDA.Wine$scaling[1], 
              intercept=-LDA.Wine$means[1,] %*% LDA.Wine$scaling/LDA.Wine$scaling[1], 
              color="black") +
  labs(title="LDA Decision Boundary")

## Question #8
predictions <- predict(LDA.Wine, newdata=train)
predicted_classes <- predictions$class

## Question #9
cm <- confusionMatrix(predicted_classes, train$y)
print(cm)
accuracy <- cm$overall["Accuracy"]
test_error_rate <- 1 - accuracy
print(test_error_rate)






