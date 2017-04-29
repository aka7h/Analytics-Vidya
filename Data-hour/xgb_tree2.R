library(caret)

train <- read.csv('train.csv',na.strings = c(''))
test <- read.csv('test.csv',na.strings = c(''))

summary(train)
summary(test)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


getmode(train$Job)
train$Job[is.na(train$Job)] <- getmode(train$Job)
train$Education[is.na(train$Education)] <- getmode(train$Education)
train$Marital[is.na(train$Marital)] <- getmode(train$Marital)
train$Default[is.na(train$Default)] <- getmode(train$Default)

train$Emp.Var.Rate <- scale(train$Emp.Var.Rate)
train$Cons.Conf.Idx <- scale(train$Cons.Conf.Idx)

test$Job[is.na(test$Job)] <- getmode(test$Job)
test$Education[is.na(test$Education)] <- getmode(test$Education)
test$Marital[is.na(test$Marital)] <- getmode(test$Marital)
test$Default[is.na(test$Default)] <- getmode(test$Default)

test$Emp.Var.Rate <- scale(test$Emp.Var.Rate)
test$Cons.Conf.Idx <- scale(test$Cons.Conf.Idx)

train$ID <- NULL
ID <- test$ID

test$ID <- NULL


#simple logistic regression
tr <- trainControl(method = 'cv', number = 5)
#grid <- expand.grid(nrounds=2,max_depth=c(5,10),gamma=c(1,2),eta = c(0.01, 0.001),colsample_bytree = c(0.4, 0.7),min_child_weight = c(0.5, 1),subsample=c(0.5,1))


#decrising the dept of the tree
grid <- expand.grid(nrounds=2,max_depth=c(3,5),gamma=c(0,1,2),eta = c(0.01, 0.001),colsample_bytree = c(0.4, 0.7),min_child_weight = c(0.25, 0.5),subsample=c(0.25,0.5))


model <- train(Outcome~.,train,'xgbTree',trControl=tr,tuneGrid=grid)
plot(model)


#test prediction
predictions <- predict(model,test)

#sample
submission <- data.frame('ID'=ID,'Outcome'=predictions)
write.csv(submission, file = 'Submission_xgb.csv', row.names = F)
