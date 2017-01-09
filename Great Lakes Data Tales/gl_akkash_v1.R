####Great Lakes DATA Tales

##Training Data
gl_train <- read.csv('greatlakes.csv', header = T)
head(gl_train)
str(gl_train)
summary(gl_train)

table(gl_train$Campaign, gl_train$Target.Clicks)
#From the above table we can remove Campaign as its redundant data to Targt.Clicks
#We can remove the Minimum and Maximum CPC as its covered in Amount.Spend on particular day
#We have to remove CTR since we already have Obt.Click and Impression, also CTR is in (%)

#Removing Days.Index, Campaign, CPC and CTR
new_gl_train <- gl_train[,-c(1:2,6:8)]
new_gl_train$Target.Clicks <- as.factor(new_gl_train$Target.Clicks)
new_gl_train$SpecialDay <- as.factor(new_gl_train$SpecialDay)
new_gl_train$Avg.Position <- as.factor(new_gl_train$Avg.Position)

str(new_gl_train)

#Distribution of Y
plot(density(gl_train$Obtained.Leads))

library(corrplot)
corrplot(cor(new_gl_train),method = 'number')

library(ggplot2)
#Checking the CTR.
plot(density(gl_train$Avg..Bounce.Rate))
plot(density(gl_train$Returning.Users))

table(new_gl_train$SpecialDay)
table(new_gl_train$Avg.Position)

plot(new_gl_train$Obtained.Leads, new_gl_train$Targeted.Leads)
plot(new_gl_train$Obtained.Leads, new_gl_train$Returning.Users)
plot(new_gl_train$Obtained.Leads, new_gl_train$Amount.Spend)
plot(new_gl_train$Obtained.Leads, new_gl_train$Total.Revenue)
plot(new_gl_train$Obtained.Leads, new_gl_train$Avg.Time.Page)
plot(new_gl_train$Obtained.Leads, new_gl_train$Impressions)


#Build a model using simple linear regression
gl.train.model <- lm(Obtained.Leads~.,data = new_gl_train)
summary(gl.train.model)

gl.train.model_f <- lm(Obtained.Leads~1, data= new_gl_train)

#Implementing Stepwise regression
step(gl.train.model, scope = c(lower = gl.train.model, upper=gl.train.model_f), direction = 'backward')

gl.train.model_2 <- update(gl.train.model,.~.-Obt.Click-Targeted.Leads-Total.Revenue, data = new_gl_train)
summary(gl.train.model_2)

gl.train.model_3 <- update(gl.train.model,.~.-Targeted.Leads-Total.Revenue, data = new_gl_train)
summary(gl.train.model_3)

###Testing Dataset
gl_test <- read.csv('greatlakes_test.csv', header = T)
head(gl_test)
str(gl_test)

new_gl_test <- gl_test[,-c(1:2,6:8)]
new_gl_test$Target.Clicks <- as.factor(new_gl_test$Target.Clicks)
new_gl_test$SpecialDay <- as.factor(new_gl_test$SpecialDay)
new_gl_test$Avg.Position <- as.factor(new_gl_test$Avg.Position)

gl.prediction <- predict(gl.train.model_2, newdata = new_gl_test)
summary(gl.prediction)
mean(gl.prediction)

submission <- data.frame(Day.Index = gl_test$Day.Index, Obtained.Leads= gl.prediction)

write.csv(submission,'gl_Solution3.csv')


