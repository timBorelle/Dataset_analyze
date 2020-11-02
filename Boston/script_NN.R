### Réseaux de neurones (Neural networks) ###

library(dplyr)
library(MASS)

data <- Boston
str(data)
summary(data)
any(is.na(data))

#install.packages('neuralnet',repos = 'http://cran.us.r-project.org')
library(neuralnet)

#Normalisation
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

#Normalisation des données (entre 0 et 1)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

library(caTools)

split <- sample.split(scaled, SplitRatio = 0.70) 

train = subset(scaled, split == TRUE)
test = subset(scaled, split == FALSE)

# Get column names
n <- names(train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=TRUE)

plot(nn)  

predicted.nn.values <- compute(nn,test[1:13])


#On dénormalise les prédictions
true.predictions <- predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
# Convert the test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

# test.r : la réalité
# shoot. : les prédictions
# Mean Squared Error --> objectif minimiser MSE
MSE.nn <- sum((test.r - predicted.nn.values$net.result)^2)/nrow(test)
MSE.nn

error.df <- data.frame(test.r,true.predictions)
head(error.df)

library(ggplot2)
ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point() + stat_smooth()
  