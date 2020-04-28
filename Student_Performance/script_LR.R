### Linear regression ###

# dirname of the current file
dirnamepath = dirname(rstudioapi::getSourceEditorContext()$path)
csvDataPath <- paste(dirnamepath, "/student-mat.csv", sep="")

# 1) COmprendre les données
# Read CSV
df <- read.csv(csvDataPath, sep=';')

head(df)
summary(df)

#df$address <- as.factor(df$address)

df$Medu <- as.factor(df$Medu)
df$Fedu <- as.factor(df$Fedu)
df$Dalc <- as.factor(df$Dalc)
df$Walc <- as.factor(df$Walc)

str(df)

library(ggplot2)    # visualization
library(ggthemes)
library(dplyr)      # data maipulation  

#install.packages('corrgram',repos = 'http://cran.us.r-project.org')
#install.packages('corrplot',repos = 'http://cran.us.r-project.org')
library(corrgram)
library(corrplot)

num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[,num.cols])

cor.data

corrplot(cor.data, method='color')

corrgram(df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df,aes(x = G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()
ggplot(df,aes(x = G2)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()
ggplot(df,aes(x = G1)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()


#3) Sampling
#Problèmes d'overfitting

library(caTools)
# set a random see so your "random" results are the same as this notebook
set.seed(101)

# split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$age, SplitRatio = 0.70) # SplitRatio = percent of sample=

# Training data - 70/80% (exos de cours)
train = subset(df, sample = TRUE)

# Testing data - 20/30% (examen final)
test = subset(df, sample = FALSE)


#4) Modélisation
model <- lm(G3 ~ .,train)
summary(model)


#5) Prédictions
G3.predictions <- predict(model, test)

#6) Evaluation
results <- cbind(G3.predictions,test$G3)
colnames(results) <- c('pred','real') 
results <- as.data.frame(results)

SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
R2



