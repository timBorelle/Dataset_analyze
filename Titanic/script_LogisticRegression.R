### Logistic regression ###

# Titanic train dataset
dirnamepath = dirname(rstudioapi::getSourceEditorContext()$path)
csvDataPath <- paste(dirnamepath, "/train.csv", sep="")

df <- read.csv(csvDataPath, sep=',')

library(ggplot2)

ggplot(df,aes(Survived)) + geom_bar()
ggplot(df,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)
ggplot(df,aes(Sex)) + geom_bar(aes(fill=factor(Sex)),alpha=0.5)
ggplot(df,aes(Age)) + geom_histogram(fill='blue',bins=20,alpha=0.5)

library(Amelia)

# missmap des données manquantes
missmap(df, main="Titanic Missing Data", 
        col=c("yellow", "black"), legend=FALSE)

missingage <- is.na(df$Age)
table(missingage)

#Different ways to handle missing data
#1) Remove the line or culumn
#df <- filter(df, !is.na(df$Age))
#2) Replace your missing
#2.2) Take the global average
#2.2) Averag per class or anything relevent

mean(df$Age)
mean(df$Age, na.rm=TRUE) 

#Subsetting on the class of people
df_3 = subset(df, df[,3] == 1)
mean(df_3$Age, na.rm=TRUE) 

#Subsetting on the sex of people
df_male = subset(df, df[,5] == "female")
mean(df_male$Age, na.rm=TRUE) 

# Gérer les cabines (nb de cabines: 0|1|2|..., A|B|C|..., )
head(df)
#df$Embarked[df$Embarked==""] <- NA
#Add new column : number of cabins
#df["nbCabins"] <- lengths(regmatches(df$Cabin, gregexpr("[A-Z]+", df$Cabin)))
#Add new column : class of cabin (0: no cabin, 1: A, 2: B, ...)
#df["CabinClass"] <-  

valuesTest1 <- "C125"
valuesTest2 <- "C125 B269"
regmatches(valuesTest2, gregexpr("[A-Z]{1}", valuesTest2))
#lengths(regmatches(valuesTest, gregexpr("[A-Z]+", valuesTest)))
#df$Cabin
head(df)

#missmap(df, main="Titanic Missing Data", 
#        col=c("yellow", "black"), legend=FALSE)
# ...

#Feature Engineering

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 38
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
fixed.ages <- impute_age(df$Age,df$Pclass)
df$Age <- fixed.ages

missmap(df, main="Titanic Missing Data", 
        col=c("yellow", "black"), legend=FALSE)
missingage <- is.na(df$Age)
table(missingage)



library(dplyr)
#Supprimer : idPassager,nom, idCabine, ...
df <- select(df,-PassengerId,-Name,-Ticket,-Cabin,-Embarked)

#To improve you model, you might want to deal with the Cabin variable
#Feature Engineering.

str(df)
df$Survived <- as.factor(df$Survived )
df$Pclass   <- as.factor(df$Pclass)

#3) Sampling
# Set a random seed so your "random" results are the same as this results
library(caTools)
set.seed(21) #Reproducibility!

sample <- sample.split(df$Survived, SplitRatio = 0.80) # SplitRatio = percent of sample==TRUE

# Training Data - 70/80%
train = subset(df, sample == TRUE)

# Testing Data - 20/30%
test = subset(df, sample == FALSE)

#Random Forest
library(randomForest)
model <- randomForest(Survived ~ . , data=df)
print(model)     #view result
predictions <- predict(model,newdata=test,type='response')
table(test$Survived, predictions)

#Regression logistic
log.model <- glm(formula = Survived ~ . , family = binomial(link='logit'),data = df)
summary(log.model)

#Predictions
predictions <- predict(log.model,newdata=test,type='response')

t = 0.63
fitted.results <- ifelse(predictions > t,1,0)

#Matrice de Confusion, Confusion Matrix
table(test$Survived, predictions > t)

ErrorRate <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-ErrorRate))

# avant : t:0.63 & accuracy=0.842