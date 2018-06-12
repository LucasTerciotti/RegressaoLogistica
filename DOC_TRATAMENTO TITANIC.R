library(randomForest)
library(rpart)
titanic <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/titanic.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))

#Precisamos tratar os dados:
summary(titanic)

library(Amelia)
missmap(titanic, main = "Missing values vs observed")

#Idades:
Agefit <- rpart(Age ~ Pclass + Sex + SibSp +
                  Parch + Fare + Embarked,
                data=titanic[!is.na(titanic$Age),], 
                method="anova")

titanic$Age[is.na(titanic$Age)] <-
  
  predict(Agefit, titanic[is.na(titanic$Age),])

summary(titanic$Age)

#Embarked:
summary(titanic$Embarked) #esta como caracter e devem ser categorias para melhor quantificar

titanic$Embarked <- as.factor(titanic$Embarked)

summary(titanic$Embarked)
#descobrir onde temos vazios:
#Who they are?
which(titanic$Embarked == "")

titanic$Embarked[c(62,830)] = "S"
#Make sure "S" is entered as a factor
titanic$Embarked <- factor(titanic$Embarked)

#Olhando todos de novo:
str(titanic)#embarked ja esta como fator, mas sexo não

#SEXO:
titanic$Sex <- as.factor(titanic$Sex)
summary(titanic$Sex)
contrasts(titanic$Sex)

#ETAPA1 - separar em dados de treinamento e em dados de teste! (30 teste/70 treinamento)
set.seed(123)

train.index <- sample((nrow(titanic)),0.7*nrow(titanic))

train <- titanic[train.index,]
test  <- titanic[-train.index,]

save(titanic, file="titanic.Rdata")
