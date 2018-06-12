bw <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/birthwt.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))

library(randomForest)
library(rpart)
library(Amelia)

missmap(bw, main = "Missing values vs observed")
summary(bw)
str(bw)
hist(bw$bwt)

#transformar as categóricas em fatores:
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
bw[cols] <- lapply(bw[cols], as.factor)

#ETAPA 1: separação treinamento e teste
set.seed(123)

train.index <- sample((nrow(bw)),0.7*nrow(bw))

train <- bw[train.index,]
test  <- bw[-train.index,]

#agora para o modelo:
fit <- glm(low ~ age + lwt + race +
             smoke + ptl + ht + ui + ftv,
           data = train,
           family = binomial(link = "logit"))
summary(fit)

anova(fit, test="Chisq")

prediction_prob <- predict(fit,newdata=test,type='response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction,test$low)

prop.table(table(prediction,test$low))
