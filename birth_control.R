bw <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/birthwt.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))

library(randomForest)
library(rpart)
library(Amelia)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

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
#aqui podemos verificar que age, lwt e ftv apresentam um alto valor de P-value, o que os sinaliza
#como estatisticamente insignificantes.
anova(fit, test="Chisq")
#Aqui reforçamos a ideia de que algumas variáveis não contribuem significativamente para o modelo, ja que
#ao serem adicionadas a queda no desvio residual é baixa em comparação a contribuição de, por exemplo, 
#smoke, ptl e ui. Um p-value alto aqui indica que o modelo explicaria mais ou menos a mesma quantidade de
#variação sem a adição dessas variaveis (race, age, ftv - as duas ultimas tambem foram vistas  summary)

#fizemos o modelo com o conjunto "train" de dados, agora aplicaremos em "test":
prediction_prob <- predict(fit,newdata=test,type='response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction,test$low)
prop.table(table(prediction,test$low))

#
#prediction          0          1
#         0 0.59649123 0.28070175
#         1 0.05263158 0.07017544
#Alcançamos uma acurácia de aproximadamente 66,6% de acerto. Devido as analises feitas anteriormente,
#vamos tentar utilizar um conjunto de dados menor: retiraremos os menos relevantes tanto pela análise em
#Summary quanto pela Anova:
fit <- glm(low ~ lwt + race +
             smoke + ptl + ht + ui,
           data = train,
           family = binomial(link = "logit"))
summary(fit) #ainda verificamos lwt com alto P-value
anova(fit, test="Chisq")

prediction_prob <- predict(fit,newdata=test,type='response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction,test$low)
prop.table(table(prediction,test$low))
# agora alcançamos exatamente a mesma acurácia anterior (66,6%), mesmo tendo retirado age e ftv.
