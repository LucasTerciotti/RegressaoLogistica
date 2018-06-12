fit <- glm(Survived ~ Pclass + Age + Sex + SibSp +
             Parch + Fare + Embarked,
           data = train,
           family = binomial(link = "logit"))
summary(fit)

anova(fit, test="Chisq")

#aqui criamos um modelo com mais campos, que segundo vimos pelo anova, não afetam diretamente a eficácia do modelo!
prediction_prob <- predict(fit,newdata=test,type='response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction,test$Survived)

prop.table(table(prediction,test$Survived))

#Se reduzirmos apenas para os mais relevantes ssegundo o ANOVA:
fit <- glm(Survived ~ Pclass + Sex + Age + SibSp,
           data=train, 
           family=binomial(link="logit"))
prediction_prob <- predict(fit, test, type='response')
prediction <- ifelse(prediction_prob > 0.5, 1, 0)
table(prediction,test$Survived)

prop.table(table(prediction,test$Survived))
#obtivemos uma porcentagem apenas 1% menor, provavelmente um pouco mais genérica.
#(Isso para o TITANIC! O nosso Homework é adaptar exatamente as mesmas coisas, mas para o birth control)
