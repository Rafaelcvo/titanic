#chamando os pacotes 
library(randomForest)
library(ggplot2)

#importando os dados
train <- read.csv(file = "dataset/train.csv", stringsAsFactors = T,  header = TRUE)
test <- read.csv(file = "dataset/test.csv", stringsAsFactors = T,  header = TRUE)

# verificando dados faltantes.
colSums(is.na(train))
colSums(is.na(test))
colSums(train == "")
colSums(test == "")

# criando a coluna survived no test
test$Survived <- NA

# craindo uma coluna para identificar se um dado é treino ou teste
train$IsTrainSet <- T
test$IsTrainSet <- F

# agrupando os dados
titanic_set <- rbind(train, test)

# Estatistica descritiva dos dados
summary(titanic_set)

#Check dados faltantes
colSums(is.na(titanic_set)) 
colSums(is.na(titanic_set))
colSums(titanic_set =="")

# Transformação simples ETL/4Cs (Cleaning, completing, coreting, creating)
titanic_set$Survived                      <- as.factor(titanic_set$Survived)
titanic_set$Pclass                        <- as.factor(titanic_set$Pclass)
titanic_set$Age[is.na(titanic_set$Age)]   <- median(titanic_set$Age, na.rm = T)
titanic_set$SibSp                         <- as.numeric(titanic_set$SibSp)
titanic_set$Parch                         <- as.numeric(titanic_set$Parch)
titanic_set$Fare[is.na(titanic_set$Fare)] <- median(titanic_set$Fare, na.rm = T) 
titanic_set$Embarked[titanic_set$Embarked ==""] <- "S"
titanic_set$Embarked                            <- as.factor(as.character(titanic_set$Embarked))

# Criando os modelos de treino e teste ja preparados.
titanic_train <- titanic_set[titanic_set$IsTrainSet == T,]
titanic_test <- titanic_set[titanic_set$IsTrainSet == F,]

# Criando a formula
survived_formula <- as.formula("Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked")

titanic_model <- randomForest(formula = survived_formula,
                              data = titanic_train, 
                              ntree = 100, 
                              importance = T)

titanic_model

plot(titanic_model)

# Matriz de importancia daas variaveis
importance_var <- importance(titanic_model, type = 1)

#Dando uma formatada na tabela
tabela_de_importancia <- data.frame(variaveis=row.names(importance_var), 
                                    importancia=importance_var[,1]);tabela_de_importancia

#Gerando o grafico
grafico <- ggplot(tabela_de_importancia, 
                  aes(x=reorder(variaveis,importancia), y=importance_var)) +
  geom_bar(stat="identity", fill="#5cc9c1") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importância") + 
  ggtitle("Importância das variáveis no Modelo RF") +
  theme(plot.title=element_text(size=18))

grafico

#Cria um data frame com o campo PassengerId
submission <- data.frame(PassengerId = test$PassengerId,
                         Survived = predict(titanic_model, newdata =  titanic_test))

#View nos dados de saida
View(submission)

write.csv(submission, file = "submission_r.csv", row.names=F)



























































































































