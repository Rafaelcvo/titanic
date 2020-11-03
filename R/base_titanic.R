# Bibliotecas
library(ggplot2)

# Importando a base de dados
base <- read.csv('dataset/train.csv')
teste <- read.csv('dataset/test.csv')
base$Name <- NULL
base$Ticket <- NULL
base$Cabin <- NULL
base$SibSp <- NULL
base$Parch <- NULL

summary(base)

base[base$Age < 0 & !is.na(base$Age), ]

base[is.na(base$Age),]

base$Age <- ifelse(is.na(base$Age), mean(base$Age, na.rm = TRUE), base$Age)

base$Sex <- factor(base$Sex, levels = c('male', 'female'), c(0,1))


















