# ANÁLISE DESCRITIVA DOS CASOS CONFIRMADOS DE ÓBITOS NO ESTADO DE GOIÁS
#------------------------------------------------------------------------

# Fonte: SECRETARIA DE ESTADO DA SAÚDE - GOIÁS
# https://extranet.saude.go.gov.br/pentaho/api/repos/:coronavirus:paineis:painel.wcdf/generatedContent
# Data do estudo 24/08/2020  

#Carregamdo os dados
dataset <- read.table(file ="C:/Users/robis/OneDrive/DSA/5-Business_analytics/Cap02/R/dados/obitos_confirmados.csv", sep=";", header = TRUE)

#Analizando o tipo das variáveis
str(dataset)

# Análise das variáveis nunéricas
summary(dataset$semana_epi)
hist(dataset$semana_epi, main = "N° Óbitos por Sem", xlab = "Sem epidemiológica")

#A funçao summary e o histogram mostram que 50% dos óbitos até a data estudada ficaram
#apartir da Sem 28 ate a 34 com um range de apenas 6 Sem, o range totaol do estudo
# é de 23

library(lubridate)

# Inclusao da coluna "sintomas_óbitos(dias) que o intervalo de dias entre o início dos sintomeas e o óbito.
dataset2 <- dataset
dataset2["sintomas_óbito(dias)"] <- (ymd(dataset2$data_inicio_sintomas) %--% ymd(dataset2$data_obito)/ddays(1))

summary(dataset2$`sintomas_óbito(dias)`)
boxplot(dataset2$`sintomas_óbito(dias)`, main = "Box Plot - Dias até o óbito", ylab = "Dias")
plot(dataset2$`sintomas_óbito(dias)`, main = "Scatter Plot - Dias até o óbito",xlab = "Dias")

#Eliminaçao de outliers ou incocrreçoes nos registros
outliers_correcoes <- c(245,1001,1121,236,442,1534,12)
dataset3 <- dataset2[-outliers_correcoes, ]
summary(dataset3$`sintomas_óbito(dias)`)
# Média de 15 dias do aparecimento dos sintomas até o óbito
mean(dataset3$`sintomas_óbito(dias)`)
# 75% dos casos estao no range de até 20 dias do aparecimento dos sintomas até o óbito
quantile(dataset3$`sintomas_óbito(dias)`)

#Visualization
boxplot(dataset3$`sintomas_óbito(dias)`, main = "Box Plot -Dias do aparecimento dos sintomas até o óbito", ylab = "Dias")
plot(dataset3$`sintomas_óbito(dias)`, main = "Scatter Plot -Dias do aparecimento dos sintomas até o óbito",xlab = "Dias")

hist(dataset3$`sintomas_óbito(dias)`, main = "Dias do aparecimento dos sintomas até o óbito", xlab = "Dias", ylab = "Nº de óbitos", axes=F)
axis(1, at = seq(0,111, by = 3), pos = 0)
axis(2, at = seq(0,1527, by = 10), pos = 0)


# Análise das variáveis categóricas

#Proporção pro sexo em óbitos confirmados
prop_sex <- table(dataset3$sexo)
prop_sex1 <- prop.table(prop_sex)*100
round(prop_sex1, digits = 2)

#Proporção pro sexo em óbitos confirmados
prop_fxetaria <- table(dataset3$faixa_etaria)
prop_fxetaria1 <- prop.table(prop_fxetaria)*100
round(prop_fxetaria1, digits = 2)

library(ggplot2)
#Gráfico óbitos por sexo segmentado por faixa etária
ggplot(dataset3, aes(x = sexo)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Sexo") +
  scale_y_continuous(labels = scales::percent, name = "Proporção") +
  facet_grid(~ faixa_etaria) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Óbitos no sexo feminino sao maiores na faixa etária de 20 a 40 anos   

# Média de dias dos sintomas até Óbito por apresentação de comorbidade
aggregate(dataset3$`sintomas_óbito(dias)`~ dataset3$diabetes, FUN = mean, data = dataset3)
aggregate(dataset3$'sintomas_óbito(dias)'~ dataset3$doenca_cardiovascular, FUN = mean, data = dataset3)
aggregate(dataset3$'sintomas_óbito(dias)'~ dataset3$doenca_respiratoria, FUN = mean, data = dataset3)
aggregate(dataset3$'sintomas_óbito(dias)'~ dataset3$imunossupressao, FUN = mean, data = dataset3)

# total de casos por cidade

datasetcidades <- dataset3$municipio
datasetcidades <-as.data.frame(datasetcidades)
names(datasetcidades) <-"Cidade"
datasetcidades["Obitos_por_Covid-19"] <- 1
datasetcidades <- aggregate(datasetcidades$`Obitos_por_Covid-19` ~ datasetcidades$Cidade, FUN = sum, data = datasetcidades)
names(datasetcidades) <- c("Cidade", "Obitos_por_Covid-19")

library(dplyr)

datasetcidades1 <- datasetcidades
datasetcidades1["%"] <- datasetcidades1$`Obitos_por_Covid-19` / sum(datasetcidades1$`Obitos_por_Covid-19`)
datasetcidades1["%"] <- round(datasetcidades1$`%`*100, digits = 2)
datasetcidades1 <- datasetcidades1 %>% arrange(desc(datasetcidades1$`Obitos_por_Covid-19`))

print(datasetcidades1)






