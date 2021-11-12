library(readr)
library(dplyr)
library(psych)
library(ggplot2)


datasetcovid <- read_csv("datasetcovid.csv")

# Transformar sexo em fator

datasetcovid$CS_SEXO <- factor(datasetcovid$CS_SEXO, label = c("M", "F"),
                               levels = c(0,1))

# Transformando escolaridade em fator

datasetcovid$CS_ESCOL_N <- factor(datasetcovid$CS_ESCOL_N,
                                  labels = c("Sem escolaridade", "Fundamental 1", "Fundamental 2", "Medio", "Superior"),
                                  levels = 0:4, order = T)

# Transformando raça em fator

datasetcovid$CS_RACA <- factor(datasetcovid$CS_RACA,
                               labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
                               levels = 1:5)

# Tranformando obesidade, asma, diabetes, cardiopatia e UTI em fator

datasetcovid$OBESIDADE <- factor(datasetcovid$OBESIDADE,
                                 labels = c("Nao", "Sim"),
                                 levels = c(0,1))
datasetcovid$ASMA <- factor(datasetcovid$ASMA,
                                 labels = c("Nao", "Sim"),
                                 levels = c(0,1))
datasetcovid$DIABETES <- factor(datasetcovid$DIABETES,
                            labels = c("Nao", "Sim"),
                            levels = c(0,1))
datasetcovid$CARDIOPATI <- factor(datasetcovid$CARDIOPATI,
                                labels = c("Nao", "Sim"),
                                levels = c(0,1))

datasetcovid$UTI <- factor(datasetcovid$UTI,
                                  labels = c("Nao", "Sim"),
                                  levels = c(0,1))

glimpse(datasetcovid)

#Analise descritiva

# tabela cruzada sexo e escolaridade
tabela_1 <- table(datasetcovid$CS_SEXO, datasetcovid$CS_ESCOL_N)
prop.table(tabela_1)

# Histograma e tabela para variável idade
hist(datasetcovid$NU_IDADE_N, freq = F,  xlab = "Idade", ylim = c(0, 0.04), main = "Distribuição das idades", col = "lightblue")
curve(dnorm(x, mean = mean(datasetcovid$NU_IDADE_N), sd = sd(datasetcovid$NU_IDADE_N)), add = T)
range(datasetcovid$NU_IDADE_N)
nclass.Sturges(datasetcovid$NU_IDADE_N)
tabela_2 <- table(cut(datasetcovid$NU_IDADE_N, seq(0, 102, l = 14)))
summary(datasetcovid$NU_IDADE_N)

# Distribuição da idade por grupos: sexo e escolaridade.
describeBy(datasetcovid$NU_IDADE_N, group = datasetcovid$CS_SEXO:datasetcovid$CS_ESCOL_N)


# Distribuição e boxplot entre obesidade e tempo entre os primeiros sintomas e a data de evolução

tapply(datasetcovid$TEMP_SIN, datasetcovid$OBESIDADE, summary)
tapply(datasetcovid$TEMP_SIN, datasetcovid$OBESIDADE, sd)
plot(datasetcovid$TEMP_SIN ~ datasetcovid$OBESIDADE, ylab = "Tempo" , xlab = "Obesidade")

#Associação entre as variáveis
#fprov = table(datasetcovid$OBESIDADE)
#variancias <- tapply(datasetcovid$TEMP_SIN, datasetcovid$OBESIDADE, var)
#s2barra = weighted.mean(variancias, fprov)
#s2 = var(datasetcovid$TEMP_SIN)
#R2 <- 1 - (s2barra/s2)
#R2

# Distribuição e boxplot entre asma e tempo entre os primeiros sintomas e a data de evolução

tapply(datasetcovid$TEMP_SIN, datasetcovid$ASMA, summary)
tapply(datasetcovid$TEMP_SIN, datasetcovid$ASMA, sd)
plot(datasetcovid$TEMP_SIN ~ datasetcovid$ASMA, ylab = "Tempo", xlab = "Asma")


# Distribuição e boxplot entre diabetes e tempo entre os primeiros sintomas e a data de evolução

tapply(datasetcovid$TEMP_SIN, datasetcovid$DIABETES, summary)
tapply(datasetcovid$TEMP_SIN, datasetcovid$DIABETES, sd)
plot(datasetcovid$TEMP_SIN ~ datasetcovid$DIABETES, ylab = "Tempo", xlab = "Diabates")

# Distribuição e boxplot entre cardiopatia e tempo entre os primeiros sintomas e a data de evolução

tapply(datasetcovid$TEMP_SIN, datasetcovid$CARDIOPATI, summary)
tapply(datasetcovid$TEMP_SIN, datasetcovid$CARDIOPATI, sd)
plot(datasetcovid$TEMP_SIN ~ datasetcovid$CARDIOPATI, ylab = "Tempo", xlab = "Cardiopatia")

#Cormobidades X Obitos

