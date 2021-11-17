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

#Tabela por faixa etária 0-19, 20-59, 60 >
tabela_2 <- table(cut(datasetcovid$NU_IDADE_N, breaks = c(0, 19, 59, 102)))
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

#UTI X Obitos

Obito <- factor(datasetcovid$EVOLUCAO,
                labels = c("Sim", "Nao"),
                levels = c(1,0))

tabela_3 <- table(datasetcovid$UTI, Obito)
row.names(tabela_3) <- c("NAO UTI", "UTI")
tabela_3

mosaicplot(tabela_3, col = c("aquamarine", "lightblue"), cex =1.1,
           main = "Pacientes Internados" )

Q2 <- chisq.test(tabela_3)
Q2

# Sexo X Obito
tabela_4 <- table(datasetcovid$CS_SEXO, Obito)
row.names(tabela_4) <- c("Masculino", "Feminino")
tabela_4

mosaicplot(tabela_4, col = c("lightblue", "red"), cex =1.1,
           main = "Sexo dos Pacientes Internados" )

Q2 <- chisq.test(tabela_4)
Q2

# Obesidade X Obito
tabela_5 <- table(datasetcovid$OBESIDADE, Obito)
row.names(tabela_5) <- c("Possui_Obesidade", "Nao_Possui_Obesidade")
tabela_5

barplot(tabela_5,
        main = "Pacientes com Obesidade",
        ylim = c(0, 2300),
        col = c("#836FFF", "#483D8B"))

Q2 <- chisq.test(tabela_5)
Q2

# Diabetes x Obito
tabela_6 <- table(datasetcovid$DIABETES, Obito)
row.names(tabela_6) <- c("Possui_Diabetes", "Nao_Possui_Diabetes")
tabela_6
barplot(tabela_6,
        main = "Pacientes com Diabetes",
        ylim = c(0, 2500),
        col = c("#87CEFA", "#4682B4")
        )

Q2 <- chisq.test(tabela_6)
Q2

# Cardiopatia X Obito
tabela_7 <- table(datasetcovid$CARDIOPATI, Obito)
row.names(tabela_7) <- c("Possui_Cardiopatia", "Nao_Possui_Cardiopatia")
tabela_7
barplot(tabela_7,
        main = "Pacientes com Cardiopatia",
        ylim = c(0, 2300),
        col = c("#7FFFD4", "#5F9EA0"))

Q2 <- chisq.test(tabela_7)
Q2

# Asma X Obito
tabela_8 <- table(datasetcovid$ASMA, Obito)
row.names(tabela_8) <- c("Possui_Asma", "Nao_Possui_Asma")
tabela_8
barplot(tabela_8,
        main = "Pacientes com Asma",
        ylim = c(0, 2200),
        col = c("#F4A460", "#FFDEAD"))

Q2 <- chisq.test(tabela_8)
Q2

#Idade X Obito

tabela_9 <- table(cut(datasetcovid$NU_IDADE_N, breaks = c(0,19,60,102)), Obito)
row.names(tabela_9) <- c("Jovem", "Adulto", "Idoso")
tabela_9


barplot(tabela_9,
        beside = TRUE,
        legend = c("Jovem", "Adulto", "idoso"),
        args.legend = list(x = "top", bty = "n", ncol = 3),
        ylim = c(0, 1600),
        main = "Pacientes por Faixa Etária")


# Escolaridade X Obito
tabela_10 <- table(datasetcovid$CS_ESCOL_N, Obito)
tabela_10

barplot(tabela_10,
        beside = TRUE,
        legend = c("Sem escolaridade", "Fundamental 1", "Fundamental 2 ", "Médio", "Superior"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        ylim = c(0, 900),
        main = "Pacientes por Escolaridade")

Q2 <- chisq.test(tabela_10)
Q2

# Raca X Obito
tabela_11 <- table(datasetcovid$CS_RACA, Obito)
tabela_11

barplot(tabela_11,
        beside = TRUE,
        legend = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        ylim = c(0, 1600),
        main = "Pacientes por Raça")

Q2 <- chisq.test(tabela_11)
Q2


