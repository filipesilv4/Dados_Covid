library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(survival)
library(survminer)

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

idadeFaixaEtaria <- cut(datasetcovid$NU_IDADE_N, c(0,19,60,102),
                         labels = c("Jovem", "Adulto", "Idoso"), order = T)

tabela_9 <- table(idadeFaixaEtaria, Obito)
tabela_9

barplot(tabela_9,
        beside = TRUE,
        legend = c("Jovem", "Adulto", "Idoso"),
        args.legend = list(x = "top", bty = "n", ncol = 3),
        col = c("#1E90FF", "#00008B", "#191970"),
        ylim = c(0, 1600),
        main = "Pacientes por Faixa Etária")

Q2 <- chisq.test(tabela_9)
Q2


# Escolaridade X Obito
tabela_10 <- table(datasetcovid$CS_ESCOL_N, Obito)
tabela_10

barplot(tabela_10,
        beside = TRUE,
        legend = c("Sem escolaridade", "Fundamental 1", "Fundamental 2 ", "Médio", "Superior"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FA8072","#E9967A","#FFA07A","#FF7F50","#FF6347"),
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
        col = c("#FFFF00", "#FFD700", "#FFA500", "#FF8C00", "#FF4500"),
        ylim = c(0, 1600),
        main = "Pacientes por Raça")

Q2 <- chisq.test(tabela_11)
Q2


# Gráficos de Sobrevivência de Kaplan-Meier, gráfico de taxa (função) falha acumulada
# e teste logRANK.

attach(datasetcovid)

## UTI
km_1 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ UTI))

ggsurvplot(km_1, risk.table = F,
           palette = c("#FF9E29", "#86AA00"),
           data = datasetcovid)

ggsurvplot(km_1, conf.int = T, 
           palette = c("#FF9E29", "#86AA00"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = datasetcovid)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ UTI, rho=0)

## ASMA
km_2 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ ASMA))

ggsurvplot(km_2, risk.table = F,
           palette = c("#11F872", "#1DF0EC"),
           data = datasetcovid)

ggsurvplot(km_2, conf.int = T, 
                 palette = c("#11F872", "#1DF0EC"),
                 risk.table = F, risk.table.col = "strata",
                 fun = "event", data = datasetcovid)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~  ASMA, rho=0)

##CARDIOPATIA
km_3 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ CARDIOPATI))

ggsurvplot(km_3, risk.table = F,
           palette = c("#0FD99C", "#0FA7D9"),
           data = datasetcovid)

ggsurvplot(km_3, conf.int = T, 
           palette = c("#0FD99C", "#0FA7D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = datasetcovid)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ CARDIOPATI, rho=0)

#DIABETES
km_4 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  DIABETES))

ggsurvplot(km_4, risk.table = F,
           palette = c("#FAC0EE", "#6125D9"),
           data = datasetcovid)

ggsurvplot(km_4, conf.int = T, 
           palette = c("#FAC0EE", "#6125D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = datasetcovid)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ DIABETES, rho=0)

## FAIXA ETÁRIA
km_5 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  idadeFaixaEtaria))

ggsurvplot(km_5, risk.table = F,
           data = idadeFaixaEtaria)

ggsurvplot(km_5, conf.int = T,
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = idadeFaixaEtaria)


survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ idadeFaixaEtaria, rho=0)

## ESCOLARIDADE
km_6 <- datasetcovid %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  CS_ESCOL_N))

ggsurvplot(km_6, risk.table = T,
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           data = datasetcovid)

ggsurvplot(km_6, conf.int = T, 
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = datasetcovid)


survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ CS_ESCOL_N, rho=0)

## Residuos de Schoenfeld
