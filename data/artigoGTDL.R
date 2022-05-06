library(GTDL)
library(dplyr)
library(psych)
library(ggplot2)
library(survival)
library(survminer)
library(bbmle)
library(cowplot)

dataset <- SP_covid
View(dataset)

#Transformando em fator as variáveis

dataset$sex <- factor(dataset$sex, labels = c("Masculino", "Feminino"), 
                      levels = c(0,1))

dataset$schooling <- factor(dataset$schooling, 
                            labels = c("Sem escolaridade", "Fundamental 1",
                                       "Fundamental 2", "Medio", "Superior"),
                             levels = 0:4,
                             ordered = T)

dataset$ethnicities <- factor(dataset$ethnicities,
                          labels = c("Branca", "Preta", "Amarela", "Parda",
                                     "Indigena"),
                          levels = c("White", "Black", "Asian", "Pardo",
                                     "Indigineous"))

dataset$obesity <- factor(dataset$obesity,
                            labels = c("Nao", "Sim"),
                            levels = c(0,1))

dataset$asthma <- factor(dataset$asthma,
                       labels = c("Nao", "Sim"),
                       levels = c(0,1))

dataset$diabetes <- factor(dataset$diabetes,
                           labels = c("Nao", "Sim"),
                           levels = c(0,1))

dataset$cardiopathy <- factor(dataset$cardiopathy,
                             labels = c("Nao", "Sim"),
                             levels = c(0,1))

dataset$icu <- factor(dataset$icu,
                      labels = c("Nao", "Sim"),
                      levels = c(0,1))

dataset$age_group <- factor(dataset$age_group,
                             labels = c("Jovem", "Adulto", "Idoso"),
                             levels = c("Young", "Adult", "Elderly"))


glimpse(dataset)
attach(dataset)

#Analise Descritiva

#colocar todos os gráficos ou parte deles na mesma imagem

# tabela cruzada sexo e escolaridade
tabela <- table(sex, schooling)
prop.table(tabela)

#Distribuição e boxplot entre obesidade e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, age_group, summary)
tapply(time_symptoms, age_group, sd)
p1 <- dataset %>% ggplot(aes(x = age_group , y = time_symptoms)) + geom_boxplot()

# Distribuição e boxplot entre obesidade e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, obesity, summary)
tapply(time_symptoms, obesity, sd)
p2 <- dataset %>% ggplot(aes(x = obesity , y = time_symptoms)) + geom_boxplot()

# Distribuição e boxplot entre asma e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, asthma, summary)
tapply(time_symptoms, asthma, sd)
p3 <- dataset %>% ggplot(aes(x = asthma , y = time_symptoms)) + geom_boxplot()


# Distribuição e boxplot entre diabetes e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, diabetes, summary)
tapply(time_symptoms, diabetes, sd)
p4 <- dataset %>% ggplot(aes(x = diabetes , y = time_symptoms)) + geom_boxplot()

# Distribuição e boxplot entre cardiopatia e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, cardiopathy, summary)
tapply(time_symptoms, cardiopathy, sd)
p5 <- dataset %>% ggplot(aes(x = cardiopathy , y = time_symptoms)) + geom_boxplot()

# Distribuição e boxplot entre uti e tempo entre os primeiros sintomas e a data de evolução
tapply(time_symptoms, icu, summary)
tapply(time_symptoms, icu, sd)
p6 <- dataset %>% ggplot(aes(x = icu , y = time_symptoms)) + geom_boxplot()

plot_grid(p1, p2, p3, p4, p5, p6) # figura 1


#UTI X Obitos
obito <- factor(censure,
                labels = c("Sim", "Nao"),
                levels = c(1,0))

tabela_1 <- table(icu, obito)
row.names(tabela_1) <- c("NAO UTI", "UTI")
tabela_1

p1 <- mosaicplot(tabela_1, col = c("aquamarine", "lightblue"), cex = 1.1,
           main = "Pacientes Internados" )


Q2 <- chisq.test(tabela_1)
Q2

# Sexo X Obito
tabela_2 <- table(sex, obito)
row.names(tabela_2) <- c("Masculino", "Feminino")
tabela_2

p2 <- mosaicplot(tabela_2, col = c("lightblue", "red"), cex = 1.1,
           main = "Sexo dos Pacientes Internados" )

Q2 <- chisq.test(tabela_2)
Q2

# Obesidade X Obito
tabela_3 <- table(obesity, obito)
row.names(tabela_3) <- c("Possui_Obesidade", "Nao_Possui_Obesidade")
tabela_3

p3 <- mosaicplot(tabela_3, col = c("#836FFF", "#483D8B"), cex = 1.1,
           main = "Pacientes com Obesidade" )


Q2 <- chisq.test(tabela_3)
Q2


# Diabetes x Obito
tabela_4 <- table(diabetes, obito)
row.names(tabela_4) <- c("Possui_Diabetes", "Nao_Possui_Diabetes")
tabela_4

barplot(tabela_4,
        main = "Pacientes com Diabetes",
        ylim = c(0, 2500),
        col = c("#87CEFA", "#4682B4")
)

Q2 <- chisq.test(tabela_4)
Q2

# Cardiopatia X Obito
tabela_5 <- table(cardiopathy, obito)
row.names(tabela_5) <- c("Possui_Cardiopatia", "Nao_Possui_Cardiopatia")
tabela_5
barplot(tabela_5,
        main = "Pacientes com Cardiopatia",
        ylim = c(0, 2300),
        col = c("#7FFFD4", "#5F9EA0"))

Q2 <- chisq.test(tabela_5)
Q2

# Asma X Obito
tabela_6 <- table(asthma, obito)
row.names(tabela_6) <- c("Possui_Asma", "Nao_Possui_Asma")
tabela_6

barplot(tabela_6,
        beside = T,
        main = "Pacientes com Asma",
        ylim = c(0, 2200),
        col = c("#F4A460", "#FFDEAD"))

Q2 <- chisq.test(tabela_6)
Q2

#Idade X Obito
tabela_7 <- table(age_group, obito)
tabela_7

barplot(tabela_7,
        beside = TRUE,
        legend = c("Jovem", "Adulto", "Idoso"),
        args.legend = list(x = "top", bty = "n", ncol = 3),
        col = c("#1E90FF", "#00008B", "#191970"),
        ylim = c(0, 1550),
        main = "Pacientes por Faixa Etária")

Q2 <- chisq.test(tabela_7)
Q2


# Escolaridade X Obito
tabela_8 <- table(schooling, obito)
tabela_8

barplot(tabela_8,
        beside = TRUE,
        legend = c("Sem escolaridade", "Fundamental 1", "Fundamental 2 ", "Médio", "Superior"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FA8072","#E9967A","#FFA07A","#FF7F50","#FF6347"),
        ylim = c(0, 900),
        main = "Pacientes por Escolaridade")

Q2 <- chisq.test(tabela_8)
Q2


# Raca X Obito
tabela_9 <- table(ethnicities, obito)
tabela_9

barplot(tabela_9,
        beside = TRUE,
        legend = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FFFF00", "#FFD700", "#FFA500", "#FF8C00", "#FF4500"),
        ylim = c(0, 1700),
        main = "Pacientes por Raça")

Q2 <- fisher.test(tabela_9, alternative = "greater", conf.int = TRUE, conf.level
                  = 0.95)
Q2


#Analise de Sobrevivência

# Gráficos de Sobrevivência de Kaplan-Meier, gráfico de taxa (função) falha acumulada
# e teste logRANK.

## UTI
km_1 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ icu))

p1 <- ggsurvplot(km_1, risk.table = F,
           palette = c("#FF9E29", "#86AA00"),
           data = dataset)

ggsurvplot(km_1, conf.int = F, 
           palette = c("#FF9E29", "#86AA00"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(time_symptoms,censure) ~ icu, rho=0)

## ASMA
km_2 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ asthma))

p2 <- ggsurvplot(km_2, risk.table = F,
           palette = c("#11F872", "#1DF0EC"),
           data = dataset)

ggsurvplot(km_2, conf.int = F, 
           palette = c("#11F872", "#1DF0EC"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(time_symptoms,censure) ~ asthma, rho=0)

##CARDIOPATIA
km_3 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ cardiopathy))

p3 <- ggsurvplot(km_3, risk.table = F,
           palette = c("#0FD99C", "#0FA7D9"),
           data = dataset)

ggsurvplot(km_3, conf.int = F, 
           palette = c("#0FD99C", "#0FA7D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(time_symptoms,censure) ~ cardiopathy, rho=0)

#DIABETES
km_4 <- dataset %>% 
  with(survfit(Surv(time_symptoms, censure) ~  diabetes))

p4 <- ggsurvplot(km_4, risk.table = F,
           palette = c("#FAC0EE", "#6125D9"),
           data = dataset)

ggsurvplot(km_4, conf.int = F, 
           palette = c("#FAC0EE", "#6125D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(time_symptoms, censure) ~  diabetes, rho=0)


## ESCOLARIDADE
km_5 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~  schooling))

p5 <- ggsurvplot(km_5, risk.table = F,
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           data = dataset)

ggsurvplot(km_5, conf.int = F, 
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)


survdiff(Surv(time_symptoms,censure) ~ schooling, rho=0)

#Obesidade
km_6 <- dataset %>% 
  with(survfit(Surv(time_symptoms, censure) ~  obesity))

p6 <- ggsurvplot(km_6, risk.table = T,
           palette = c("#F0E257", "#43BAF8"),
           data = dataset)

ggsurvplot(km_6, conf.int = F, 
           palette = c("#F0E257","#43BAF8"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)


survdiff(Surv(time_symptoms, censure) ~  obesity, rho=0)

 # figura

## Residuos de Schoenfeld
cox_covid <- coxph(Surv(time_symptoms,censure) ~ icu + asthma + cardiopathy +
                     diabetes + obesity + age_group +  schooling)

summary(cox_covid)

par(mfrow = c(3,3))
plot(cox.zph(cox_covid))

#-----GTDL--------------------------------------------------------------------#