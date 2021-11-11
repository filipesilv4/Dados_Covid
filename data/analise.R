library(readr)
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

