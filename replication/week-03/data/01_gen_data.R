## ------------------------------------------------------------
## Datos sintéticos estilo Angrist & Krueger (1991) + 2SLS
## ------------------------------------------------------------
## Paquetes (descomenta si no los tienes):

## 
rm(list=ls())
library(dplyr)
set.seed(1234)

## Tamaño de muestra y dominios
N      <- 50000
yob    <- sample(1930:1939, N, replace = TRUE)         # año de nacimiento
qob    <- sample(1:4, N, replace = TRUE)               # trimestre (1-4)
state  <- sample(1:30, N, replace = TRUE)              # estado de nacimiento (30 ficticios)

## Controles “observables”
black   <- rbinom(N, 1, 0.12)                          # ~12% afrodescendiente
married <- rbinom(N, 1, 0.75)
smsa    <- rbinom(N, 1, 0.65)

## Edad (en 1980), medida con algo de detalle por trimestre
age_qtrs <- (1980 - yob) + (qob - 2.5)/4               # años + fracción por trimestre
age2     <- age_qtrs^2

## -------------------------------------------
## Mecanismo de primera etapa (educación)
## -------------------------------------------
## Hechos estilizados:
## - Tendencia suave al alza de educación por cohorte (1930->1939)
## - Q1 < Q2 < Q3 < Q4 en escolaridad (pequeñas brechas)
## - Permitir que el efecto por trimestre varíe por cohorte (interacciones útiles)
## - Algo de heterogeneidad por estado

## Efecto base por trimestre (en años)
qob_base <- c(-0.10, -0.05, -0.02, 0.00)               # respecto a Q4≈0

## Pequeña variación por cohorte para fortalecer QOB×YOB
qob_yob_slope <- c(-0.005, -0.003, -0.001, 0.000)      # por año desde 1930

## Efecto por estado (heterogeneidad leve)
state_eff <- rnorm(30, 0, 0.02)                        # muy pequeño

## Tendencia por cohorte (más educación en nacidos después)
trend_yob <- 0.035 * (yob - 1930)

## Ruido idiosincrático en educación
eps_edu <- rnorm(N, 0, 0.35)

## Construcción de educación
edu <- 12.6 +
  trend_yob +                                         # tendencia por cohorte
  qob_base[qob] +                                     # efecto base por trimestre
  qob_yob_slope[qob] * (yob - 1930) +                 # interacción QOB×YOB
  state_eff[state] +                                  # heterogeneidad por estado
  (-0.25)*black + 0.10*married + 0.05*smsa +          # correlaciones plausibles
  eps_edu

## Truncar a [8, 20] años (para mantener plausibilidad)
edu <- pmin(pmax(edu, 8), 20)

## -------------------------------------------
## Ecuación salarial “verdadera” (segunda etapa)
## -------------------------------------------
## Retorno causal (≈ 7%): log(wage) = beta*edu + controles + ruido
beta_true <- 0.075

## Controles con signos razonables; sin efecto directo de QOB (exclusión)
u <- rnorm(N, 0, 0.25)

ln_wage <- 1.8 +
  beta_true * edu +
  (-0.25)*black + 0.18*married + 0.16*smsa +
  0.12*age_qtrs - 0.001*age2 +                        # perfil de edad casi plano 40–49
  u

## Base final
ak91_synth <- data.frame(
  ln_wage, edu, yob, qob, state, black, married, smsa, age_qtrs, age2
)

## Factores (como en el paper: dummies de cohorte y opcionalmente estado)
ak91_synth <- ak91_synth %>%
  mutate(
    yob_f   = factor(yob),
    qob_f   = factor(qob, levels = 1:4, labels = c("Q1","Q2","Q3","Q4")),
    state_f = factor(state)
  )

## export data
saveRDS(ak91_synth,"data/datos.rds")

