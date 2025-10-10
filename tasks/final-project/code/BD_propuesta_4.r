# ==============================================================
# Propuesta 4 – Microcrédito y ahorro (corte transversal)
# Diseño: IV (instrumento = distancia a sucursal)
# ==============================================================

rm(list=ls())
pacman::p_load(tidyverse)

set.seed(20251009)

# Tamaño muestral
n <- 8000
municipios <- 1:200

# Estructura base
data <- tibble(
  id_hogar = 1:n,
  municipio = sample(municipios, n, replace = TRUE),
  distance_km = pmin(15, pmax(0.5, rlnorm(n, log(3), 0.6))) # instrumento (km)
)

# Variables socioeconómicas
data <- data %>%
  mutate(
    ingreso_mensual = rlnorm(n, meanlog = log(1200000), sdlog = 0.45),
    educacion_jefe  = round(rnorm(n, 8.5, 3)),
    edad_jefe       = round(runif(n, 25, 70)),
    tamano_hogar    = pmax(1, rpois(n, 4)),
    hectareas_tierra = pmax(0, rlnorm(n, log(1.2), 0.9)),
    mujer_jefe      = rbinom(n, 1, 0.42),
    educacion_financiera = rbinom(n, 1, 0.30)
  )

# ---- Primera etapa: microcredit ~ distancia ----
# Probabilidad de acceso decrece con distancia
p_micro <- plogis(2.5 - 0.35*data$distance_km + 0.25*(data$educacion_financiera) +
                    0.1*scale(data$educacion_jefe) - 0.05*scale(data$edad_jefe))
data <- data %>%
  mutate(microcredit = rbinom(n, 1, p_micro))

# ---- Outcome: ahorro mensual ----
# Efecto causal: +15% para quienes tienen microcrédito
# Efectos de controles incluidos
data <- data %>%
  mutate(
    ahorro_mensual = ingreso_mensual * (
      0.08 + 0.15*microcredit +
        0.01*educacion_financiera +
        0.002*(educacion_jefe - mean(educacion_jefe)) -
        0.001*(tamano_hogar - mean(tamano_hogar)) +
        rnorm(n, 0, 0.02)
    ),
    cuentas_ahorro = pmax(0, rpois(n, lambda = 1.2 + 0.8*microcredit + 0.3*educacion_financiera))
  )

saveRDS(data,"../github/tasks/final-project/data/BD_propuesta_4.rds")


