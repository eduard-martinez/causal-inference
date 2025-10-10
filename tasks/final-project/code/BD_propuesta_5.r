#===============================================================
# Generador de datos sintéticos - Proyecto Hogares Solares (Cali)
# Autor: Eduard F. Martínez González
# Fecha: 2025-10-08
#===============================================================

# Paquetes
pacman::p_load(tidyverse)

# Semilla reproducible
set.seed(20251008)

#---------------------------------------------------------------
# 1. Definición de parámetros generales
#---------------------------------------------------------------
n <- 20000             # total de hogares
p_treated <- 0.4       # 8000 tratados / 12000 controles
n_treated <- round(n * p_treated)
n_control <- n - n_treated

#---------------------------------------------------------------
# 2. Crear grupo de tratamiento
#---------------------------------------------------------------
data <- tibble(
  id_vivienda = 1:n,
  tratado = c(rep(1, n_treated), rep(0, n_control))
)

#---------------------------------------------------------------
# 3. Variable de distancia (km)
#---------------------------------------------------------------
# Para tratados: distancia al borde de barrio no tratado
# Distribución sesgada a la izquierda (más cerca del borde)
dist_tratados <- rbeta(n_treated, shape1 = 2, shape2 = 5) * 1.5   # entre 0 y 1.5 km

# Para controles: distancia al barrio tratado más cercano
# También sesgada a la izquierda, concentrando 80% entre 0 y 1 km
dist_controles <- rbeta(n_control, shape1 = 2, shape2 = 4) * 1.5

data <- data %>%
  mutate(distancia_km = ifelse(tratado == 1, dist_tratados, dist_controles))

#---------------------------------------------------------------
# 4. Variables socioeconómicas base
#---------------------------------------------------------------
data <- data %>%
  mutate(
    ingreso_mensual = rlnorm(n, meanlog = log(1200000), sdlog = 0.4),  # ingreso base
    num_personas = pmax(1, round(rnorm(n, mean = 4.2, sd = 1.2))),
    mujeres = pmax(0, round(num_personas * runif(n, 0.45, 0.55))),
    cabeza_mujer = rbinom(n, 1, 0.45),
    personas_leen = pmin(num_personas, round(num_personas * runif(n, 0.7, 1))),
    comidas_dia = round(rnorm(n, mean = 2.8, sd = 0.3))
  )

#---------------------------------------------------------------
# 5. Efectos de tratamiento sobre outcomes
#---------------------------------------------------------------
# Gasto en energía (como % del ingreso mensual)
# Controles: entre 15% y 20% del ingreso
# Tratados: 12% menos en promedio

data <- data %>%
  mutate(
    gasto_energia_pct = ifelse(tratado == 1,
                               runif(n, 0.13, 0.18),
                               runif(n, 0.15, 0.20)),
    gasto_energia_monto = ingreso_mensual * gasto_energia_pct
  )

# Inclusión financiera: número de cuentas de ahorro
# Tratados: 15% más cuentas en promedio
data <- data %>%
  mutate(
    cuentas_ahorro = ifelse(tratado == 1,
                            rpois(n, lambda = 1.15),
                            rpois(n, lambda = 1.0))
  )

# Monto ahorrado: 10% más para tratados
data <- data %>%
  mutate(
    ahorro_monto = ifelse(tratado == 1,
                          ingreso_mensual * runif(n, 0.08, 0.15),
                          ingreso_mensual * runif(n, 0.06, 0.13))
  )

#---------------------------------------------------------------
# 6. Ajustes finales y verificación
#---------------------------------------------------------------
data <- data %>%
  mutate(
    barrio = ifelse(tratado == 1,
                    sample(1:20, n, replace = TRUE),
                    sample(21:40, n, replace = TRUE))
  )
saveRDS(data,"../github/tasks/final-project/data/BD_propuesta_5.rds")
