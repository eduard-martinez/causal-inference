# ==============================================================
# PNIS Synthetic Panel (2010-2018) - 1,121 municipios
# Autor: Eduard F. Martínez G.
# Fecha: 2025-10-09
# Descripción:
#   - Construye suitability_coca (instrumento estructural).
#   - Define treated = 1{ suitability >= p80 } (20% municipios).
#   - Introduce shock PNIS post-2014 en coca_hectareas (mayor en treated).
#   - Genera outcomes: nt_lights, leukemia_rate (con lag), deforestation_rate.
#   - Incluye controles y guarda CSV.
# ==============================================================

# Paquetes
pacman::p_load(tidyverse)

# Semilla reproducible
set.seed(20251009)

# -----------------------------
# 1) Universo y calendario
# -----------------------------
n_mun  <- 1121
years  <- 2010:2018
panel  <- tidyr::expand_grid(mun_id = 1:n_mun, year = years)

# -----------------------------
# 2) Suitability e indicadores de tratamiento
# -----------------------------
# Suitability base (idoneidad estructural para coca): 0-1, sesgada a la izquierda
suitability <- tibble(
  mun_id = 1:n_mun,
  suitability_coca = pmin(1, pmax(0, rbeta(n_mun, shape1 = 2, shape2 = 5)))
)

panel <- panel %>% left_join(suitability, by = "mun_id")

# Tratados = top 20% por suitability (p80)
p80 <- quantile(panel$suitability_coca, probs = 0.80)
treated_df <- tibble(
  mun_id = 1:n_mun,
  treated = as.integer(suitability$suitability_coca >= p80)
)

panel <- panel %>%
  left_join(treated_df, by = "mun_id") %>%
  mutate(
    post_2014 = as.integer(year >= 2014),
    # IV principal (más relevante en post): interacción
    Z_iv = suitability_coca * post_2014
  )

# -----------------------------
# 3) Choques nacionales y FE año/municipio
# -----------------------------
# Shock nacional (mercado/entorno) – suave ciclo + ruido
nat_shock <- tibble(
  year = years,
  nshock = scales::rescale(sin((years - 2008)/2.2) + rnorm(length(years), 0, 0.2), to = c(0, 1))
)

panel <- panel %>% left_join(nat_shock, by = "year")

# Efectos fijos simulados
alpha_i <- rnorm(n_mun, 0, 0.15)              # FE municipio
lambda_t <- rnorm(length(years), 0, 0.10)     # FE año
fe_mun <- tibble(mun_id = 1:n_mun, alpha_i = alpha_i)
fe_yr  <- tibble(year = years, lambda_t = lambda_t)

panel <- panel %>% left_join(fe_mun, by = "mun_id") %>% left_join(fe_yr, by = "year")

# -----------------------------
# 4) Controles y población
# -----------------------------
ctrls <- panel %>%
  group_by(mun_id) %>%
  mutate(
    population = round(pmax(7000, rlnorm(n(), meanlog = log(35000 + 15000 * suitability_coca), sdlog = 0.4))),
    poverty_index = pmin(1, pmax(0, 0.55 + 0.25*(1 - suitability_coca) + rnorm(n(), 0, 0.06))),
    armed_presence = rbinom(n(), 1, prob = pmin(0.7, 0.15 + 0.35 * suitability_coca + 0.15 * nshock)),
    education_rate = pmin(0.98, pmax(0.55, 0.65 + 0.1 * rnorm(n(), 0, 0.5) - 0.05 * poverty_index)),
    violence_rate = pmax(0, rnorm(n(), mean = 20 + 25 * armed_presence + 5 * nshock, sd = 8)),
    health_access = pmin(1, pmax(0, 0.55 + 0.25 * log1p(population) / max(log1p(population)) + rnorm(n(), 0, 0.07)))
  ) %>%
  ungroup()

panel <- ctrls

# -----------------------------
# 5) Coca: niveles y crecimiento con shock PNIS post-2014
# -----------------------------
panel <- panel %>%
  group_by(mun_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Nivel base pre-2014 (más alto si suitability mayor)
    coca_base = pmax(0,
                     10 + 80 * suitability_coca + 20 * nshock + rnorm(n(), 0, 10)
    ),
    
    # Efecto PNIS (incentivo perverso): salto mayor en tratados a partir de 2014
    pnis_bump = post_2014 * (
      15 + 60 * treated + 40 * suitability_coca + 30 * treated * suitability_coca
    ),
    
    # Dinámica AR(1) suave para persistencia
    eps = rnorm(n(), 0, 8),
    coca_hectareas = NA_real_
  ) %>%
  mutate(
    coca_hectareas = {
      ch <- numeric(n())
      for (t in seq_along(ch)) {
        base_t <- coca_base[t] + pnis_bump[t]
        if (t == 1) {
          ch[t] <- pmax(0, base_t + eps[t])
        } else {
          ch[t] <- pmax(0, 0.6 * ch[t-1] + 0.5 * base_t + eps[t])
        }
      }
      ch
    }
  ) %>%
  mutate(
    coca_growth = coca_hectareas - dplyr::lag(coca_hectareas, 1, default = coca_hectareas[1])
  ) %>%
  ungroup()

# -----------------------------
# 6) Outcomes vinculados a coca
# -----------------------------
panel <- panel %>%
  group_by(mun_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Luces nocturnas (proxy actividad): depende de log(pop) y coca
    nt_lights = pmax(0,
                     3.0 + 0.8 * log1p(population) / max(log1p(population)) +
                       0.015 * coca_hectareas + 0.05 * nshock + alpha_i + lambda_t + rnorm(n(), 0, 0.15)
    ),
    
    # Deforestación: respuesta positiva a expansión de coca (en % del área)
    deforestation_rate = pmin(15,
                              pmax(0, 0.4 + 0.02 * coca_hectareas + 0.6 * nshock + 0.5 * armed_presence + rnorm(n(), 0, 0.6)))
  ) %>%
  ungroup()

# Leucemia: efecto pequeño con rezago de 1 año y modulado por acceso en salud (más acceso => más diagnóstico)
panel <- panel %>%
  group_by(mun_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    coca_lag1 = dplyr::lag(coca_hectareas, 1, default = coca_hectareas[1]),
    leukemia_rate = pmax(0,
                         4.5 + 0.006 * coca_lag1 + 0.8 * (1 - health_access) + 0.03 * violence_rate/10 +
                           alpha_i + lambda_t + rnorm(n(), 0, 0.4)
    )
  ) %>%
  ungroup()

# -----------------------------
# 7) Salida y chequeos
# -----------------------------
out <- panel %>%
  select(mun_id, year, suitability_coca, treated, post_2014, Z_iv,
         coca_hectareas, coca_growth, nt_lights,
         leukemia_rate, deforestation_rate,
         population, poverty_index, armed_presence,
         education_rate, violence_rate, health_access,
         alpha_i, lambda_t)

# Carpeta y exportación
if (!dir.exists("data")) dir.create("data")
readr::write_csv(out, "data/pnis_panel_2010_2018.csv")

# Resumen rápido
summary_df <- out %>%
  summarise(
    n = n(),
    n_mun = n_distinct(mun_id),
    min_year = min(year), max_year = max(year),
    treated_share = mean(treated),
    mean_suit = mean(suitability_coca),
    corr_IV_coca = cor(Z_iv, coca_hectareas)
  )

print(summary_df)
table_treated <- out %>% distinct(mun_id, treated) %>% count(treated)
print(table_treated)

# ==============================================================
# 6) Outcomes vinculados a coca (versión con aumento 10% en tratados post 2014)
# ==============================================================

panel <- panel %>%
  group_by(mun_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Luces nocturnas (proxy actividad): depende de log(pop) y coca
    nt_lights = pmax(0,
                     3.0 + 0.8 * log1p(population) / max(log1p(population)) +
                       0.015 * coca_hectareas + 0.05 * nshock + alpha_i + lambda_t + rnorm(n(), 0, 0.15)
    ),
    
    # Deforestación: respuesta positiva a expansión de coca (en % del área)
    deforestation_rate = pmin(15,
                              pmax(0, 0.4 + 0.02 * coca_hectareas + 0.6 * nshock + 0.5 * armed_presence + rnorm(n(), 0, 0.6)))
  ) %>%
  ungroup()

# ==============================================================
# Leucemia: ahora con efecto 10% mayor en tratados post-2014
# ==============================================================
panel <- panel %>%
  group_by(mun_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    coca_lag1 = dplyr::lag(coca_hectareas, 1, default = coca_hectareas[1]),
    
    # Tasa base de leucemia (por 100k)
    leukemia_base = pmax(0,
                         4.5 + 0.006 * coca_lag1 + 0.8 * (1 - health_access) +
                           0.03 * violence_rate / 10 + alpha_i + lambda_t + rnorm(n(), 0, 0.4)
    ),
    
    # Efecto causal simulado: +10% en tratados post 2014
    leukemia_rate = leukemia_base * (1 + 0.10 * treated * post_2014)
  ) %>%
  ungroup()



names(panel)
panel <- panel %>% 
         select(mun_id:nshock,population,poverty_index,
                armed_presence,violence_rate,health_access,
                leukemia_rate,coca_hectareas,deforestation_rate,eps,nt_lights) %>% select(-nshock)


saveRDS(panel,"../github/tasks/final-project/data/BD_propuesta_2.rds")

