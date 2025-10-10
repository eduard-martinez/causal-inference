# ==============================================================
# Propuesta 1 – RD temporal: Movilidad vs homicidios (Cali, 2020)
# Panel comuna–día, corte: 2020-03-21
# ==============================================================

rm(list=ls())
# ==============================================================
# Propuesta 1 – RD temporal: Movilidad vs homicidios (Cali, 2020)
# Versión con proxies: transporte público y luces nocturnas
# ==============================================================

pacman::p_load(tidyverse, lubridate)

set.seed(20251009)

# ----- Configuración temporal -----
start_date <- as.Date("2020-02-01")
end_date   <- as.Date("2020-05-23")
cutoff     <- as.Date("2020-03-21")

dates  <- seq(start_date, end_date, by = "day")
n_days <- length(dates)
n_comm <- 22

base <- expand_grid(
  date = dates,
  commune_id = 1:n_comm
) %>%
  mutate(
    day_rel = as.integer(date - cutoff),
    post = as.integer(day_rel >= 0)
  )

# ----- Efectos fijos de comuna -----
comm_eff <- tibble(
  commune_id = 1:n_comm,
  base_lambda = pmax(0.3, rnorm(n_comm, mean = 0.9, sd = 0.25))
)

base <- base %>% left_join(comm_eff, by = "commune_id")

# ----- Clima -----
clima <- tibble(
  date = dates,
  temperature_c = 24 + sin(2*pi*(yday(date))/365) + rnorm(n_days, 0, 0.5),
  rain_mm = pmax(0, rlnorm(n_days, log(3), 0.5) - 2)
)
base <- base %>% left_join(clima, by = "date")

# ----- Proxies de movilidad -----
# Caen abruptamente en el corte
base <- base %>%
  mutate(
    public_transport_users = if_else(
      post == 1,
      round(pmax(0, rnorm(n(), mean = 450, sd = 80))),
      round(rnorm(n(), mean = 1200, sd = 150))
    ),
    night_lights = if_else(
      post == 1,
      pmax(0, rnorm(n(), mean = 12, sd = 2)),
      pmax(0, rnorm(n(), mean = 20, sd = 2))
    )
  )

# ----- Patrullajes -----
base <- base %>%
  mutate(
    police_patrols = pmax(0, round(rnorm(n(), mean = 6 + 1.5*post, sd = 2)))
  )

# ----- Tipo de homicidio -----
base <- base %>%
  group_by(commune_id) %>%
  mutate(
    homicide_type = if_else(runif(n()) < 0.6, "sym", "asym")
  ) %>%
  ungroup()

# ----- Tendencias locales -----
base <- base %>%
  mutate(
    t = day_rel,
    f_left  = if_else(t < 0, -0.002*t + 0.0005*t^2, 0),
    f_right = if_else(t >= 0, -0.001*t + 0.0003*t^2, 0)
  )

# ----- Efecto RD: caída post-corte -----
# caída del 25% promedio en homicidios post
base <- base %>%
  mutate(
    rd_jump = -0.25 * post
  )

# ----- Intensidad lambda -----
base <- base %>%
  mutate(
    lambda_lin = base_lambda +
      0.0005 * public_transport_users + 0.02 * night_lights / 10 -
      0.02 * police_patrols +
      f_left + f_right,
    lambda_jump = lambda_lin * (1 + rd_jump),
    lambda = pmax(0.05, lambda_jump)
  )

# ----- Conteos Poisson -----
base <- base %>%
  mutate(homicides = rpois(n(), lambda = lambda))

# ----- Salida final -----
out <- base %>%
  transmute(
    date, commune_id, day_rel, post,
    homicides, homicide_type,
    public_transport_users, night_lights,
    police_patrols, temperature_c, rain_mm
  )


saveRDS(out,"../github/tasks/final-project/data/BD_propuesta_1.rds")



