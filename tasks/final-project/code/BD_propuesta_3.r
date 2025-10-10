# ==============================================================
# Propuesta 3 – Conflicto y continuidad escolar (base sintética)
# Panel colegio–año (2014–2018), 1,121 municipios
# ==============================================================
pacman::p_load(tidyverse)

set.seed(20251009)

# ----- Universo municipal y años -----
n_mun  <- 1121
years  <- 2014:2018

mun_df <- tibble(
  mun_id = 1:n_mun,
  # Prevalencia ~30% de presencia histórica FARC
  farc_presence_pre = rbinom(n_mun, 1, 0.30),
  # Tamaño municipal para escalamiento de violencia/desplazamiento
  pop_mun = round(pmax(15000, rlnorm(n_mun, log(80000), 0.6)))
)

# ----- Número de colegios por municipio -----
# Más colegios en municipios más poblados
mun_df <- mun_df %>%
  mutate(n_schools = pmax(1, rpois(n_mun, lambda = 6 + log1p(pop_mun)/2))) 

school_df <- mun_df %>%
  rowwise() %>%
  mutate(school_ids = list(paste0("S", mun_id, "_", seq_len(n_schools)))) %>%
  unnest(school_ids) %>%
  rename(school_id = school_ids) %>%
  ungroup()

# ----- Panel colegio–año -----
panel <- tidyr::expand_grid(
  school_id = school_df$school_id,
  year = years
) %>%
  left_join(select(school_df, school_id, mun_id), by = "school_id") %>%
  left_join(select(mun_df, mun_id, farc_presence_pre, pop_mun), by = "mun_id") %>%
  mutate(
    post_2016 = as.integer(year >= 2017),  # dos años post (2017-2018)
    # Atributos del colegio
    urban = rbinom(n(), 1, 0.65),
    private = rbinom(n(), 1, 0.25),
    jornada_mt = rbinom(n(), 1, 0.5),             # 1=mañana, 0=tarde (codificación simple)
    modalidad_presencial = rbinom(n(), 1, 0.9)
  )

# ----- Shocks de violencia municipales (anuales) -----
# Más violencia en tratados post; también ruido idiosincrático
viol_df <- panel %>%
  group_by(mun_id, year) %>%
  summarise(
    base_vio = 15 + 10 * first(farc_presence_pre) + rnorm(1, 0, 5),
    vio_bump = if_else(year >= 2017 & first(farc_presence_pre) == 1, 10, 0),
    violence_rate = pmax(0, base_vio + vio_bump + rnorm(1, 0, 3)),
    kidnap_rate   = pmax(0, rpois(1, lambda = 0.2 + 0.1*first(farc_presence_pre) + 0.2*(year>=2017))),
    disp_rate     = pmax(0, rlnorm(1, log(30 + 10*first(farc_presence_pre) + 8*(year>=2017)), 0.4))
  ) %>%
  ungroup()

panel <- panel %>%
  left_join(viol_df, by = c("mun_id","year"))

# ----- Controles del colegio -----
panel <- panel %>%
  group_by(school_id) %>%
  mutate(
    teachers = pmax(5, round(rnorm(n(), mean = 25 + 10*private + 5*urban, sd = 6))),
    students = pmax(40, round(rnorm(n(), mean = 400 + 120*urban + 100*private, sd = 60))),
    ipm_pdet = pmin(1, pmax(0, 0.45 + 0.15*(1-urban) + rnorm(n(), 0, 0.06))),
    saber11_avg = pmax(150, round(rnorm(n(), mean = 260 + 12*private + 8*urban, sd = 20)))
  ) %>%
  ungroup()

# ----- Efectos en outcomes educativos -----
# Efecto DiD calibrado:
#   treated = farc_presence_pre == 1
#   post    = post_2016 == 1
treated_post <- with(panel, as.integer(farc_presence_pre == 1 & post_2016 == 1))

# Baselines (probabilidades/tasas en [0,1])
panel <- panel %>%
  mutate(
    # Base de matrícula neta (mejor en urbano/privado)
    enrollment_base = plogis(qlogis(0.75) + 0.10*urban + 0.06*private - 0.08*ipm_pdet),
    # Deserción y repetición base mayores con violencia e IPM
    dropout_base    = plogis(qlogis(0.08) + 0.015*violence_rate + 0.10*ipm_pdet - 0.05*private),
    repetition_base = plogis(qlogis(0.07) + 0.010*violence_rate + 0.06*ipm_pdet - 0.03*urban),
    # Permanencia como complemento, ajustada
    permanence_base = pmin(0.99, pmax(0.50, 1 - dropout_base - repetition_base + rnorm(n(),0,0.01)))
  )

# Shocks idiosincráticos
e_enr <- rnorm(nrow(panel), 0, 0.01)
e_drp <- rnorm(nrow(panel), 0, 0.005)
e_rep <- rnorm(nrow(panel), 0, 0.004)
e_prm <- rnorm(nrow(panel), 0, 0.01)

# Aplicamos efecto DiD objetivo (tratados x post):
#   enrollment:  -0.03
#   dropout:     +0.015
#   repetition:  +0.010
#   permanence:  -0.02
panel <- panel %>%
  mutate(
    enrollment_rate  = pmin(0.99, pmax(0.30, enrollment_base  + (-0.03)*treated_post + e_enr)),
    dropout_rate     = pmin(0.25, pmax(0.00, dropout_base     + ( 0.015)*treated_post + e_drp)),
    repetition_rate  = pmin(0.30, pmax(0.00, repetition_base  + ( 0.010)*treated_post + e_rep)),
    permanence_rate  = pmin(0.99, pmax(0.30, permanence_base  + (-0.02)*treated_post + e_prm))
  )

# ----- Exportación -----
out <- panel %>%
  select(school_id, mun_id, year, urban, private, jornada_mt, modalidad_presencial,
         farc_presence_pre, post_2016,
         violence_rate, kidnap_rate, disp_rate, ipm_pdet,
         teachers, students, saber11_avg,
         enrollment_rate, dropout_rate, repetition_rate, permanence_rate)

saveRDS(out,"../github/tasks/final-project/data/BD_propuesta_3.rds")
