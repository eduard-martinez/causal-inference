# ============================================================
# Script: synthetic_serpilo_psm_did.R
# Autor: Eduard F. Martínez González
# Objetivo: Aplicar PSM y PSM–DiD sobre base de datos tipo "Ser Pilo Paga"
# ============================================================

## initial setup
rm(list=ls())
require(pacman)
p_load(tidyverse , rio , MatchIt , broom)

##===========================##
## Propensity Score Matching ##
##===========================##

## load data
base_psm <- import("https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/slides/week-08/data/base_psm.rds") %>%
            as_tibble()

##==: 1. Estimar y evaluar el soporte común

## Obtener el soporte comun
m_psm <- matchit(formula = beneficiario ~ puntaje_saber11 + estrato + colegio_privado + mujer,
                 data     = base_psm,    # base de datos
                 method   = "nearest",   # método de emparejamiento: vecino más cercano (1 a 1)
                 distance = "logit",     # estima el propensity score con un modelo logit
                 replace  = TRUE,        # permite reutilizar un mismo control para varios tratados
                 caliper  = 0.2,         # distancia máxima permitida en el PS (evita matches lejanos)
                 discard  = "both"       # descarta observaciones fuera del soporte común
)

## Al ejecutar summary(m_psm):
##   - Verás cuántas observaciones quedaron emparejadas.
##   - Evalúa si las diferencias de medias (Std. Mean Diff.) disminuyeron
##     después del emparejamiento.
summary(m_psm)  

## obtener probabilidades 
base_psm$p_treated <- m_psm$distance

## Pintar propensity score
base_psm %>%
mutate(grupo = ifelse(beneficiario == 1, "Tratado", "Control")) %>%
ggplot(aes(x = p_treated, fill = grupo)) +
geom_density(alpha = 0.35) +
labs(title = "Propensity Score (antes del matching)",
     subtitle = "Distribución de la probabilidad de ser beneficiario",
     x = "Propensity score (probabilidad estimada de tratamiento)",
     y = "Densidad") +
theme_minimal(base_size = 12)

##==: 2. Estimar el ATT

## obtener la base de datos
matched_psm <- match.data(m_psm)

## Pintar propensity score
matched_psm %>%
mutate(grupo = ifelse(beneficiario == 1, "Tratado", "Control")) %>%
ggplot(aes(x = distance, fill = grupo)) +
geom_density(alpha = 0.35) +
labs(title = "Propensity Score (despues del matching)",
     subtitle = "Distribución de la probabilidad de ser beneficiario",
     x = "Propensity score (probabilidad estimada de tratamiento)",
     y = "Densidad") +
theme_minimal(base_size = 12)

## efecto
att_psm <- matched_psm %>%
           group_by(beneficiario) %>%
           summarise(mean_ingreso = mean(ingreso_uni)) %>%
           summarise(ATT = diff(mean_ingreso))
att_psm

##==================================##
## Propensity Score Matching + DinD ##
##==================================##

## load data
base_panel <- import("https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/slides/week-08/data/base_panel.rds") %>%
              as_tibble()

##==: 1. Calcular cambio individual (ΔY)
delta <- base_panel %>%
         pivot_wider(names_from = year, values_from = ingreso, names_prefix = "t") %>%
         mutate(delta = t1 - t0) %>%
         select(id, beneficiario, puntaje_saber11, estrato, colegio_privado, mujer, delta)

##==: 2. Emparejar por PSM sobre los observables (mismo método que antes)
m_psm_did <- matchit(formula = beneficiario ~ puntaje_saber11 + estrato + colegio_privado + mujer,
                     data = delta,
                     method = "nearest",
                     distance = "logit",
                     replace = F,
                     caliper = 0.2,
                     discard = "both")

## Diagnóstico
summary(m_psm_did)

##==: 3. Soporte común (opcional: plot con ggplot)
delta$p_treated <- m_psm_did$distance

delta %>%
mutate(grupo = ifelse(beneficiario == 1, "Tratado", "Control")) %>%
ggplot(aes(x = p_treated, fill = grupo)) +
geom_density(alpha = 0.35) +
labs(title = "Propensity Score (PSM-DiD)",
     subtitle = "Distribución de la probabilidad de ser beneficiario (antes del matching)",
     x = "Propensity score (probabilidad estimada de tratamiento)",
     y = "Densidad") +
theme_minimal(base_size = 12)

##==: 4. Base emparejada

## get base
matched_did <- match.data(m_psm_did)

matched_did %>%
mutate(grupo = ifelse(beneficiario == 1, "Tratado", "Control")) %>%
ggplot(aes(x = distance, fill = grupo)) +
geom_density(alpha = 0.35) +
labs(title = "Propensity Score (PSM-DiD)",
     subtitle = "Distribución posterior al matching",
     x = "Propensity score (probabilidad estimada de tratamiento)",
     y = "Densidad") +
theme_minimal(base_size = 12)

##==: 5. Estimar ATT (sobre el cambio ΔY)
att_psm_did <- matched_did %>%
               group_by(beneficiario) %>%
               summarise(mean_delta = mean(delta)) %>%
               summarise(ATT_DID = diff(mean_delta))

att_psm ; att_psm_did

