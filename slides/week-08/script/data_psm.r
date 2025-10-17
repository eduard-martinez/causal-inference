# ============================================================
# Script: synthetic_serpilo_psm_did.R  (versión corregida)
# Autor: Eduard F. Martínez González
# Objetivo: Simular base tipo "Ser Pilo Paga"
# ============================================================

rm(list=ls())
require(pacman)
p_load(tidyverse, rio)

set.seed(101)

# ------------------------------------------------------------
# 1. Generar base sintética
# ------------------------------------------------------------
n <- 2000

# Covariables pre-programa
puntaje_saber11 <- rnorm(n, mean = 250, sd = 30)
estrato <- sample(1:6, n, replace = TRUE, prob = c(0.25,0.25,0.25,0.15,0.07,0.03))
colegio_privado <- rbinom(n, 1, prob = 0.35)
mujer <- rbinom(n, 1, prob = 0.55)

# ------------------------------------------------------------
# Probabilidad de ser beneficiario (centrada)
# ------------------------------------------------------------
# Ajustamos intercepto y varianza del ruido para centrar el soporte común
linpred <- -3 + 0.015 * puntaje_saber11 - 0.25 * estrato - 0.3 * colegio_privado + rnorm(n, 0, 1.0)
p_benef <- plogis(linpred)

beneficiario <- rbinom(n, 1, p_benef)

# Diagnóstico rápido
summary(p_benef)
hist(p_benef, breaks = 30, col = "gray80", border = "white")

# ------------------------------------------------------------
# Resultados potenciales
# ------------------------------------------------------------
Y0 <- 0.3 + 0.001 * puntaje_saber11 + 0.05 * colegio_privado + 
  0.04 * mujer - 0.03 * estrato + rnorm(n, 0, 0.05)
Y1 <- Y0 + 0.10   # efecto causal = +10 pp

ingreso_uni <- ifelse(beneficiario == 1, Y1, Y0)

# Base final
base_psm <- tibble(
  id = 1:n,
  puntaje_saber11,
  estrato,
  colegio_privado,
  mujer,
  beneficiario,
  ingreso_uni
)

export(base_psm, "data/base_psm.rds")

# ------------------------------------------------------------
# 2. Simular datos panel (antes / después)
# ------------------------------------------------------------
ingreso_pre  <- 0.25 + 0.001 * puntaje_saber11 + 0.05 * colegio_privado +
  0.04 * mujer - 0.03 * estrato + rnorm(n, 0, 0.05)

ingreso_post <- ingreso_pre + ifelse(beneficiario == 1, 0.10, 0.00) + rnorm(n, 0, 0.05)

base_panel <- tibble(
  id = rep(1:n, each = 2),
  year = rep(c(0, 1), times = n),
  puntaje_saber11 = rep(puntaje_saber11, each = 2),
  estrato = rep(estrato, each = 2),
  colegio_privado = rep(colegio_privado, each = 2),
  mujer = rep(mujer, each = 2),
  beneficiario = rep(beneficiario, each = 2),
  ingreso = c(ingreso_pre, ingreso_post)
)

export(base_panel, "data/base_panel.rds")

