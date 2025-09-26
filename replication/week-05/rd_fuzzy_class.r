## Author: Eduard Martinez 
## Date: 25-09-2025

##==================
## 0. Initial Setup 

## clean environment
rm(list=ls())

## packages
require(pacman)
p_load(dplyr,ggplot2,rdrobust)

## load data
source("https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/replication/week-05/data_rd.r")

##==================
## 1. Check Data

## Initial plot
ggplot(df, aes(calificacion , ingreso_usd)) +
geom_point(color="grey40" , size=1.6 , alpha = 0.55) +
geom_vline(xintercept = 75, linetype = "solid", linewidth = 0.8 , color="red") +
theme_classic()

## Distribution
ggplot(df, aes(calificacion)) +
geom_density(color="grey40" , size=1 , alpha = 0.55) +
geom_vline(xintercept = 75, linetype = "solid", linewidth = 0.8 , color="red") +
theme_classic()

## Tablas Cruzadas


##==================
## 2. ITT

## Plot
rdplot(y = df$ingreso_usd , 
       x = df$running ,
       c = 0 , 
       p = 1 ,
       h = 10 ,
       nbins = 2000 , 
       kernel = "triangular")

## OLS Estimation
ols <- lm(ingreso_usd ~ running + elegible , data=df)
summary(ols)

## Estimation no parametrica
itt <- rdrobust(y = df$ingreso_usd , 
                x = df$running , 
                c = 0 ,
                q = 2 , 
                cluster = df$cod_mpio)
summary(itt)

## Number of Obs. 1744 / 2660: observaciones usadas a cada lado con el h seleccionado.
## Eff. Number of Obs. 695 / 736: “n efectivo” tras ponderar con el kernel (menos que el bruto).
## Order est. (p) = 1: polinomio local lineal.
## Order bias (q) = 2: orden usado para corregir sesgo (cuadrático).
## BW est. (h) = 4.131: ancho de banda para estimar el salto.
## BW bias (b) = 6.803: banda (más grande) para estimar términos de sesgo.
## rho (h/b) = 0.607: razón entre ambas bandas (parámetro de RBC).
## Unique Obs. 814 / 1221: valores únicos de la running variable a cada lado (informativo).

##==================
## 3. LATE

## Primera Etapa
fs <- lm(tratado ~ running + elegible , data=df)
summary(fs)

## Estimation 


## Estimation no parametrica
itt <- rdrobust(y = df$ingreso_usd , 
                x = df$running , 
                c = 0 ,
                q = 2 , 
                cluster = df$cod_mpio)
summary(itt)


# --- 2) Gráficos rdplot -------------------------------------------------------
dir.create("plots", showWarnings = FALSE)

# Resultado principal
png("plots/rdplot_ingreso.png", 1200, 850, res = 150)
rdplot(y, x, c = corte, binselect = "qs", p = 1, kernel = "triangular",
       col.dots = "grey40", col.lines = "black",
       x.label = expression(running==orden-10), y.label = "Ingreso (USD)",
       title = "RD plot: ingreso vs running")
dev.off()

# Primera etapa: probabilidad de ser tratado
png("plots/rdplot_tratado.png", 1200, 850, res = 150)
rdplot(D, x, c = corte, binselect = "qs", p = 1, kernel = "triangular",
       col.dots = "grey40", col.lines = "black",
       x.label = expression(running==orden-10), y.label = "Pr(Tratado)",
       title = "RD plot: tratamiento recibido vs running")
dev.off()

# --- 3) Validación de supuestos ----------------------------------------------
# 3.1 Densidad (McCrary)
den <- rddensity(x, c = corte)
print(summary(den))
png("plots/density_running.png", 1200, 850, res = 150)
rdplotdensity(den, x, c = corte)
title("Prueba de densidad (McCrary)")
dev.off()

# 3.2 Balance en covariables predeterminadas (no deberían saltar)
covars <- c("edad","mujer","lee","esc_annos_t0","ingreso_usd_t0",
            "acceso_credito_t0","tam_parcela_ha_t0")
balance_tab <- lapply(covars, function(v){
  out <- rdrobust(y = df[[v]], x = x, c = corte, kernel = "triangular",
                  p = 1, vce = "hc0", cluster = df$cod_mpio)
  c(var = v,
    coef = round(out$coef[1], 3),
    se   = round(out$se[1],   3),
    pval = round(out$pv[1],   3))
}) %>% bind_rows()
print(balance_tab)

# --- 4) Resumen rápido en consola --------------------------------------------
cat("\n--- RESUMEN ---\n")
cat("FRD (RBC): tau_F =", round(frd$coef[1],2), " (se =", round(frd$se[1],2), ")\n")
cat("Primera etapa salto ΔPr(D=1) =", round(fs$coef[1],3), "\n")
cat("Densidad (McCrary) z =", round(summary(den)$test$z, 2),
    "  p-value =", round(summary(den)$test$p_j[1], 3), "\n")
cat("Balance (p-val) - mediana:", median(as.numeric(balance_tab$pval), na.rm = TRUE),
    " | min:", min(as.numeric(balance_tab$pval), na.rm = TRUE), "\n")

# --- 5) (Opcional) Donut RD: excluir |running| < 1 ---------------------------
# Útil si sospechas heaping/manipulación justo en el corte.
df_donut <- df %>% filter(abs(running) >= 1)
frd_donut <- rdrobust(df_donut$ingreso_usd, df_donut$running, fuzzy = df_donut$tratado,
                      c = corte, kernel = "triangular", p = 1, vce = "hc0",
                      cluster = df_donut$cod_mpio)
cat("\nFRD (donut |running|>=1): tau_F =", round(frd_donut$coef[1],2),
    " (se =", round(frd_donut$se[1],2), ")\n")