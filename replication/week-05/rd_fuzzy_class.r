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

## plot
ggplot(df, aes(calificacion , ingreso_usd)) +
geom_point(color="grey40" , size=1.6 , alpha = 0.55) 


  geom_line(data = gridL, aes(z, yhat), linewidth = 1.2, color = "black") +
  geom_line(data = gridR, aes(z, yhat), linewidth = 1.2, color = "black") +
  geom_vline(xintercept = z0, linetype = "dotted", linewidth = 0.8) +
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(x = expression(z[i]), y = "Ingresos (log)") +
  annotate("text", x = z0 + 0.02, y = (0 + 1.1)/2,
           label = sprintf("\u03C4 = 0.2"),
           hjust = 0, size = 4) +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        axis.ticks.length = unit(0.18, "cm"))

##==================
## 2. ITT

## Plot
rdplot(y = df$ingreso_usd , 
       x = df$calificacion ,
       c = 75 , 
       p = 2,
       h = 5, 
       kernel = "triangular")

## Estimation
itt <- rdrobust( , x = x, fuzzy = D, c = corte, ,
                p = 1, vce = "hc0", cluster = df$cod_mpio)
print(frd)



##==================
## 3. LATE



# --- 1) Estimación FRD (Wald local) ------------------------------------------
y <- df$ingreso_usd
x <- df$running
D <- df$tratado
corte <- 0

# FRD con corrección de sesgo (RBC). Sug: clúster por municipio

cat("\n=== FRD: LATE estimado en el umbral ===\n")
cat("tau_F =", round(frd$coef[1], 2), "  (SE RBC:", round(frd$se[1],2), ")\n")
cat("Bandas (h_l, h_r) =", round(frd$bws[1,1:2], 3), "\n\n")

# (Opcional) Forma reducida y 1ª etapa por separado, usando MISMAS bandas
rf  <- rdrobust(y = y, x = x, c = corte, kernel = "triangular", p = 1, vce = "hc0",
                h = frd$bws[1,1:2], cluster = df$cod_mpio)
fs  <- rdrobust(y = D, x = x, c = corte, kernel = "triangular", p = 1, vce = "hc0",
                h = frd$bws[1,1:2], cluster = df$cod_mpio)
cat("Forma reducida (ΔY):", round(rf$coef[1],2), " | 1ª etapa (ΔPr(D=1)):", round(fs$coef[1],3), "\n")
cat("Wald (ΔY/ΔD) con mismas h:", round(rf$coef[1]/fs$coef[1],2), "\n\n")

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