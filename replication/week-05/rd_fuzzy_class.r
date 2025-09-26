## Author: Eduard Martinez 
## Date: 25-09-2025

##==================
## 0. Initial Setup 

## clean environment
rm(list=ls())

## packages
require(pacman)
p_load(dplyr,ggplot2,rdrobust,fixest)

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
## BW est. (h) = 4.131: ancho de banda para estimar el salto. Viene de Calonico–Cattaneo–Titiunik (CCT).
## BW bias (b) = 6.803: banda (más grande) para estimar términos de sesgo.
## rho (h/b) = 0.607: razón entre ambas bandas (parámetro de RBC).
## Unique Obs. 814 / 1221: valores únicos de la running variable a cada lado (informativo).

##==================
## 3. LATE

## Primera Etapa
fs <- lm(tratado ~ running + elegible , data=df)
summary(fs)

## Estimation 2SLS
reg_2sls <- feols(ingreso_usd ~ 1 | 0 | tratado ~ elegible , data=df)
summary(reg_2sls)

## Estimation no parametrica
late <- rdrobust(y = df$ingreso_usd , 
                 x = df$running , 
                 fuzzy = df$tratado ,
                 c = 0 ,
                 q = 2 , 
                 cluster = df$cod_mpio)
summary(late)

##==================
## 4. Validacion de Supuestos

## Densidad
ggplot(df, aes(calificacion)) +
geom_histogram(color="grey40" , size=1 , alpha = 0.55) +
geom_vline(xintercept = 75, linetype = "solid", linewidth = 0.8 , color="red") +
theme_classic()

## Balance Ingresos t0
ing_t0 <- lm(ingreso_usd_t0 ~ elegible , data=df)
summary(ing_t0)

## Balance: Creditos t0
credi_t0 <- lm(acceso_credito_t0 ~ elegible , data=df)
summary(credi_t0)
