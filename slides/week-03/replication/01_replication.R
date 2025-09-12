## ============================================================
## Diccionario de variables - Datos sintéticos estilo A&K (1991)
## ============================================================

# ln_wage   : Logaritmo natural del salario semanal.
#             Variable dependiente en la segunda etapa (2SLS).

# edu       : Años de educación completados.
#             Variable endógena en la ecuación de salarios.

# yob       : Año de nacimiento (numérico).
#             Cohortes entre 1930 y 1939 en este dataset ficticio.

# qob       : Trimestre de nacimiento (1 = Ene–Mar, 2 = Abr–Jun, 3 = Jul–Sep, 4 = Oct–Dic).
#             Usado como instrumento para edu.

# state     : Estado de nacimiento (código numérico 1–30).
#             Permite construir interacciones QOB × STATE como instrumentos adicionales.

# black     : Dummy de raza (1 = afrodescendiente, 0 = blanco).
#             Control en las regresiones, se asocia con menores niveles de educación e ingresos.

# married   : Dummy de estado civil (1 = casado, 0 = no casado).
#             Control; correlacionado con mayores ingresos.

# smsa      : Dummy de residencia en zona metropolitana (1 = vive en SMSA, 0 = no).
#             Control de localización.

# age_qtrs  : Edad en 1980 medida en años + fracción de trimestre.
#             Se usa para capturar perfil etario con más precisión.

# age2      : Edad al cuadrado.
#             Permite modelar no linealidad en el perfil de ingresos.

# yob_f     : Versión factor (categórica) de yob.
#             Usada para incluir efectos fijos por cohorte.

# qob_f     : Versión factor de qob con etiquetas "Q1","Q2","Q3","Q4".
#             Usada para interacciones en instrumentos.

# state_f   : Versión factor de state.
#             Usada para efectos fijos y como parte de los instrumentos (QOB × STATE).


##==: 1. Initial setup 

## call packages
rm(list=ls())
require(pacman)
p_load(tidyverse, fixest, broom)

## load data
data <- readRDS("data/datos.rds")

##==: PRIMERA ETAPA:
fs_fit <- lm("edu ~ black + married + smsa + age_qtrs + I(age_qtrs^2) + yob_f + qob_f:yob_f", data = data)
summary(fs_fit)$coefficients[1:8,]


##==: SEGUNDA ETAPA:


##==: FORMA REDUCIDA:


##==: ESTIMADOR DE WALD:











