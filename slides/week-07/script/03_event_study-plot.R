#==============================================================================#
# Author: Eduard-Martinez
# update: 02-05-2022
# R version 4.1.1 (2021-08-10)
#==============================================================================#

## initial configuration
cat("\f")
rm(list=ls())
source("00_program/packages.R")

##=== Extensive Margin ===##

## load data
df <- import("03_make_panel_data/output/panel_grids_regressions.rds") %>% 
      subset(year %in% 2011:2021 & sample_control_1==1)  %>% 
      mutate(treated=d_predio_in_grid ,
             runtime=case_when(treated==1~year-as.numeric(year_conf),.default=-1),
             interaction =interaction(runtime,treated)) %>%
      select(id_grid,year,year_conf,provincia,crops,treated,runtime,interaction)

## gen dummy
df <- dummy_cols(df, select_columns = "interaction", remove_selected_columns = TRUE)
df <- df %>% select(-ends_with(".0"),-"interaction_-1.1")
names(df) <- gsub(".1","",names(df)) %>% gsub("interaction_","interaction",.) %>% gsub("interaction-","interaction_",.)
names(df)[names(df)=="interaction"] <- "interaction1"

## estimation
modelo <- as.formula(paste0("crops~", 
                            paste0('interaction_',abs(min(df$runtime)):2,collapse="+"),"+",
                            paste0('interaction',0:abs(max(df$runtime)),collapse="+"),
                            "+ as.factor(provincia):as.factor(year) | id_grid + year | 0 | id_grid"))

reg <- felm(formula=modelo , data=df) %>% tidy() %>%
       subset(str_detect(term,'interaction')) %>% 
       select(term,estimate,std.error) %>%
       mutate(term = gsub("interaction","",term) %>% gsub("_","-",.) %>% as.numeric(),
              ci_lower=estimate-1.96*std.error,
              ci_upper=estimate+1.96*std.error)

## Event Study Plot
ggplot(data=reg, mapping=aes(y=estimate, x=term)) +
geom_hline(yintercept=0, colour="gray60", linetype="solid", alpha=1, size=0.75) +
geom_vline(xintercept=-1, colour="red", linetype="dashed", alpha=1, size=0.5) +
geom_errorbar(width=.05, aes(ymin=ci_lower, ymax=ci_upper , color="CI 95") , show.legend=F , alpha=1) +
geom_point(mapping=aes(fill="Coef") , shape=21 , size=2 , show.legend=F , alpha=1) +
scale_color_manual(name="", values=c("CI 95"="darkblue")) +
scale_fill_manual(name="", values=c("Coef"="darkblue")) +
scale_x_discrete(limits=unique(round(reg$term))) +
theme_bw() + 
theme(title=element_text(size=16, face="bold") , plot.title=element_text(hjust = 0.5), 
      axis.text.x=element_text(size=12, face="bold") , axis.title.x=element_text(size=13, face="plain") ,
      axis.text.y=element_text(size=12, face="bold") , axis.title.y=element_text(size=13, face="plain") ,
      legend.position="bottom" , legend.text=element_text(size=12)) +
labs(y="Effect Size", x="Years to Land Titling" , legend="" , caption="") + 
guides(color = guide_legend(override.aes = list(linetype = "solid", shape = NA)),      # Para la línea (barras de error)
       fill = guide_legend(override.aes = list(linetype = 0, shape = 21)))             # Para los puntos (estimaciones)
ggsave(file="../../Aplicaciones/Overleaf/FxS_Crops/figures/main_regressions/event_study_main-reg_crops.png" , width=7, height=4.5)


