##============================
# Author: Eduard-Martinez     
# R version 4.1.1 (2021-08-10)

## initial configuration
rm(list=ls())
require(pacman)
p_load(tidyverse , rio , lfe , fastDummies , broom)

## load data
data <- import("https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/slides/week-07/data/panel_grids_regressions.rds")

## Models
modelo <- as.formula("crops ~ treat | id_grid + year | 0 | id_grid") 

##==: 2. Check data

## D&D year-treated


## D&D year-stager


##==: 3. Raw Data

## Control Group 1
data %>% 
select(year,crops,treat_staggered,d_treated_1) %>%
group_by(d_treated_1,year) %>%
summarise(average=mean(crops)) %>% ungroup() %>% 
ggplot() + 
geom_line(aes(x=year , y=average,group=as.factor(d_treated_1) , colour=as.factor(d_treated_1)),linewidth=0.7) +
geom_point(aes(x=year , y=average,group=as.factor(d_treated_1),colour=as.factor(d_treated_1)) , show.legend=F , shape=17 , size=2) + 
scale_color_manual(name="" , values=c("1"="darkblue","0"="gray") , labels=c("1"="FxS","0"="No FxS")) +
scale_x_continuous(breaks=c(2011:2021) , expand=c(0,0.2)) +
geom_vline(xintercept=2018 , colour = "black", linetype="dashed", alpha=0.6 , size=0.5) +
labs(y='Share of Coca Crops Cultivated (hectares)',x='Year') +
theme_classic()

##==: 3. Estimations

## reg nb D&D
reg_dd <- rename(data , treat=treat_2016) %>% 
          felm(formula=modelo) 
summary(reg_dd)

## reg nb rezago
reg_stag <- rename(data , treat=treat_staggered) %>%
            felm(formula=modelo) 
summary(reg_stag)

##==: 4. Estudio de Eventos


## load data
df <- data %>% 
      mutate(treated=d_predio_in_grid ,
             runtime=case_when(treated==1~year-as.numeric(year_conf),.default=-1),
             interaction = interaction(runtime,treated)) %>%
      select(id_grid,year,year_conf,crops,treated,runtime,interaction)

## gen dummy
df <- dummy_cols(df, select_columns = "interaction", remove_selected_columns = TRUE) %>% 
      select(-ends_with(".0"),-"interaction_-1.1")
names(df) <- gsub(".1","",names(df)) %>% gsub("interaction_","interaction",.) %>% gsub("interaction-","interaction_",.)
names(df)[names(df)=="interaction"] <- "interaction1"

## estimation
modelo <- as.formula(paste0("crops~", 
                            paste0('interaction_',abs(min(df$runtime)):2,collapse="+"),"+",
                            paste0('interaction',0:abs(max(df$runtime)),collapse="+"),
                            "| id_grid + year | 0 | id_grid"))

## estimacion
reg <- felm(formula=modelo , data=df) %>% 
       broom::tidy() %>%
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
labs(y="Effect Size", x="Years to Land Titling" , legend="" , caption="") + 
guides(color = guide_legend(override.aes = list(linetype = "solid", shape = NA)), 
       fill = guide_legend(override.aes = list(linetype = 0, shape = 21)))         






