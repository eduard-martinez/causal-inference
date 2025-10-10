
## initial setup
rm(list=ls())
require(pacman)
p_load(tidyverse , rio , lfe , fastDummies , broom)

##==: 1. Prepare data

## load data
db <- import(file="https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/slides/week-07/data/panel_mnz.rds" , setclass="data.table")

## Check data

## subset data
data <- db %>%
        subset(time %in% -6:6 & dist_rest>=-0.3 & dist_rest<=0.3) 

##==: 2. Calculo a mano

## Tratados en T=0
t0 <- data %>% 
      subset(treated==0 & post==0) %>% 
      summarise(hurto = round(mean(hurto_per),3))

## Tratados en T=1


## Controles en T=0


## Controles en T=1



##==: 3. Raw-Data

## plot
mutate(data, time=ifelse(time>0,time-1,time)) %>%
group_by(time,treated) %>% 
summarise(var=mean(hurto_per , na.rm=T)) %>%
ggplot(aes(x=time,y=var , color=as.factor(treated))) +
geom_vline(xintercept=-0.5 , colour="#990000" , linetype="dashed" , alpha=1 , size=0.5) +
geom_point(size=1 , show.legend=F) + 
geom_line(size=0.6) +
scale_color_manual(values=c("#999999", "#003366" , "#8B4513")) +
theme_bw() + 
scale_x_continuous(breaks=-6:6) + 
guides(color = guide_legend(nrow=1 , title=NULL)) + 
labs(x = "Months to restriction", y = "N. of Crimes per block (share)")

##==: 4. Estimación 

## tradicional
dd_t <- felm(data=data , hurto_per ~ post*treated) 
summary(dd_t)

## TWFE
dd_fe <- felm(data=data , hurto_per ~ post*treated | unique_id + time) 
summary(dd_fe)

## TWFE + cluster
dd_fe_c <- felm(data=data , hurto_per ~ post*treated | unique_id + time | 0 | unique_id) 
summary(dd_fe_c)

##==: 5. Estudio de Eventos

## estimacion
es_data <- data %>% 
           mutate(time_t=time*treated) %>%
           dummy_cols(select_columns="time_t" , remove_selected_columns=T) %>%
           select(unique_id,hurto_per,time,treated,starts_with("time_t"),-`time_t_-1`,-`time_t_0`)
names(es_data) <- gsub("-","_",names(es_data))

## model
model <- paste0("hurto_per ~ ",paste0('time_t_' , c(-6:-2,1:6) , collapse=" + ")," | unique_id + time") %>% 
         gsub("-","_",.) %>% as.formula()
model 

## estimation
tb <- felm(data=es_data , formula=model) %>% 
      broom::tidy() %>% 
      select(term,estimate,std.error) %>%
      mutate(beta=ifelse(str_detect(term,"time_t"),"Motorcycle Ban",NA) ,
             time=gsub("time_t_","",term) %>% gsub("_","-",.) %>% as.numeric(),
             time=ifelse(time>0,time-1,time), 
             ci_lower=estimate-1.96*std.error,
             ci_upper=estimate+1.96*std.error) 
tb

## plot
ggplot(tb , aes(x=time , y=estimate , color=beta , fill=beta)) +
geom_hline(yintercept=0 , colour="#000000" , linetype="solid" , alpha=1 , size=0.5) +
geom_vline(xintercept=-1 , colour="#990000" , linetype="dashed" , alpha=1 , size=0.5) +
scale_color_manual(values=c("Motorcycle Ban"="#003366")) + 
scale_fill_manual(values=c("Motorcycle Ban"="#003366")) +
geom_errorbar(width=.05, aes(ymin=ci_lower , ymax=ci_upper) , show.legend=F) + 
geom_point(shape=21 , size=2 , show.legend=F) +
scale_x_continuous(breaks = -6:5) +
theme_bw()  +
labs(y="Effect Size" , x="Months to Restriction" , legend=NULL , color="" , fill="")




