## Data lab #5

library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	


#########
# Part 1: figure
#########

operas <- read_stata("20years.dta")  # read_dta is used for .dta files


operas_c <- operas %>% mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state)) 

### GRAPH ###
# reshape data to wide to make graphs
operas_w <- reshape(as.data.frame(operas_c),
                    idvar = "year", # which var is id
                    direction = "wide", # how you want things reshaped
                    timevar = "treated",  # which var is repeating
                    sep = "_")

operas_w <- operas_w %>% mutate(mean_0 = operas_count_0/regions_0,
                                mean_1 = operas_count_1/regions_1)
# Make two Time Series Graphs (figure 1B below, easy to change for Figure 2B)
ggplot(operas_w) +
  geom_line(aes(x=year, y=mean_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year, y=mean_0, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year, y=mean_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year, y=mean_1, colour = "darkorange2"), size = 1.2) +
  geom_vline(xintercept = 1801, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1794, label="1801 Copyright Law", y=5.5), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("royalblue", "darkorange2"), 
                       labels = c("Operas in regions without copyright", 
                                  "Operas in regions with copyright"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Mean New Operas per Year") +
  ggtitle("Figure 1 - Operas in Italian regions from 1780 to 1820") +
  theme(legend.position = "right", legend.text = element_text(size = 10))

ggsave(paste0("figure1.png"),
       width = 8, height = 6, units = "in")


#########
# Part 2: table
#########


operas_table <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)

opera_total_table <- operas %>%
  filter(year %in% 1781:1820) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)

table1_part1 <- rbind(opera_total_table , operas_table)
#########################

operas_table2 <- operas %>%
  filter(year %in% 1781:1820 & annals == 1) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 20,
         operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)

opera_total_table2 <- operas %>%
  filter(year %in% 1781:1820 & annals == 1) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 40) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)

table1_part2 <- rbind(opera_total_table2 , operas_table2)
#######################

operas_table3 <- operas %>%
  filter(year %in% 1781:1820 & amazon == 1) %>% 
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(yr00_20, treated) %>% 
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 20,
         operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion, yr00_20) %>%
  spread(treated, operas_peryar_perregion)


opera_total_table3 <- operas %>%
  filter(year %in% 1781:1820 & amazon == 1) %>%
  mutate(treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(treated)%>%
  summarise(operas_count = n()) %>%
  mutate(regions = ifelse(treated==0, 6, 2),
         length = 40) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length)) %>%
  select(treated, operas_peryar_perregion) %>%
  spread(treated, operas_peryar_perregion) %>%
  mutate(yr00_20=2)

table1_part3 <- rbind(opera_total_table3 , operas_table3)

final_table1 <- rbind(table1_part1,table1_part2,table1_part3)

final_table1 = final_table1[c(3,2,1)]

final_table1 <- final_table1 %>% rename("Years" = yr00_20, "Other States" = "0", "Venetia and Lombardy" = "1")

final_table1$Years[final_table1$Years == 2] <- "1781-1820"
final_table1$Years[final_table1$Years == 0] <- "1781-1800"
final_table1$Years[final_table1$Years == 1] <- "1801-1820"

#windows system version
final_table1 <- final_table1 %>% mutate(across(where(is.numeric), ~ round(., 3)))
#mac system version
final_table1 <- final_table1 %>% mutate_if(is.numeric, ~round(., 3))


######2c popular operas
popular_opera <- operas %>%
  mutate(popular = annals + amazon) %>%
  filter(year %in% 1781:1820 & popular > 0)
popular_opera_total = length(popular_opera$popular)
popular_opera_amazon = length(popular_opera$popular[popular_opera$amazon == 1 & popular_opera$annals == 0])
popular_opera_annal = length(popular_opera$popular[popular_opera$annals == 1 & popular_opera$amazon == 0])
popular_opera_both = length(popular_opera$popular[popular_opera$amazon == 1 & popular_opera$annals == 1])






#########
# Part 3: regressions
#########
operas_forreg <- operas %>%
  filter(year %in% 1781:1820) %>% #drop the 1821 operas
  mutate(yr00_20 = as.numeric(year>=1801), #year is above 1801
         treated = as.numeric(state %in% c("lombardy", "venetia"))) %>%
  group_by(year, yr00_20, treated, state) %>% 
  summarise(operas_count = n(),
            regions = n_distinct(state),
            length = n_distinct(year)) %>%
  mutate(operas_peryar_perregion = operas_count/(regions*length),
         post_treat = yr00_20*treated) 

operas_template <- operas_forreg %>%select(year,state)

operas_pop <- operas %>% filter(title !='' & first_name !='') %>%
  mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, state) %>% 
  summarise(operas_count = n()) 

operas_fam <- merge(operas_template, operas_pop,
                    by=c('year','state'),
                    all.x = TRUE)

operas_fam <- operas_fam %>% mutate( count_operas = replace_na(operas_count,0))%>%
  mutate(post_treat = yr00_20*treated)



reg <- felm(count_operas ~ post_treat| 
              state +year| # two FEs? use a + to add them
              0,
            data=operas_fam) 
summary(reg)


#3b
operas <- operas %>%
  mutate(pop = annals + amazon)

operas$popular <- ifelse(is.na(operas$pop), NA, operas$pop)
operas$popular <- ifelse(operas$popular>0,1,0)

operas_pop2 <- operas %>% filter(title !='' & (first_name !='' | last_name != '') & popular==1) %>%
  mutate(treated = state %in% c("lombardy", "venetia")) %>%
  mutate(treated = as.numeric(treated)) %>%
  group_by(year, state) %>% 
  summarise(operas_count = n()) 

operas_fam2 <- merge(operas_template, operas_pop2,
                     by=c('year','state'),
                     all.x = TRUE)

operas_fam2 <- operas_fam2 %>% mutate( count_operas = replace_na(operas_count,0))%>%
  mutate(post_treat = yr00_20*treated)

reg2 <- felm(count_operas ~ post_treat| 
               state +year| # two FEs? use a + to add them
               0,
             data=operas_fam2) 
summary(reg2)

#3c
reg3 <- felm(count_operas ~ treated + yr00_20 + post_treat| 
               0| # without 2 FE
               0,
             data=operas_fam) 
summary(reg3)

huxreg(reg, reg2, reg3,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       coefs = c("Treated X post-1801" = "post_treat", # rename vars to smthing pretty
                 "Post-1801" = "yr00_20",
                 "Treated" = "treated"),
       statistics = c("N" = "nobs",
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("Year FE", "yes", "yes","no"), #note! you need more "yes" if you have >3 models
                 c("State FE", "yes","yes", "no"),
                 c("Sample", "Known", "Known & popular", "Known")),
           copy_cell_props = FALSE,
           after = c(nrow(.) - 3))# %>%
# quick_docx("report_reg.docx")