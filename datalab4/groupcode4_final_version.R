library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	
getwd()

startups <- read_stata("aggregate_startups.dta")  # read_dta is used for .dta files
patents <- read_stata("aggcount_patents.dta")
startups_by_nace <- read_stata("science_yearly_nace.dta")
patent_categories <- read_stata("aggcount_tech1.dta")

startups_w <- reshape(as.data.frame(startups),
                      idvar = "yr", # which var is id
                      direction = "wide", # how you want things reshaped
                      timevar = "treated",  # which var is repeating
                      sep = "_")

patents_w <- reshape(as.data.frame(patents),
                     idvar = "appyr", # which var is id
                     direction = "wide", # how you want things reshaped
                     timevar = "treated",  # which var is repeating
                     sep = "_")


# graph 1b


ggplot(startups_w) +
  geom_line(aes(x=yr, y=startups_pc_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=yr, y=startups_pc_0, colour = "royalblue"), size = 2) +
  geom_line(aes(x=yr, y=startups_pc_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=yr, y=startups_pc_1, colour = "darkorange2"), size = 2) +
  scale_color_identity(name = "", 
                       breaks = c("darkorange2", "royalblue"), 
                       labels = c("Startups by University Workers", "Startups by Non-Univ. Workers"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year of Foundation") +
  ylab("Startups Founded per Person") +
  ggtitle("University versus Non-University Startups per Worker") +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black", size = .8) +
  theme(legend.position = "right", text = element_text(size = 16), legend.text = element_text(size = 16))


# graph 2b


ggplot(patents_w, aes(x=appyr)) +
  
  geom_line( aes(y=patents_pc_1), size=0.8, color='darkorange2') + 
  geom_point(aes(y=patents_pc_1, colour = "darkorange2"), size = 2)+
  geom_line( aes(y=patents_pc_0*52), size=0.8, color='royalblue') +
  geom_point(aes(y=patents_pc_0*52, colour = "royalblue"), size = 2)+
  scale_color_identity(name = "", 
                       breaks = c("darkorange2", "royalblue"), 
                       labels = c("Patents by University Workers", "Patents by Non-Univ. Workers"), 
                       guide = "legend") +
  theme_bw() +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Patents per University Worker",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./52,name="Patents per Non-University Worker")
  ) + 
  ggtitle("University versus Non-University Patents per Worker")+
  scale_x_continuous("Year of Patent Application",labels = as.character(patents_w$appyr), breaks = patents_w$appyr)+
  geom_vline(xintercept = 2003,color = "black",linetype = "dotted", size = .8) + 
  theme(legend.position = "right", text = element_text(size = 16), legend.text = element_text(size = 16))





#2*2table
startups_table <- filter(startups_w, yr %in% c('2002','2003'))
startups_table <- startups_table[c('yr','startups_pc_0','startups_pc_1')]
names(startups_table)[names(startups_table) == 'yr'] <- 'foundation year'
names(startups_table)[names(startups_table) == 'startups_pc_0'] <- 'start-ups per capita for Univ.professors'
names(startups_table)[names(startups_table) == 'startups_pc_1'] <- 'start-ups per capita for outside Univ. workers'


patents_table <- filter(patents_w, appyr %in% c('2002','2003'))
patents_table <- patents_table[c('appyr','patents_pc_0','patents_pc_1')]
names(patents_table)[names(patents_table) == 'appyr'] <- 'application year'
names(patents_table)[names(patents_table) == 'patents_pc_0'] <- 'patents per capita for Univ.professors'
names(patents_table)[names(patents_table) == 'patents_pc_1'] <- 'patents per capita for outside Univ. workers'






##diff in diff


######sample 0 and 2


startups_by_nace_sample02 <- startups_by_nace %>% filter(sample %in% c(0,2) &
                                                           stiftaar %in% c(2002,2003)) %>%
  mutate(startups_p100k = n_startups/n_workers * 100000) %>%
  select(sample, stiftaar, n1, startups_p100k)
startups_by_nace_sample02 <- reshape(as.data.frame(startups_by_nace_sample02),
                                     idvar = c("sample", "n1"), 
                                     direction = "wide", 
                                     timevar = "stiftaar",  
                                     sep = "_")
startups_by_nace_sample02 <- startups_by_nace_sample02 %>% 
  arrange(sample, n1) %>%
  mutate(perc_change = startups_p100k_2003/startups_p100k_2002 - 1) %>%
  mutate(perc_change = replace_na(perc_change, 0)) 

# assumptions: percentage change is normally distributed, 
# identically for all naca fields within the groups (for example, all )
t.test(perc_change ~sample, 
       data = startups_by_nace_sample02, 
       paired = FALSE)


######sample 0 and 4


startups_by_nace_sample04 <- startups_by_nace %>% filter(sample %in% c(0,4) &
                                                           stiftaar %in% c(2002,2003)) %>%
  mutate(startups_p100k = n_startups/n_workers * 100000) %>%
  select(sample, stiftaar, n1, startups_p100k)
startups_by_nace_sample04 <- reshape(as.data.frame(startups_by_nace_sample04),
                                     idvar = c("sample", "n1"), 
                                     direction = "wide", 
                                     timevar = "stiftaar",  
                                     sep = "_")
startups_by_nace_sample04 <- startups_by_nace_sample04 %>% 
  arrange(sample, n1) %>%
  mutate(perc_change = startups_p100k_2003/startups_p100k_2002 - 1) %>%
  mutate(perc_change = replace_na(perc_change, 0)) 

# assumptions: percentage change is normally distributed, 
# identically for all naca fields within the groups (for example, all )
t.test(perc_change ~sample, 
       data = startups_by_nace_sample04, 
       paired = FALSE)


#####t-test for patents


patent_sample <- patent_categories %>% filter(appyr %in% c(2002,2003)) %>%
  select(treated, appyr, techdum1, patents_pc)
patent_sample <- reshape(as.data.frame(patent_sample),
                         idvar = c("treated",'techdum1'), 
                         direction = "wide", 
                         timevar = "appyr",  
                         sep = "_")
patent_sample <- patent_sample %>% 
  arrange(treated,techdum1) %>%
  mutate(perc_change = patents_pc_2003/patents_pc_2002 - 1) %>%
  mutate(perc_change = replace_na(perc_change, 0)) 

t.test(perc_change ~treated, 
       data = patent_sample, 
       paired = FALSE)




##regression 
startups_by_nace <- startups_by_nace %>%
  # n_workers reflects the num. of workers in a given group
  # (for sample ==0,, it's "University researchers)
  mutate(startups_p100k = n_startups/n_workers * 100000) 


patent_regression <- patent_categories%>%
  filter(appyr %in% c(2000,2001,2002,2003,2004,2005,2006,2007))

patent_regression$post <- ifelse(patent_regression$appyr < 2003, 0, 1)
patent_regression$treated_post <- ifelse (patent_regression$appyr >=2003 & patent_regression$treated == 1,  1, 0)

patent_regression <- patent_regression%>%
  mutate(patent_p100k = patents_pc * 100000)

reg1 <- felm(startups_p100k ~ treated + after + after_treated, #no fixed efects
             data = startups_by_nace %>% filter(sample %in% c(0,2))) # sample==2 is workers outside unis
reg1.5 <- felm(startups_p100k ~ treated + after + after_treated|
                 n1, #field fixed effects
               data = startups_by_nace %>% filter(sample %in% c(0,2)))

reg1.75 <- felm(startups_p100k ~ treated + after_treated|
                  n1+after, #time & field fixed effects
                data = startups_by_nace %>% filter(sample %in% c(0,2)))

reg2 <- felm(startups_p100k ~ treated + after + after_treated | 
               n1, # field fixed effects
             data = startups_by_nace %>% filter(sample %in% c(0,4))) # sample==4 is workers with PhD

reg3 <-  felm(patent_p100k ~ treated + post + treated_post | 
                techdum1, # field fixed effects
              data = patent_regression)


# a summary that is easier to read
huxreg(reg1,reg1.5, reg2, reg3.5,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

# make it pretty and export 
# (other formats are supported, not just excel)
huxreg(reg1,reg1.5, reg1.75,reg2, reg3,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       coefs = c("Treated X post-2003" = "after_treated", # rename vars to smthing pretty
                 "Post-2003" = "after",
                 "Treated" = "treated",
                 "Patent_post_2003" = "post",
                 "Patent_treat&post"="treated_post"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("Year FE", "no", "no",'yes','no','no'),
                 c("Field FE", "no", "yes",'yes','yes','yes'),
                 c("Control group", "All workers",'All workers',"All workers", "With PhD","All workers")
  ), 
  copy_cell_props = FALSE,
  after = c(nrow(.) - 3)) %>% 
  quick_docx("report_reg1.docx")

