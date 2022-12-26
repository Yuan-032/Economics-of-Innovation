library(tidyverse) 
library(huxtable) 
library(haven)
library(gridExtra)
library(lfe) 
library(reshape2)
library(MASS) 
rm(list= ls())	

getwd()
setwd("C:/Users/yh2741/Desktop")
chem_pat <- read_stata("chem_patents_labelled.dta") 





#question 0 
#a
num_classes <- length(unique(chem_pat$main))
num_subclasses <- length(unique(chem_pat$uspto_class))

#b
sum_subclass <- chem_pat %>% 
  group_by(main) %>%
  summarise(count = n(),
            n_treat = sum(licensed_class),
            any_treat = any(licensed_class))

num_subclasses_treated <- sum(sum_subclass$n_treat)/65
num_subclasses_untreated <- sum(sum_subclass$count)/65 - num_subclasses_treated

# 7*2 table of licensed/non-licensed patents
table <- chem_pat %>%
  group_by(licensed_class,grntyr) %>%
  summarize(n_patent = sum(count_usa)) %>% 
  ungroup
table1 <- table[1:65,]
table2 <- table[66:130,]
table1 <- table1%>%mutate(decade = (grntyr %/% 10)*10) %>%
  group_by(decade) %>%
  summarise(n_patent_untreated = sum(n_patent))


table2 <- table2%>%mutate(decade = (grntyr %/% 10)*10) %>%
  group_by(decade) %>%
  summarise(n_patent_treated = sum(n_patent))

table_seven_by_two <- merge(table1, table2)

table_seven_by_two %>% quick_xlsx(file = 'table2.xlsx')
library(xlsx)
write.xlsx(table_seven_by_two, "C:/Users/yh2741/Deskto/mydata.xlsx")


# time series of licensed/non-licensed patents
table <- chem_pat %>%
  group_by(licensed_class,grntyr)%>%
  count(n_patent = sum(count)) %>%
  ungroup
  
table1 <- table[1:65,]
table2 <- table[66:130,]
table1 <- table1%>%mutate(decade = (grntyr %/% 10)*10) %>%
  group_by(decade) %>%
  summarise(n_patent = sum(n_patent))

table2 <- table2%>%mutate(decade = (grntyr %/% 10)*10) %>%
  group_by(decade) %>%
  summarise(n_patent = sum(n_patent))

table1$data <- 'untreated'
table2$data <- 'treated'
combine_table <- rbind.data.frame(table1, table2)


ggplot(combine_table, aes(x = decade, y = n_patent))+
  geom_line(aes(colour = data), size = 1)+
  geom_point(colour = 'royalblue', size = 2)+
  scale_x_continuous(name="Decades") +
  scale_y_continuous(name="Total number of patents") + 
  ggtitle("The total number of patents in 
          licensed/non-licensed subclasses")+
  theme_bw()


#c
group_by_nation <- chem_pat %>%
  group_by(treat) %>%
  summarise(share_of_usa = sum(count_usa)/sum(count),
            share_of_germany = sum(count_germany)/sum(count),
            share_of_france = sum(count_france)/sum(count)
            )
group_by_nation %>% quick_xlsx(file = 'table3.xlsx')


##Question 1

# treatment method applied to subclasses
chem_pat_treatment <- chem_pat %>%
  group_by(uspto_class) %>%
  summarize(treated = (licensed_class == 1 & years_remaining >= 7)) %>%
  distinct()

# subclass level measurements of change in US patents
chem_pat_diff <- chem_pat %>%
  filter(grntyr >= 1900 & grntyr <= 1939) %>%
  group_by(uspto_class) %>%
  summarize(pre_sum = sum(count_usa[1:20]),
            post_sum = sum(count_usa[21:40])) %>%
  mutate(change_sum = post_sum - pre_sum)

# diff_in_diff setup
chem_pat_diff_in_diff <- merge(chem_pat_treatment,chem_pat_diff)
t.test(change_sum ~ treated,
       data = chem_pat_diff_in_diff,
       paired = FALSE)

# diff_in_diff graphing
chem_pat_diff_graph <- chem_pat %>%
  filter(grntyr >= 1900 & grntyr <= 1939) %>%
  group_by(uspto_class) %>%
  mutate(treated = (licensed_class == 1 & years_remaining >= 7)) %>%
  group_by(treated, grntyr) %>%
  summarize(sum_patents = sum(count_usa)) %>%
  mutate(num_subclasses_type = 0)

chem_pat_diff_graph$num_subclasses_type <- ifelse(chem_pat_diff_graph$treated==TRUE, num_subclasses_treated,num_subclasses_untreated)

chem_pat_diff_graph <- chem_pat_diff_graph %>% mutate(patents_per_subclass = sum_patents / num_subclasses_type)

ggplot(chem_pat_diff_graph, aes(grntyr,patents_per_subclass * 100, group = treated, color = treated), se  = FALSE) +
  geom_line() +
  geom_vline(xintercept = 1919, linetype = "dotted", size  = 1.25) +
  labs(x = "Year", y = "Patents by US Inventors per 100 Subclasses") +
  scale_color_manual(name="Effect by the TWEA",labels=c("Unlicensed", "Licensed"), values=c("blue", "red"))

chem_pat_diff_graph <- spread(chem_pat_diff_graph,treated,sum_patents)
names(chem_pat_diff_graph)[names(chem_pat_diff_graph)=="FALSE"] <- "patents_untreated"
names(chem_pat_diff_graph)[names(chem_pat_diff_graph)=="TRUE"] <- "patents_treated"




#Question 2

#filters out subclasses which had no patents actually available for licensing
chem_pat_available <- chem_pat %>% filter(confiscated_class > 0)

#just table to check all classes are valid
chem_pat_valid_main <- chem_pat_available %>%
  group_by(main) %>%
  summarise(valid_intent = ((sum(licensed_class) != n()) & (sum(licensed_class) != 0)))

#Redo Part 2 using only subclasses with patents available. Again, use two measures of treatment.

#check for number of treated and untreated subclasses in intent to treat set
sum_subclass_available <- chem_pat_available %>% 
  group_by(main) %>%
  summarise(count = n(),
            n_treat = sum(licensed_class),
            any_treat = any(licensed_class))

num_subclasses_available_treated <- sum(sum_subclass_available$n_treat)
num_subclasses_available_untreated <- sum(sum_subclass_available$count) - num_subclasses_available_treated



# treatment method applied to subclasses
chem_pat_available_treatment <- chem_pat_available %>%
  group_by(uspto_class) %>%
  summarize(treated = (licensed_class == 1 & years_remaining >= 7)) %>%
  distinct()

# subclass level measurements of change in US patents
chem_pat_available_diff <- chem_pat_available %>%
  filter(grntyr >= 1900 & grntyr <= 1939) %>%
  group_by(uspto_class) %>%
  summarize(pre_sum = sum(count_usa[1:20]),
            post_sum = sum(count_usa[21:40])) %>%
  mutate(change_sum = post_sum - pre_sum)

# diff_in_diff setup
chem_pat_available_diff_in_diff <- merge(chem_pat_available_treatment,chem_pat_available_diff)
t.test(change_sum ~ treated,
       data = chem_pat_available_diff_in_diff,
       paired = FALSE)

# diff_in_diff graphing
chem_pat_available_diff_graph <- chem_pat_available %>%
  filter(grntyr >= 1900 & grntyr <= 1939) %>%
  group_by(uspto_class) %>%
  mutate(treated = (licensed_class == 1 & years_remaining >= 7)) %>%
  group_by(treated, grntyr) %>%
  summarize(sum_patents = sum(count_usa)) %>%
  mutate(num_subclasses_type = 0)

chem_pat_available_diff_graph$num_subclasses_type <- ifelse(chem_pat_available_diff_graph$treated==TRUE, num_subclasses_available_treated,num_subclasses_available_untreated)

chem_pat_available_diff_graph <- chem_pat_available_diff_graph %>% mutate(patents_per_subclass = sum_patents / num_subclasses_type)

ggplot(chem_pat_available_diff_graph, aes(grntyr,patents_per_subclass * 100, group = treated, color = treated), se  = FALSE) +
  geom_line() +
  geom_vline(xintercept = 1919, linetype = "dashed") +
  labs(x = "Year", y = "Patents by US Inventors per 100 Subclasses") +
  scale_color_manual(name="Effect by the TWEA",labels=c("Unlicensed", "Licensed"), values=c("blue", "red"))

chem_pat_available_diff_graph <- spread(chem_pat_available_diff_graph,treated,sum_patents)
names(chem_pat_available_diff_graph)[names(chem_pat_available_diff_graph)=="FALSE"] <- "patents_untreated"
names(chem_pat_available_diff_graph)[names(chem_pat_available_diff_graph)=="TRUE"] <- "patents_treated"


# histogram for the share of domestic inventors
sh <- chem_pat %>% 
  filter(grntyr >= 1900 & grntyr <= 1918 ) %>%
  group_by(uspto_class,licensed_class)%>%
  summarise(sh = sum(count_usa)/sum(count))
sh[,'sh']=round(sh[,'sh'],1)
sh_1 <- sh %>% filter(licensed_class == 0)
sh_1 <- table(sh_1$sh)
sh_2 <- sh %>% filter(licensed_class == 1)
sh_2 <- table(sh_2$sh)
barplot(sh_1, main= "Untreated subclasses",
        xlab="share of U.S. inventors 1900-1918 ", col="orange")

barplot(sh_2, main= "Treated subclasses",
        xlab="share of U.S. inventors 1900-1918 ", col="orange")

# make a line graph to show the evolution of number of patents & remaining time in subclasses with 0, 1, and 2 or more licenses.
groupby_count_licenses <- chem_pat %>%
  group_by(count_licenses) %>%
  summarise(patent_us = mean(count_usa),
            remain_time = mean(years_remaining))
groupby_count_licenses <- groupby_count_licenses[2:11,]
ggplot(groupby_count_licenses) + 
  geom_line(aes(x=count_licenses, y=patent_us))+
  ylab("Average number of patents by U.S. inventors")+
  xlab("Number of licenses in the subclass from 1875 to 1939") +
  theme_economist()

ggplot(groupby_count_licenses) + 
  geom_line(aes(x=count_licenses, y=remain_time)) +
  ylab("Remaining lifetime of licensed patents 
       at the time of licensing")+
  xlab("Number of licenses in the subclass from 1875 to 1939") +
  theme_economist()

