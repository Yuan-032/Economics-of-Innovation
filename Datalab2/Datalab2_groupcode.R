library(tidyverse) 
library(huxtable) 
library(dplyr)
rm(list= ls())	

getwd()
setwd("/Users/yuanhuang/Desktop/Datalab2") 
getwd()

#sometimes the ï..regid changes to a ?..regid when we save our code, 
#so when we try to run the line that's supposed to rename the ï..regid the code 
#sometimes breaks because it changed it to ?..regid after that last time it got saved
df_roses <- read.csv("rose_data.csv", 
                     stringsAsFactors = FALSE)
df_roses <- df_roses %>% rename(regid = ?..regid)
df_roses_2000 <- df_roses%>%filter(regyear == 2000)


#Question 1.a 

# Drop duplicates by regid and name
df_roses_unique <- df_roses[!duplicated(df_roses$regid),]
df_roses_unique <- df_roses_unique[!duplicated(df_roses$name), ]


#for the duplicate of patentnumber, we keep the first row from the duplicate

df_roses_unique_withpatents <- df_roses_unique %>%
  filter(!is.na(patentnumber))

# df_roses_unique_withpatents_duplicates <- df_roses_unique_withpatents %>% 
#   group_by(patentnumber) %>% 
#   filter(n()>1) %>%
#   ungroup %>%
#   arrange(patentnumber)
  
df_roses_unique_withpatents_nonduplicates <- df_roses_unique_withpatents %>%
  distinct(patentnumber, .keep_all = T)

df_roses_unique_withoutpatents <- df_roses_unique %>%
  filter(is.na(patentnumber))

df_roses_unique <- bind_rows(df_roses_unique_withpatents_nonduplicates,df_roses_unique_withoutpatents)
  
# add reg_count that counts the number of roses regsistered in a given year
# also add patent_count that excludes patents that were filed before the roses were officially created
# and exclude differences between patentyear and regyear larger than 10 years to avoid the patents that happened too late 
df_annual_excl_late <- df_roses_unique %>% group_by(regyear) %>%
  mutate(roses_regist = n()) %>%
  ungroup %>%
  group_by(patentyear) %>%
  mutate(roses_patent = sum(!is.na(patentnumber) & (regyear <= patentyear) & (patentyear-regyear<10)) ) %>%
  ungroup

df_annual_excl_late_collapsed_reg <- df_annual_excl_late %>%
  group_by(regyear) %>%
  select(regyear, roses_regist) %>%
  rename(year=regyear) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  distinct(year, .keep_all = TRUE)


df_annual_excl_late_collapsed_patent <- df_annual_excl_late %>%
  group_by(patentyear) %>%
  select(patentyear, roses_patent) %>%
  rename(year = patentyear) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  distinct(year, .keep_all = TRUE)


#Question 1.b

df_annual_excl_late_1900_2000 <- merge(df_annual_excl_late_collapsed_reg, df_annual_excl_late_collapsed_patent, all = TRUE) %>% 
  replace(is.na(.), 0) %>%
  filter((year >= 1900) & (year <= 2000))
avg_roses_registered_pre <- sum(df_annual_excl_late_1900_2000$roses_regist[20:30]) / 10
avg_roses_registered_post <- sum(df_annual_excl_late_1900_2000$roses_regist[31:41]) / 10


#Question 1.c

colors <- c("Registered Roses" = 'red',"Patented Roses"='blue')
ggplot(df_annual_excl_late_1900_2000) +
  geom_line(aes(x=year, y=roses_regist, color = "Registered Roses")) +
  geom_line(aes(x=year, y=roses_patent, color = "Patented Roses"))+
  geom_vline(xintercept=1930)+
  labs(x="Years",y = "Number of roses registered/patented",color='Rose Type', title = "Number of registered/patented roses over time")+
  scale_color_manual(values = colors)
  

plot(df_annual_excl_late_1900_2000$year,df_annual_excl_late_1900_2000$roses_regist,xlab ="Year",ylab = "Number of roses registered")
lines(lowess(df_annual_excl_late_1900_2000$roses_regist ~ df_annual_excl_late_1900_2000$year,f=0.75),col="red")
lines(lowess(df_annual_excl_late_1900_2000$roses_regist ~ df_annual_excl_late_1900_2000$year,f=0.05),col="blue")
legend("topleft",c("span = 0.75","span = 0.05"),fill=c("red","blue"))  

df_annual_excl_late_1900_2000 <- df_annual_excl_late_1900_2000 %>% mutate(patentshare = roses_patent / roses_regist) %>% replace(is.na(.), 0)
ggplot(df_annual_excl_late_1900_2000, aes(x=year, y=patentshare)) +
  geom_line()+
  geom_vline(xintercept=1930)+
  labs(x="Year",y = "Share of registered roses that were patented", title = "Share of registered roses patented over time")


df_annual_excl_late_1900_2000<-df_annual_excl_late_1900_2000 %>% arrange(year) %>% mutate(difference = roses_regist-lag(roses_regist))%>% mutate(difference2 = roses_patent-lag(roses_patent))
colors1 <- c("Difference_of_registered_roses" = 'orange',"Difference_of_patented_roses"='blue')
ggplot(df_annual_excl_late_1900_2000) +
  geom_line(aes(x=year, y=difference, 
                color = "Difference_of_registered_roses")) +
  geom_line(aes(x=year, y=difference2, 
                color = "Difference_of_patented_roses")) +
  geom_vline(xintercept=1930)+
  labs(x="Year",y = "Difference of registered/patented roses 
       between current year and previous year ",color = 'Legend')+
  scale_color_manual(values = colors1)

#Question 2

df_roses_unique_sub1900_2000 <- subset(df_roses_unique,regyear>=1900&regyear<=2000)
df_annual_colors <- df_roses_unique_sub1900_2000 %>% group_by(regyear) %>%
  summarise(roses_regist = n(), 
            roses_white = sum(color == "w"),
            roses_yellow = sum(color %in% c("ly", "my", "dy", "yb")),
            roses_orange = sum(color %in% c('ab','ob','op','or')),
            roses_pink = sum(color %in% c('lp','mp','dp')),
            roses_red = sum(color %in% c('mr','dr','rb')),
            )
df_annual_colors<- df_annual_colors[1:63,]

#time series graph for color
ggplot(df_annual_colors) +
  geom_line(aes(x=regyear, y=roses_white), 
            color = "gray90") + 
  geom_line(aes(x=regyear, y=roses_yellow), 
            color = "yellow") + 
  geom_line(aes(x=regyear, y=roses_orange), 
            color = "orange") + 
  geom_line(aes(x=regyear, y=roses_pink), 
            color = "pink") + 
  geom_line(aes(x=regyear, y=roses_red), 
            color = "red") + 
  theme_dark() + 
  labs(title = "Amounts of Different Colored Roses Registered Over Time", x = "Years", y = "Roses Registered", color = "Color Type")

#area graph for color
df_roses_all_color <- df_roses_unique_sub1900_2000 %>%
  filter(color %in% c("ly", "my", "dy", "yb", 
                      "mr", "dr", "rb",
                      'w',
                      'ob','op','ab','or',
                      'lp','mp','dp')) 
df_roses_all_color <- df_roses_all_color %>% 
  mutate(color=recode(color, 
                      "ly"="yellow", #"ly" gets recoded into "yellow"
                      "my"="yellow", #"my" also becomes "yellow". 
                      "dy"="yellow",
                      "yb"="yellow",
                      "mr"="red",
                      "dr"="red",
                      "rb"="red",
                      "w"="white",
                      "ob"="orange",
                      "op"="orange",
                      'ab'='orange',
                      "or"="orange",
                      "lp"="pink",
                      "mp"="pink",
                      "dp"="pink"))
df_annual_by_color <- df_roses_all_color %>% 
  group_by(regyear, color) %>%
  summarise(roses_regist = n(),
            roses_patent = sum(!is.na(patentnumber)))
group.colors <- c(yellow = "khaki1", 
                  red = "tomato2",
                  white = "grey91",
                  orange = "tan1",
                  pink = "lightpink1")
ggplot(df_annual_by_color) + 
  geom_area(aes(x=regyear, y=roses_regist, fill=color)) + 
  scale_fill_manual(values=group.colors) + 
  labs(x="Years",y = "Number of registered roses for different colors",color='Legend')+
  theme_bw() 

#time series graph for class
df_annual_classes <- df_roses_unique_sub1900_2000 %>% 
  mutate(class=recode(class,"A"="Old Garden",
         "Ayr"="Old Garden",
         "B"="Old Garden",
         "Bslt"="Old Garden",
         "C"="Old Garden",
         "Ch"="Old Garden",
         "Cl T"="Old Garden",
         "D"="Old Garden",
         "E"="Old Garden",
         "G"="Old Garden",
         "HBc"="Old Garden",
         "HCh"="Old Garden",
         "HFt"="Old Garden",
         "HMult"="Old Garden",
         "HP"="Old Garden",
         "HSem"="Old Garden",
         "HSet"="Old Garden",
         "HSpn"="Old Garden",
         "M"="Old Garden",
         "Misc. OGR"="Old Garden",
         "N"="Old Garden",
         "P"="Old Garden",
         "Sp"="Old Garden",
         "T"="Old Garden",
         "Cl Min"="Miniature",
         "Min"="Miniature",
         "HMsk"="Shrub",
         "HRg"="Shrub",
         "K"="Shrub",
         "S"="Shrub",
         "Cl F"="Other",
         "Cl Gr"="Other",
         "Cl HT"="Other",
         "Cl Pol"="Other",
         "F"="Other",
         "Gr"="Other",
         "HT"="Other",
         "LCl"="Other",
         "Pol"="Other",
         "R"="Other",
         .default = NA_character_)
         ) %>%
  filter(!is.na(class)) %>%
  group_by(regyear,class) %>%
  count()

ggplot(df_annual_classes, aes(x=regyear, y=n, group=class, color=class)) +
  geom_line() +
  labs(title="Varieties of Roses Over Time", x="Year",y="Number of Rose Registrations", color = "Rose Class")


#Question 3 

df_gdp <- read.csv("USGDP_1790-2013.csv", 
                   skip = 1, 
                   stringsAsFactors = FALSE)  
colnames(df_gdp) <- c("year", "ngdp", "realgdp", 
                      "deflator", "pop")
df_gdp <- df_gdp %>% mutate(ngdp = as.numeric(gsub(",", "", ngdp)), 
                            realgdp = as.numeric(gsub(",", "", realgdp)),
                            deflator = as.numeric(gsub(",", "", deflator)),
                            pop = as.numeric(gsub(",", "", pop)))

df_annual_merge <- merge(df_roses_unique_sub1900_2000, df_gdp,
                         by.x = "regyear", by.y = "year", all.x = TRUE) 

df_annual_merge_group <- df_annual_merge %>% group_by(regyear) %>%
  summarise(roses_regist = n(), 
            roses_patent = sum(!is.na(patentnumber) & (regyear < patentyear) & (patentyear-regyear<10)),
            ngdppc = mean(ngdp)/mean(pop),
            realgdppc = mean(realgdp)/mean(pop))
ggplot(df_annual_merge_group) +
  geom_line(aes(x=regyear, y=roses_regist, color= "Roses Registered")) +
  geom_line(aes(x=regyear, y=ngdppc*10, color = "Nominal GDP Per Cap")) +
  scale_y_continuous(
    name = "Roses Registered",
    sec.axis = sec_axis(~./10, name="Nominal GDP Per Capita")
  ) + 
  theme(
    axis.title.y = element_text(color = "Blue", size=13),
    axis.title.y.right = element_text(color = "Red", size=13)
  ) +
  labs(x="Year", colour = "Data Type", title = "Rose Registrations vs. Nominal GDP over time")

ggplot(df_annual_merge_group) +
  geom_line(aes(x=regyear, y=roses_regist, color= "Roses Registered")) +
  geom_line(aes(x=regyear, y=realgdppc*10, color = "Real GDP Per Cap")) +
  scale_y_continuous(
    name = "Roses Registered",
    sec.axis = sec_axis(~./10, name="Real GDP Per Capita")
  ) + 
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +
  labs(x="Year", colour = "Data Type",  title = "Rose Registrations vs. Real GDP over time(base year=2012)")


#Question 4 Diversity of Hybridizer

df_annual_hybridizer <- df_roses_unique %>% group_by(regyear) %>%
  summarise(hybridizer =n_distinct(hybridizer))%>%
  filter((regyear>=1900)&(regyear<=2000))
view(df_annual_hybridizer)

ggplot(df_annual_hybridizer) +
  geom_line(aes(x=regyear, y=hybridizer)) +
  geom_vline(xintercept=1930)+
  labs(x="Years",y = "Number of hybridizer of registered roses", title = "Number of Rose Hybridizers over time")

avg_hybridizers_pre <- sum(df_annual_hybridizer$hybridizer[1:30]) / 30
avg_hybridizers_post <- sum(df_annual_hybridizer$hybridizer[31:101]) / 70
avg_hybridizers_pre_10 <- sum(df_annual_hybridizer$hybridizer[20:30]) / 10
avg_hybridizers_post_10 <- sum(df_annual_hybridizer$hybridizer[31:41]) / 10





