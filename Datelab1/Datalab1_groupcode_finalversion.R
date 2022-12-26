###### Data lab #1 ######
# Chris Ren, Ziyan Zeng, Yuan Huang


### Initial library imports and imports of data ###

#load libraries and clear workspace
library(tidyverse) 
library(huxtable)
library(ggrepel)
library(plotly)
rm(list= ls())	

#set working directory for project
setwd("/Users/yuanhuang/Desktop/Datalab1") 

#load and view data
df_fairs <- read.csv("world_fairs_data.csv") 
View(df_fairs) 

#disable scientific notation
options(scipen=999)


### Replications of Table 3 ###

#creation of variables for having patents (has_patents, dummy variable) and for the number of total exhibits (tot_exh, numerical var)
df_fairs <- df_fairs %>% mutate(has_patents = (pleng>0)) 
df_fairs <- df_fairs %>% 
  mutate(tot_exh = mining+chemicals+food+mach+scinst+textiles+manu)

#Table 3 recreation based on patent lengths in 1851 and 1876
df_fairs_sub <- df_fairs[ , c("country", "year", "pleng", "pop", "gdpmad" , "prim514" )]
df_fairs_sub_namechange <- df_fairs_sub %>% rename("Patent_Length" = pleng, "Population" = pop, "GDP" = gdpmad, "Primary_Education" = prim514)
df_fairs_wide <- reshape(df_fairs_sub_namechange,
                         idvar = "country", # which var is id
                         direction = "wide", # how you want things reshaped
                         timevar = "year",  # which var is repeating
                         sep = "_in_")
df_fairs_table3_1851 <- df_fairs_wide %>% arrange(desc(Patent_Length_in_1851))
df_fairs_table3_1851 <- df_fairs_table3_1851[,c(1,2,6,3,7,4,8,5,9)]
df_fairs_table3_1876 <- df_fairs_wide %>% arrange(desc(Patent_Length_in_1876))
df_fairs_table3_1876 <- df_fairs_table3_1876[,c(1,2,6,3,7,4,8,5,9)]


df_fairs_1 <- df_fairs%>% 
  mutate(share_of_mining = mining/tot_exh,share_of_chemicals = chemicals/tot_exh,share_of_food_processing = food/tot_exh,share_of_machinery = mach/tot_exh,share_of_scientific_instruments = scinst/tot_exh,share_of_textiles = textiles/tot_exh,share_of_manufacturing = manu/tot_exh) 
### (Q1) Demonstration that distribution of exhibits in different industries statistically significantly varies between countries ###

#Chi-squared test for 1851 testing if distributions between countries are homogenous within the bounds of what we would expect with some random variation
chi2_df_1851 <- df_fairs %>% 
  filter(year == 1851) %>%
  select(mining, chemicals, food, mach, scinst, textiles, manu)
chi2_test_1851 <- chisq.test(chi2_df_1851)
chi2_test_1851
chi2_test_1851$expected

#Chi-squared test for 1876 testing if distributions between countries are homogenous within the bounds of what we would expect with some random variation
chi2_df_1876 <- df_fairs %>% 
  filter(year == 1876) %>%
  select(mining, chemicals, food, mach, scinst, textiles, manu)
chi2_test_1876 <- chisq.test(chi2_df_1876)
chi2_test_1876
chi2_test_1876$expected


### (Q1) Creation of Scatter Plots of Patent-Reliant and Non-Patent-Reliant Exhibition Categories in 1851 and 1876 ###

#scatter plots in 1851 for manufacturing exhibitions (patent-reliant) and scientific instruments exhibitions (non-patent-reliant)
df_fairs_1851_manu_sci <- df_fairs %>%
  select(country, year, pleng, manu, scinst, tot_exh) %>%
  mutate(manu_share = manu / tot_exh, scinst_share = scinst / tot_exh) %>% 
  filter(year == 1851)

ggplot(df_fairs_1851_manu_sci, aes(x=pleng,y=manu_share)) +
  geom_point() +
  geom_label_repel(
    data = df_fairs_1851_manu_sci,
    size = 4, box.padding = 0.5,
    mapping=aes(x=pleng,y=manu_share,label=country)
  ) +
  labs(title = "Manufacturing Exhibitions for Countries of\nDifferent Patent Lengths (1851)", x = "Patent Length (Years)", y = "Share of Manufacturing Exhibitions")
  

ggplot(df_fairs_1851_manu_sci, aes(x=pleng,y=scinst_share)) +
  geom_point() +
  geom_label_repel(
    data = df_fairs_1851_manu_sci,
    size = 4, box.padding = 0.5,
    mapping=aes(x=pleng,y=scinst_share,label=country)
  ) +
  labs(title = "Scientific Instrument Exhibitions for Countries of\nDifferent Patent Lengths (1851)", x = "Patent Length (Years)", y = "Share of Scientific Instrument Exhibitions")

#scatter plots in 1876 for manufacturing exhibitions (patent-reliant) and scientific instruments exhibitions (non-patent-reliant)
df_fairs_1876_manu_sci <- df_fairs %>%
  select(country, year, pleng, manu, scinst, tot_exh) %>%
  mutate(manu_share = manu / tot_exh, scinst_share = scinst / tot_exh) %>% 
  filter(year == 1876)

ggplot(df_fairs_1876_manu_sci, aes(x=pleng,y=manu_share)) +
  geom_point() +
  geom_label_repel(
    data = df_fairs_1876_manu_sci,
    size = 4, box.padding = 0.5,
    mapping=aes(x=pleng,y=manu_share,label=country)
  ) +
  labs(title = "Manufacturing Exhibitions for Countries of Different Patent Lengths (1876)", x = "Patent Length (Years)", y = "Share of Manufacturing Exhibitions")

ggplot(df_fairs_1876_manu_sci, aes(x=pleng,y=scinst_share)) +
  geom_point() +
  geom_label_repel(
    data = df_fairs_1876_manu_sci,
    size = 4, box.padding = 0.5,
    mapping=aes(x=pleng,y=scinst_share,label=country)
  ) +
  labs(title = "Scientific Instrument Exhibitions for Countries of Different Patent Lengths (1876)", x = "Patent Length (Years)", y = "Share of Scientific Instrument Exhibitions")

# the histogram for share of exhibits from all field in 1851
df_fairs_sub2 <- df_fairs_1[ , c("country", "year", "pleng","share_of_mining","share_of_chemicals","share_of_food_processing","share_of_machinery","share_of_scientific_instruments","share_of_textiles","share_of_manufacturing","tot_exh")]
ds_fig_1851 <- df_fairs_sub2 %>% filter(year==1851)
fig <- plot_ly(ds_fig_1851, x = ~ds_fig_1851$country, y = ~ds_fig_1851$share_of_mining, type = 'bar', name = 'Share of mining')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_chemicals, name = 'Share of chemicals')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_food_processing, name = 'Share of food processing')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_machinery, name = 'Share of machinery')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_scientific_instruments, name = 'Share of scientific instruments')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_textiles, name = 'Share of textiles')
fig <- fig %>% add_trace(y = ~ds_fig_1851$share_of_manufacturing, name = 'Share of manufacturing')


fig <- fig %>% layout(title = "Share of exhibits among seven industries in 1851",
                      xaxis = list(title = 'Country'),
                      yaxis = list(title = 'Share of exhibits'),
                      barmode = 'stack')
# the histogram for share of exhibits from all field in 1876
ds_fig_1876 <- df_fairs_sub2 %>% filter(year==1876)
fig <- plot_ly(ds_fig_1876, x = ~ds_fig_1876$country, y = ~ds_fig_1876$share_of_mining, type = 'bar', name = 'Share of mining')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_chemicals, name = 'Share of chemicals')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_food_processing, name = 'Share of food processing')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_machinery, name = 'Share of machinery')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_scientific_instruments, name = 'Share of scientific instruments')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_textiles, name = 'Share of textiles')
fig <- fig %>% add_trace(y = ~ds_fig_1876$share_of_manufacturing, name = 'Share of manufacturing')


fig <- fig %>% layout(title = "Share of exhibits among seven industries in 1876",
                      xaxis = list(title = 'Country'),
                      yaxis = list(title = 'Share of exhibits'),
                      barmode = 'stack')


#Denmark and Netherlands only
De <- read.csv("dema.csv") 
De <- De[c(1:7),]
fig <- plot_ly(De, x = ~De$Industry, y = ~De$X1851, type = 'bar', name = 'In 1851')
fig <- fig %>% add_trace(y = ~De$X1876, name = 'In 1876')
fig <- fig %>% layout(title = "Denmark :Share of exhibits among seven industries",
                      xaxis = list(title = 'Industry'),
                      yaxis = list(title = 'Share of exhibits'),
                      barmode = 'group')

fig <- plot_ly(De, x = ~De$Industry, y = ~De$X1851.1, type = 'bar', name = 'In 1851')
fig <- fig %>% add_trace(y = ~De$X1876.1, name = 'In 1876')
fig <- fig %>% layout(title = "Netherlands :Share of exhibits among seven industries",
                      xaxis = list(title = 'Industry'),
                      yaxis = list(title = 'Share of exhibits'),
                      barmode = 'group')

### (Q2) Creation of Histograms for ... ###

#histogram for 1851
df_fairs_51 <- df_fairs %>% filter(year == 1851)%>% mutate(innov_perc = tot_exh/pop)
df_fairs_51$country <- factor(df_fairs_51$country,levels = df_fairs_51$country[order(-df_fairs_51$innov_perc)])
ggplot(data=df_fairs_51, 
       aes(x=country, y=innov_perc, fill = has_patents)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Country", y = "Innovation per capita in 1851, all fields", title = "Innovation per capita by country, 1851") +
  scale_fill_discrete(name = "Patent Law", labels = c("No Patents", "Patents"))
  
#histogram for 1876
df_fairs_76 <- df_fairs %>% filter(year == 1876) %>% mutate(innov_perc = tot_exh/pop)
df_fairs_76$country <- factor(df_fairs_76$country,levels = df_fairs_76$country[order(-df_fairs_76$innov_perc)])
ggplot(data=df_fairs_76, 
       aes(x=country, y=innov_perc, fill = has_patents)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  labs(x = "Country", y = "Innovation per capita in 1876, all fields", title = "Innovation per capita by country, 1876") +
  scale_fill_discrete(name = "Patent Law", labels = c("No Patents", "Patents"))

#comparison of average innovations per capita of two groups
df_fairs_51_patents <- df_fairs_51 %>% filter(has_patents == T)
df_fairs_51_no_patents <- df_fairs_51 %>% filter(has_patents == F)
t.test(df_fairs_51_patents$innov_perc, df_fairs_51_no_patents$innov_perc)

df_fairs_76_patents <- df_fairs_76 %>% filter(has_patents == T)
df_fairs_76_no_patents <- df_fairs_76 %>% filter(has_patents == F)
t.test(df_fairs_76_patents$innov_perc, df_fairs_76_no_patents$innov_perc)


#netherlands (limited) counterexample
netherlands_1851_innov_perc <- df_fairs_51[7,"innov_perc"]
netherlands_1876_innov_perc <- df_fairs_76[7,"innov_perc"]
netherlands_delta_innov_perc <- netherlands_1876_innov_perc - netherlands_1851_innov_perc

