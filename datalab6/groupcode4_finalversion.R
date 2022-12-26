# Chris & Ginny & Yuan
# Data lab #6
# DL# 6: Biasi, Barbara, and Petra Moser “Effects of Copyright on Science. 
# Evidence from the WWII Book Replication Program” http://ssrn.com/abstract=2542879

library(tidyverse) 
library(huxtable) 
library(haven) 
library(gridExtra)
library(lfe) 
library(class)
library(mlr)
rm(list = ls())	

#setwd("/Users/Rusanov/Dropbox/NYU/teach innovation ug/data_labs/6_book_republication/code_data/") 
books <- read_stata("books_all.dta")



###Question 1###

## Question 1A ##
# summarizing the data
books_summary <- books %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(field, brp) %>%
  summarize(number_books = n()) %>%
  mutate(brp = recode(brp,  `0` = "non_BRP", `1` = "BRP"))
books_summary1 <- books_summary %>%
  group_by(brp)%>%
  summarise(total = sum(number_books))
# Answer: 291 different books were subjected to the BRP and the subject of compounds was most frequently licensed

## Question 1B ##
# filtering books underneath threshold of 200 citations per book of its lifetime
his <- books %>%
  filter(brp==1)%>%
  group_by(id)%>%
  summarise(total_cite = sum(cit_year)) %>%
  filter(total_cite < 200)
his1 <- books %>%
  filter(brp==1, year_c >1942)%>%
  group_by(id)%>%
  summarise(total_cite = sum(cit_year)) %>%
  filter(total_cite < 200)
# histogram of citation count of books across the entire time period
his %>%
  ggplot( aes(x=total_cite)) +
  geom_histogram( binwidth=2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Citation Amounts Among BRP Books", subtitle = "(Bin width of 2, outliers above 200 citations excluded)") +
  labs(x = "Citations", y = "Books with given number of citations") +
  theme(
    plot.title = element_text(size=15)
  )
ggsave(paste0("histogram.png"),
       width = 8, height = 5, units = "in")
# histogram of citation count of books beyond the BRP year of 1942
his1 %>%
  ggplot( aes(x=total_cite)) +
  geom_histogram( binwidth=2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Citation Among BRP Books After 1942", subtitle = "(Bin width of 2, outliers above 200 citations excluded)") +
  labs(x = "Citations", y = "Books with given number of citations") +
  theme(
    plot.title = element_text(size=15)
  )
ggsave(paste0("histogram_post1942.png"),
       width = 8, height = 5, units = "in")


###Question 2###

## Addressing Question 2's issue 1: comparing Citations to the same BRP Book by English-language and Other Authors
regr_table <- books %>%
  filter(brp == 1)%>%
  group_by(year_c,id) %>%
  summarise(Eng = sum(count_eng),
            Other = sum(count_noeng))

reg_table1<- regr_table %>%
  group_by(year_c) %>%
  summarise(Eng = sum(Eng)/n(),
            Other = sum(Other)/n()) %>%
  filter(year_c>=1930) 
# plotting diff-in-diff
ggplot(reg_table1) +
  geom_line(aes(x=year_c, y=Eng, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year_c, y=Eng, colour = "darkorange2"), size = 1.2) +
  geom_line(aes(x=year_c, y=Other, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year_c, y=Other, colour = "royalblue"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1936, label="Introduction of BRP", y=0.8), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("darkorange2", "royalblue"), 
                       labels = c("English Citations", 
                                  "Non-English Citations"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Citations per book") +
  ggtitle("Citations per BRP Book in English vs Non-English Languages") +
  theme(legend.position = "right", legend.text = element_text(size = 10))
ggsave(paste0("diff_in_diff_citations.png"),
       width = 8, height = 5, units = "in")

# quantifying diff-in-diff with regression
reg_ass1 <- regr_table %>%
  filter(year_c >=1930) %>%
  mutate(change = Eng - Other) 
reg_ass1$post <- ifelse(reg_ass1$year_c>=1942,1,0)
reg_ass1$English <- ifelse(reg_ass1$Eng>0,1,0)
reg_ass1$English_post <- ifelse(reg_ass1$English + reg_ass1$post>=2,1,0)
reg_assumption1 <- felm(change ~ English_post| 
                          id +year_c| 
                          0,
                        data=reg_ass1) 
summary(reg_assumption1)

# write table to document
huxreg(reg_assumption1,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       coefs = c("Treated X post-1942" = "English_post"), 
       statistics = c("N" = "nobs",
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("Year FE", "yes"), 
                 c("State FE", "yes")),
           copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>% quick_docx(file="report_reg1.docx")
# note: observations counts every entry, but really the N of this sample was 291 unique books

###Question 3 (Addressing Question 2's issue 2: Comparing BRP and Swiss books)###

## Question 3a
# filtering to define a comparable books subset
fields_nonBRP <- books_summary %>%
  spread(brp, number_books) %>%
  filter(non_BRP>=2) %>%
  mutate(indic_non_brp_field = 1) 
# we now have 27 fields that have at lest 2 non-BRP books in them. Merge  to the original dataset
books <- merge(books, fields_nonBRP,
               by = c("field"),
               all.x = TRUE)
books_comparable <- books %>% filter(indic_non_brp_field ==1) %>%
  filter(chemistry==1|mathematics==1) %>%
  filter(!diss)

# Answer: Chemistry and Math were subjects that were licensed heavily since Germany led those fields,
# many German authors published in the Swiss copyright system which would have exempted some of their
# work of comparably high quality from the BRP. Since chemistry and mathematics are well represented
# in both the BRP set and the Swiss set and we know that many German authors published under the Swiss
# copyright system, we focused on those two fields in particular. We also dropped dissertations since 
# their behavior was quite different from an average science book


## Question 3b
# reshaping for diff=in-diff
regr_table2 <- books_comparable %>%
  group_by(year_c,brp,id)%>%
  summarise(Eng = sum(count_eng))
regr_table2_1 <- regr_table2 %>%
  group_by(year_c,brp) %>%
  summarise(cit = sum(Eng)/n())
regr_table2_w <- reshape(as.data.frame(regr_table2_1),
                         idvar = c("year_c"), # which var is id
                         direction = "wide", # how you want things reshaped
                         timevar = "brp",  # which var is repeating
                         sep = "_") %>%
  filter(year_c>=1930)
# plotting diff-in-diff
ggplot(regr_table2_w) +
  geom_line(aes(x=year_c, y=cit_0, colour = "royalblue"), size = .8) +
  geom_point(aes(x=year_c, y=cit_0, colour = "royalblue"), size = 1.2) +
  geom_line(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = .8) +
  geom_point(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.75) +
  geom_text(aes(x=1936, label="Introduction of BRP", y=0.8), 
            colour="black", 
            angle=0, 
            text=element_text(size=10)) +
  scale_color_identity(name = "", 
                       breaks = c("darkorange2","royalblue"), 
                       labels = c("BRP Licensed Books",
                                  "Swiss Books"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Citations per book") +
  ggtitle("Citations per Book for BRP Licensed Books vs Swiss Book") +
  theme(legend.position = "right", legend.text = element_text(size = 10))
ggsave(paste0("diff_in_diff_swiss.png"),
       width = 8, height = 5, units = "in")

## Question 3c
# quantifying diff-in-diff with regression
reg_ass2 <- regr_table2
reg_ass2$post <- ifelse(reg_ass2$year_c>=1942,1,0)
reg_ass2$post_treated <- ifelse(reg_ass2$post + reg_ass2$brp>=2,1,0)

reg_assumption2_0 <- felm(Eng ~ brp + post + post_treated| 
                            0| 
                            0,
                          data=reg_ass2) 

reg_assumption2_1 <- felm(Eng ~ post_treated| 
                            id| 
                            0,
                          data=reg_ass2) 

reg_assumption2_2 <- felm(Eng ~ post_treated| 
                            year_c| 
                            0,
                          data=reg_ass2) 

reg_assumption2 <- felm(Eng ~ post_treated| 
                          id +year_c| 
                          0,
                        data=reg_ass2) 


huxreg(reg_assumption2_0,reg_assumption2_1,reg_assumption2_2,reg_assumption2,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       coefs = c("Treated X post-1942" = "post_treated",
                 "Post-1942" = 'post',
                 "Treated" = 'brp'),
       statistics = c("N" = "nobs",
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("Year FE", "no","no",'yes',"yes"), #note! you need more "yes" if you have >3 models
                 c("Book FE", "no","yes",'no',"yes")),
           copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>%  quick_docx(file = "report_regdatalab6.docx")




# ## Question 3d
# # filtering and reshapping for input into mahalanobis function
# knn <- books %>%
#   group_by(id,field,year_c) %>%
#   summarise(cite = sum(count_eng) + sum(count_noeng))
# knn1 <-knn  %>%
#   filter(year_c<1942) %>%
#   group_by(field,id) %>%
#   summarise(cite = sum(cite))
# knn1 <- knn1%>%
#   filter(field!='')
# knn1 <- knn1 %>% 
#   mutate(v = 1, fie = field) %>% 
#   spread(fie, v, fill = 0)
# # creation of normalize function
# normalize <- function(x){
#   return ((x-min(x)) / (max(x)-min(x)))
# }
# # subset data and applying normalize
# sub <- knn1[,3:36]
# subset <- as.data.frame(lapply(knn1[,3:36],normalize))
# set.seed(0)
# # input of mahalanobis function results and binding to original dataframe
# temp <- as.data.frame(sqrt(mahalanobis(sub, colMeans(sub), cov(sub),tol=1e-20)))
# final <- cbind(knn1,temp)
# names(final)[37] <- "distance"
# # filtering of distances under threshold of closeness
# set <- final %>%
#   select('id','distance') %>%
#   filter(distance <= 1)
# book_wanted <- set[2]
# 
# # reshaping for diff=in-diff
# regr_table3 <- books_comparable %>%
#   filter(books_comparable$id %in% book_wanted$id) %>%
#   group_by(year_c,brp,id)%>%
#   summarise(Eng = sum(count_eng))
# regr_table3_1 <- regr_table3 %>%
#   group_by(year_c,brp) %>%
#   summarise(cit = sum(Eng)/n())
# regr_table3_w <- reshape(as.data.frame(regr_table3_1),
#                          idvar = c("year_c"), # which var is id
#                          direction = "wide", # how you want things reshaped
#                          timevar = "brp",  # which var is repeating
#                          sep = "_") %>%
#   filter(year_c>=1930)
# # plotting diff-in-diff
# ggplot(regr_table3_w) +
#   geom_line(aes(x=year_c, y=cit_0, colour = "royalblue"), size = .8) +
#   geom_point(aes(x=year_c, y=cit_0, colour = "royalblue"), size = 1.2) +
#   geom_line(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = .8) +
#   geom_point(aes(x=year_c, y=cit_1, colour = "darkorange2"), size = 1.2) +
#   geom_vline(xintercept = 1942, linetype = "dashed", size = 0.75) +
#   geom_text(aes(x=1938, label="BRP", y=0.8), 
#             colour="black", 
#             angle=0, 
#             text=element_text(size=10)) +
#   scale_color_identity(name = "", 
#                        breaks = c("darkorange2","royalblue"), 
#                        labels = c("BRP",
#                                   "Swiss"), 
#                        guide = "legend") +
#   theme_bw() +
#   xlab("Year") +
#   ylab("Citations per book and year") +
#   ggtitle("Figure 2 - Citation BRP Books and Swiss Book") +
#   theme(legend.position = "right", legend.text = element_text(size = 10))
# 
# # quantifying diff-in-diff with regression
#  reg_ass3 <- regr_table3
# reg_ass3$post <- ifelse(reg_ass3$year_c>=1942,1,0)
# reg_ass3$post_treated <- ifelse(reg_ass3$post + reg_ass3$brp>=2,1,0)
# reg_assumption3 <- felm(Eng ~ post_treated| 
#                           id +year_c| # two FEs? use a + to add them
#                           0,
#                         data=reg_ass3) 
# summary(reg_assumption3)
