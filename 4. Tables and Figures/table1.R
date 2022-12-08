# Full summary stats:

#install.packages("fst")
require(fst)
require(dplyr)
require(tidyr)
require(gtsummary)
require(usethis)
require(survival)
require(splines)
require(ggplot2)
#install.packages("pspline")
require(pspline)
require(gt)
#set working directory
setwd("~/Marianthis_Lab/Data_2")


df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")


df_3 <- rbind(df_1, df_2)

hist(df_3$MeanT_6hr)

df_3 <- df_3 %>% rename( "Insurance Status" = AnyIns,
                         Sex = sex,
                         Age = ageGroup)
df_3$Age <- ifelse(df_3$Age == "AgeGTE65", ">= 65", "< 65")

names(df_3)

uninsured <- df_3 %>% filter(`Insurance Status` == "No Insurance (self-pay or work comp)")

insured <-  df_3 %>% filter(`Insurance Status` == "Any Insurance")

#summarize based on cases and insurance status
stats_dta <- df_3 %>% filter(Case ==1) %>% select(MeanT_6hr, 
                                                   MeanR_6hr, 
                                                   MeanT_12hr, 
                                                   MeanR_12hr,
                                                   MeanT_24hr, 
                                                   MeanR_24hr,
                                                   MeanT_48hr, 
                                                   MeanR_48hr,
                                                   `Insurance Status`, 
                                                   Sex, 
                                                   Age)


#ftest_common_variance <- function(data, variable, by, conf.level, ...) {
 # data <- data[c(variable, by)] %>% dplyr::filter(complete.cases(.))
  #fisher.test(data[[variable]] ~ factor(data[[by]]), conf.level = conf.level, workspace=2e7) %>%
   # broom::tidy()
#}





stats_dta %>% tbl_summary(by = `Insurance Status`, 
                        #  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                          label= list(MeanT_6hr ~ "Ambient Temperature",
                                      MeanR_6hr ~ "Relative Humidity",
                                      MeanT_12hr ~ "Ambient Temperature",
                                      MeanR_12hr ~ "Relative Humidity",
                                      MeanT_24hr ~ "Ambient Temperature",
                                      MeanR_24hr ~ "Relative Humidity",
                                      MeanT_48hr ~ "Ambient Temperature",
                                      MeanR_48hr ~ "Relative Humidity"),
                           digits = list(all_continuous() ~ c(2, 2)))%>%
  modify_header(label ~ "**Variable**") %>%
  add_p(test = Sex~ "chisq.test") %>% 
  add_overall()%>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Insurance Status**")%>% 
  bold_labels() %>% 
  bold_p(t = .05, )%>%
  as_gt() %>% 
  gt::tab_header(title =  md("**Table 1.** Characteristics of NYS MI Hospitalizations According to Insurance Status (1995-2015)"))%>%
  tab_row_group(group = "6 Hour Pre-Event Average", rows = 1:2)%>%
  tab_row_group(group = "12 Hour Pre-Event Average", rows = 3:4)%>%
  tab_row_group(group = "24 Hour Pre-Event Average", rows = 5:6)%>%
  tab_row_group(group = "48 Hour Pre-Event Average", rows = 7:8)%>%
  tab_row_group(group = "Patient-Level Characteristics", rows = 9:15)%>% 
row_group_order(
  groups = c("6 Hour Pre-Event Average", 
             "12 Hour Pre-Event Average",
             "24 Hour Pre-Event Average",
             "48 Hour Pre-Event Average",
             "Patient-Level Characteristics")
)
  

