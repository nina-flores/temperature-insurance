install.packages("dplyr")
install.packages("tidyr")
install.packages("gtsummary")
install.packages("gt")
install.packages("flextable")
install.packages("fst")
require(dplyr)
require(tidyr)
require(gtsummary)
require(usethis)
require(survival)
require(splines)
require(ggplot2)
require(gt)
require(flextable)
require(fst)



#set working directory
setwd("~/Marianthis_Lab/Data_2")


df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")
df_3 <- rbind(df_1, df_2)
df_3 <- df_3 %>% rename( "Insurance Status" = AnyIns,
                         Sex = sex,
                         Age = ageGroup)
df_3$Age <- ifelse(df_3$Age == "AgeGTE65", ">= 65", "< 65")


df<- df_3 %>% select(MeanT_6hr, 
                       MeanT_12hr, 
                       MeanT_24hr, 
                       MeanT_48hr)


#df %>% tbl_summary(type = all_continuous() ~ "continuous2",statistic = all_continuous() ~ c("{mean} ({sd})",
 #                                                   "{median} ({p5}, {p95})", 
  #                                                  "{min}, {max}"),missing = "no")



df_6 <- data.frame()
meantemp <- round(mean(df$MeanT_6hr), digits = 3)
sdtemp <- round(sd(df$MeanT_6hr), digits = 3)
percentiletemp5 <- round(quantile(df$MeanT_6hr,.05), digits = 3)
percentiletemp95 <- round(quantile(df$MeanT_6hr,.95), digits = 3)
min_temp <- round(min(df$MeanT_6hr), digits = 3)
max_temp <- round(max(df$MeanT_6hr), digits = 3)
median_temp <- round(median(df$MeanT_6hr), digits = 3)
IQR_temp <- round(IQR(df$MeanT_6hr), digits = 3)

window <- "6 hour pre-event average"

`Mean (SD)` <- paste(meantemp, " (" , sdtemp, ")", sep = "")
`Median (IQR)` <- paste(median_temp, " (" , IQR_temp, ")", sep = "")
`5th and 95th Percentile` <- paste(percentiletemp5,", ",percentiletemp95, sep = "")
`Range` <- paste(min_temp, ", " , max_temp, sep = "")


df_6<- as.data.frame(cbind(`Mean (SD)`, `Median (IQR)`, `5th and 95th Percentile`,`Range`, window))


df_12 <- data.frame()
meantemp <- round(mean(df$MeanT_12hr), digits = 3)
sdtemp <- round(sd(df$MeanT_12hr), digits = 3)
percentiletemp5 <- round(quantile(df$MeanT_12hr,.05), digits = 3)
percentiletemp95 <- round(quantile(df$MeanT_12hr,.95), digits = 3)
min_temp <- round(min(df$MeanT_12hr), digits = 3)
max_temp <- round(max(df$MeanT_12hr), digits = 3)
median_temp <- round(median(df$MeanT_12hr), digits = 3)
IQR_temp <- round(IQR(df$MeanT_12hr), digits = 3)

window <- "12 hour pre-event average"

`Mean (SD)` <- paste(meantemp, " (" , sdtemp, ")", sep = "")
`Median (IQR)` <- paste(median_temp, " (" , IQR_temp, ")", sep = "")
`5th and 95th Percentile` <- paste(percentiletemp5,", ",percentiletemp95, sep = "")
`Range` <- paste(min_temp, ", " , max_temp, sep = "")

df_12<- as.data.frame(cbind(`Mean (SD)`, `Median (IQR)`, `5th and 95th Percentile`,`Range`, window))


df_24 <- data.frame()
meantemp <- round(mean(df$MeanT_24hr), digits = 3)
sdtemp <- round(sd(df$MeanT_24hr), digits = 3)
percentiletemp5 <- round(quantile(df$MeanT_24hr,.05), digits = 3)
percentiletemp95 <- round(quantile(df$MeanT_24hr,.95), digits = 3)
min_temp <- round(min(df$MeanT_24hr), digits = 3)
max_temp <- round(max(df$MeanT_24hr), digits = 3)
median_temp <- round(median(df$MeanT_24hr), digits = 3)
IQR_temp <- round(IQR(df$MeanT_24hr), digits = 3)

window <- "24 hour pre-event average"


`Mean (SD)` <- paste(meantemp, " (" , sdtemp, ")", sep = "")
`Median (IQR)` <- paste(median_temp, " (" , IQR_temp, ")", sep = "")
`5th and 95th Percentile` <- paste(percentiletemp5,", ",percentiletemp95, sep = "")
`Range` <- paste(min_temp, ", " , max_temp, sep = "")
df_24<- as.data.frame(cbind(`Mean (SD)`, `Median (IQR)`, `5th and 95th Percentile`,`Range`, window))

df_48 <- data.frame()
meantemp <- round(mean(df$MeanT_48hr), digits = 3)
sdtemp <- round(sd(df$MeanT_48hr), digits = 3)
percentiletemp5 <- round(quantile(df$MeanT_48hr,.05), digits = 3)
percentiletemp95 <- round(quantile(df$MeanT_48hr,.95), digits = 3)
min_temp <- round(min(df$MeanT_48hr), digits = 3)
max_temp <- round(max(df$MeanT_48hr), digits = 3)
median_temp <- round(median(df$MeanT_48hr), digits = 3)
IQR_temp <- round(IQR(df$MeanT_48hr), digits = 3)

window <- "48 hour pre-event average"


`Mean (SD)` <- paste(meantemp, " (" , sdtemp, ")", sep = "")
`Median (IQR)` <- paste(median_temp, " (" , IQR_temp, ")", sep = "")
`5th and 95th Percentile` <- paste(percentiletemp5,", ",percentiletemp95, sep = "")
`Range` <- paste(min_temp, ", " , max_temp, sep = "")
df_48<- as.data.frame(cbind(`Mean (SD)`, `Median (IQR)`, `5th and 95th Percentile`,`Range`, window))


summary_stats <- as.data.frame(rbind(df_6, df_12, df_24, df_48))


summary_stats %>% gt(rowname_col = "window")%>% 
  tab_stubhead(label = "Time Window")%>%
  tab_header(
    title = "Table 2. Descriptive Statistics of Ambient Temperatures during Case and Control Periods"
  ) 





