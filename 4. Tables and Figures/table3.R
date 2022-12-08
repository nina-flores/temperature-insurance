install.packages("dplyr")
install.packages("tidyr")
install.packages("gtsummary")
install.packages("gt")
install.packages("flextable")
require(dplyr)
require(tidyr)
require(gtsummary)
require(usethis)
require(survival)
require(splines)
require(ggplot2)
require(gt)
require(flextable)



#results table

setwd("~/Marianthis_Lab/Data_2/calculating_OR")


###########################
#6 hr

results6 <- read.csv("./results.6h.full.csv")


results6$ROR <- round(results6$ROR,2)
results6$ROR.lower <- round(results6$ROR.lower,2)
results6$ROR.upper <- round(results6$ROR.upper,2)

results6$pvalue2sided6 <- round(results6$pvalue2sided,2)
results6$Temperature6 <- round(results6$Temperature,2)

results6$Percentile6 <- recode(results6$Percentile, "0"= "0th", 
                               ".05"= "5th",
                               ".10" = "10th",
                               ".15"= "15th",
                               ".20"= "20th",
                               ".25"= "25th",
                               ".30"= "30th",
                               ".35"= "35th",
                               ".40"= "40th",
                               ".45"= "45th",
                               ".50"= "50th",
                               ".55"= "55th",
                               ".60"= "60th",
                               ".65"="65th",
                               ".70"="70th",
                               ".75" = "75th",
                               ".80"= "80th",
                               ".85" = "85th",
                               ".90" = "90th",
                               ".95"= "95th",
                               "1.00" = "100th")


#results6 <- results6 %>% select(ROR, ROR.lower, ROR.upper)
results6$RORCI6 <- paste(results6$ROR, " (", results6$ROR.lower, ", ", results6$ROR.upper, ")", sep= "")
results6$pertemp6 <- paste(results6$Temperature6, " (", results6$Percentile6, ")", sep= "")



results6<- results6 %>% select(Percentile6, RORCI6, pvalue2sided6)

#12 hr
results12 <- read.csv("./results.12h.full.csv")


results12$ROR <- round(results12$ROR,2)
results12$ROR.lower <- round(results12$ROR.lower,2)
results12$ROR.upper <- round(results12$ROR.upper,2)

results12$pvalue2sided12 <- round(results12$pvalue2sided,2)
results12$Temperature12 <- round(results12$Temperature,2)

results12$Percentile12 <- recode(results12$Percentile, "0"= "0th", 
                                 ".05"= "5th",
                                 ".10" = "10th",
                                 ".15"= "15th",
                                 ".20"= "20th",
                                 ".25"= "25th",
                                 ".30"= "30th",
                                 ".35"= "35th",
                                 ".40"= "40th",
                                 ".45"= "45th",
                                 ".50"= "50th",
                                 ".55"= "55th",
                                 ".60"= "60th",
                                 ".65"="65th",
                                 ".70"="70th",
                                 ".75" = "75th",
                                 ".80"= "80th",
                                 ".85" = "85th",
                                 ".90" = "90th",
                                 ".95"= "95th",
                                 "1.00" = "100th")


#results6 <- results6 %>% select(ROR, ROR.lower, ROR.upper)
results12$RORCI12 <- paste(results12$ROR, " (", results12$ROR.lower, ", ", results12$ROR.upper, ")", sep= "")
results12$pertemp12 <- paste(results12$Temperature12, " (", results12$Percentile12, ")", sep= "")



results12<- results12 %>% select(RORCI12, pvalue2sided12)


c <-cbind(results6, results12)


#24


results24 <- read.csv("./results.24h.full.csv")


results24$ROR <- round(results24$ROR,2)
results24$ROR.lower <- round(results24$ROR.lower,2)
results24$ROR.upper <- round(results24$ROR.upper,2)

results24$pvalue2sided24 <- round(results24$pvalue2sided,2)
results24$Temperature24 <- round(results24$Temperature,2)

results24$Percentile24 <- recode(results24$Percentile, "0"= "0th", 
                                 ".05"= "5th",
                                 ".10" = "10th",
                                 ".15"= "15th",
                                 ".20"= "20th",
                                 ".25"= "25th",
                                 ".30"= "30th",
                                 ".35"= "35th",
                                 ".40"= "40th",
                                 ".45"= "45th",
                                 ".50"= "50th",
                                 ".55"= "55th",
                                 ".60"= "60th",
                                 ".65"="65th",
                                 ".70"="70th",
                                 ".75" = "75th",
                                 ".80"= "80th",
                                 ".85" = "85th",
                                 ".90" = "90th",
                                 ".95"= "95th",
                                 "1.00" = "100th")


#results6 <- results6 %>% select(ROR, ROR.lower, ROR.upper)
results24$RORCI24 <- paste(results24$ROR, " (", results24$ROR.lower, ", ", results24$ROR.upper, ")", sep= "")
results24$pertemp24 <- paste(results24$Temperature24, " (", results24$Percentile24, ")", sep= "")



results24<- results24 %>% select(RORCI24, pvalue2sided24)


d <-cbind(c, results24)


#24


results48 <- read.csv("./results.48h.full.csv")


results48$ROR <- round(results48$ROR,2)
results48$ROR.lower <- round(results48$ROR.lower,2)
results48$ROR.upper <- round(results48$ROR.upper,2)

results48$pvalue2sided48 <- round(results48$pvalue2sided,2)
results48$Temperature48 <- round(results48$Temperature,2)

results48$Percentile48 <- recode(results48$Percentile, "0"= "0th", 
                                 ".05"= "5th",
                                 ".10" = "10th",
                                 ".15"= "15th",
                                 ".20"= "20th",
                                 ".25"= "25th",
                                 ".30"= "30th",
                                 ".35"= "35th",
                                 ".40"= "40th",
                                 ".45"= "45th",
                                 ".50"= "50th",
                                 ".55"= "55th",
                                 ".60"= "60th",
                                 ".65"="65th",
                                 ".70"="70th",
                                 ".75" = "75th",
                                 ".80"= "80th",
                                 ".85" = "85th",
                                 ".90" = "90th",
                                 ".95"= "95th",
                                 "1.00" = "100th")



#results6 <- results6 %>% select(ROR, ROR.lower, ROR.upper)
results48$RORCI48 <- paste(results48$ROR, " (", results48$ROR.lower, ", ", results48$ROR.upper, ")", sep= "")
results48$pertemp48 <- paste(results48$Temperature48, " (", results48$Percentile48, ")", sep= "")



results48<- results48 %>% select(RORCI48, pvalue2sided48)

class(e$pertemp6)
e <-cbind(d, results48)

e[is.na(e)] <- "-"


e %>% gt(rowname_col = "Percentile6")%>%
  tab_stubhead(label = "Percentile of Temperature")%>%
  tab_spanner(
    label = "6-Hour Window",
    columns = c("RORCI6", "pvalue2sided6")
  )%>%
  tab_spanner(
    label = "12-Hour Window",
    columns = c("RORCI12", "pvalue2sided12")
  )%>%
  tab_spanner(
    label = "24-Hour Window",
    columns = c("RORCI24", "pvalue2sided24")
  )%>%
  tab_spanner(
    label = "48-Hour Window",
    columns = c("RORCI48", "pvalue2sided48")
  )%>%
  cols_label(
    "RORCI6" = html("Odds Ratio (95% CI)"),
    "pvalue2sided6" = html("p-value"),
    "RORCI12" = html("Odds Ratio (95% CI)"),
    "pvalue2sided12" = html("p-value"),
    "RORCI24" = html("Odds Ratio (95% CI)"),
    "pvalue2sided24" = html("p-value"),
    "RORCI48" = html("Odds Ratio (95% CI)"),
    "pvalue2sided48" = html("p-value"),
  )%>%
  tab_header(
    title = "Table 3. Odds Ratios between Insurance Status and MI",
    subtitle = "Comparing Uninsured to Insured"
  ) %>% 
  tab_footnote("Reported odds ratios compare associations for an increase in temperature from the median
               to the specified percentile of temperature between uninsured and insured",locations = cells_column_labels(columns = c("RORCI6",'RORCI12', 'RORCI24', "RORCI48")))




