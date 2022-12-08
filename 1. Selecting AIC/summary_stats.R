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
stats_dta6 <- df_3 %>% filter(Case ==1) %>% select(MeanT_6hr, 
                                                  MeanR_6hr, 
                                                  `Insurance Status`, Sex, Age)
require(gt)

######################
#6 hr
######################
stats_dta6 %>% tbl_summary(by = `Insurance Status`, label= list(MeanT_6hr ~ "Temperature", MeanR_6hr ~ "Relative Humidity"),
  digits = list(all_continuous() ~ c(2, 2)))%>% modify_header(label ~ "**Variable**") %>%
  add_p() %>% add_overall()%>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Insurance Status**")%>% bold_labels() %>% bold_p(t = .05)%>%
  as_gt() %>% gt::tab_header(title =  md("**Table 1.** Descriptive Statistics of NYS MI Admissions (1995-2015)"))%>%
  tab_source_note("*6 hour average preceding event")

######################
#12 hr
######################
stats_dta12 <- df_3 %>% filter(Case ==1) %>% select(MeanT_12hr, 
                                                   MeanR_12hr, 
                                                   `Insurance Status`, Sex, Age)



stats_dta12 %>% tbl_summary(by = `Insurance Status`, label= list(MeanT_12hr ~ "Temperature", MeanR_12hr ~ "Relative Humidity"),
                          digits = list(all_continuous() ~ c(2, 2)))%>% modify_header(label ~ "**Variable**") %>%
  add_p() %>% add_overall()%>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Insurance Status**")%>% bold_labels() %>% bold_p(t = .05)%>%
  as_gt() %>% gt::tab_header(title =  md("**Table 1.** Descriptive Statistics of NYS MI Admissions (1995-2015)"))%>%
  tab_source_note("*12 hour average preceding event")

######################
#24 hr
######################
stats_dta24 <- df_3 %>% filter(Case ==1) %>% select(MeanT_24hr, 
                                                   MeanR_24hr, 
                                                   `Insurance Status`, Sex, Age)



stats_dta24 %>% tbl_summary(by = `Insurance Status`, label= list(MeanT_24hr ~ "Temperature", MeanR_24hr ~ "Relative Humidity"),
                          digits = list(all_continuous() ~ c(2, 2)))%>% modify_header(label ~ "**Variable**") %>%
  add_p() %>% add_overall()%>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Insurance Status**")%>% bold_labels() %>% bold_p(t = .05)%>%
  as_gt() %>% gt::tab_header(title =  md("**Table 1.** Descriptive Statistics of NYS MI Admissions (1995-2015)"))%>%
  tab_source_note("*24 hour average preceding event")

######################
#48 hr
######################
stats_dta48 <- df_3 %>% filter(Case ==1) %>% select(MeanT_48hr, 
                                                   MeanR_48hr, 
                                                   `Insurance Status`, Sex, Age)



stats_dta48 %>% tbl_summary(by = `Insurance Status`, label= list(MeanT_48hr ~ "Temperature", MeanR_48hr ~ "Relative Humidity"),
                          digits = list(all_continuous() ~ c(2, 2)))%>% modify_header(label ~ "**Variable**") %>%
  add_p() %>% add_overall()%>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Insurance Status**")%>% bold_labels() %>% bold_p(t = .05)%>%
  as_gt() %>% gt::tab_header(title =  md("**Table 1.** Descriptive Statistics of NYS MI Admissions (1995-2015)"))%>%
  tab_source_note("*48 hour average preceding event")




                          

                        


#write.fst(insured, "insured.fst")
#write.fst(uninsured, "uninsured.fst")


mean()
mean_RH<- mean(df_3$MeanR_6hr)

mean_full<- mean(df_3$MeanT_6hr)
median_full <- median(df_3$MeanT_6hr)
#95 percentile temp
df_95_6h <- df_3[df_3$MeanT_6hr >= quantile(df_3$MeanT_6hr, 0.95), ]
df_95_6h_temp<- min(df_95_6h$MeanT_6hr)

#99 percentile temp
df_99_6h <- df_3[df_3$MeanT_6hr >= quantile(df_3$MeanT_6hr, 0.99), ]
df_99_6h_temp <-min(df_99_6h$MeanT_6hr)

#5 percentile temp
df_5_6h <- df_3[df_3$MeanT_6hr <= quantile(df_3$MeanT_6hr, 0.05), ]
df_5_6h_temp<- max(df_5_6h$MeanT_6hr)


#1 percentile temp
df_1_6h <- df_3[df_3$MeanT_6hr <= quantile(df_3$MeanT_6hr, 0.01), ]
df_1_6h_temp<-max(df_1_6h$MeanT_6hr)




temps_6h<- rbind(mean_full, median_full,df_95_6h_temp,df_99_6h_temp,df_5_6h_temp,df_1_6h_temp)




#histograms
hist(df_3$MeanT_6hr)
hist(df_3$MeanT_12hr)
hist(df_3$MeanT_24hr)
hist(df_3$MeanT_48hr)



















#picking the best df for the model

#Construct models 
mod.ns.2 <- clogit(Case ~ ns(MeanT_6hr, df=2)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)


mod.ns.3 <- clogit(Case ~ ns(MeanT_6hr, df=3)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.4 <- clogit(Case ~ ns(MeanT_6hr, df=4)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_6hr, df=5)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanT_6hr, df=6)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 
aic.mod.ns.2 <- AIC(mod.ns.2)
aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
aic.mod.ns.6 <- AIC(mod.ns.6)


# Put AIC's into a table 

models_aic<- data.frame( c( "2 df","3 df", "4 df", "5 df","6 df"),
             c(aic.mod.ns.2, aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5, aic.mod.ns.6))
names(models_aic) <- c("ModelName", "AIC")
models_aic
models_aic$ModelName[which(models_aic$AIC==min(models_aic$AIC))]

#3 df has the lowest for 6 hr, let me see if this is the same for 12

#Construct models 

mod.ns.3 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.4 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 

aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
aic.mod.ns.6 <- AIC(mod.ns.6)


models_aic<- data.frame( c( "3 df", "4 df", "5 df","6 df"),
                         c(aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5, aic.mod.ns.6))
names(models_aic) <- c("ModelName", "AIC")
models_aic

#3 df is the lowest here as well


#Testing for 24:

#Construct models 

mod.ns.3 <- clogit(Case ~ ns(MeanT_24hr, df=3)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.4 <- clogit(Case ~ ns(MeanT_24hr, df=4)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_24hr, df=5)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanT_24hr, df=6)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 

aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
aic.mod.ns.6 <- AIC(mod.ns.6)


models_aic<- data.frame( c( "3 df", "4 df", "5 df","6 df"),
                         c(aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5, aic.mod.ns.6))
names(models_aic) <- c("ModelName", "AIC")
models_aic

#3 df lower for 24 hours

#Finally, looking at 48
#Construct models 

mod.ns.3 <- clogit(Case ~ ns(MeanT_24hr, df=3)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.4 <- clogit(Case ~ ns(MeanT_24hr, df=4)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_24hr, df=5)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanT_24hr, df=6)+
                     ns(MeanR_48hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 

aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
aic.mod.ns.6 <- AIC(mod.ns.6)


models_aic<- data.frame( c( "3 df", "4 df", "5 df","6 df"),
                         c(aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5, aic.mod.ns.6))
names(models_aic) <- c("ModelName", "AIC")
models_aic

#df3 is the lowest here




#Construct models 

mod.ns.3 <- clogit(Case ~ ns(MeanT_6hr, df=3)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.4 <- clogit(Case ~ ns(MeanT_6hr, df=4)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_6hr, df=5)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanT_6hr, df=6)+
                     ns(MeanR_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 

aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
aic.mod.ns.6 <- AIC(mod.ns.6)


# Put AIC's into a table 

models_aic<- data.frame( c( "3 df", "4 df", "5 df","6 df"),
                         c(aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5, aic.mod.ns.6))
names(models_aic) <- c("ModelName", "AIC")
models_aic

# df has the lowest for 6 hr, let me see if this is the same for 12

#Construct models 
mod.ns.1 <- clogit(Case ~ ns(MeanR_12hr, df=1)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.2 <- clogit(Case ~ ns(MeanR_12hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.3 <- clogit(Case ~ ns(MeanR_12hr, df=3)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.4 <- clogit(Case ~ ns(MeanR_12hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanR_12hr, df=5)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.6 <- clogit(Case ~ ns(MeanR_12hr, df=6)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

#Extract AIC of each model 

aic.mod.ns.1 <- AIC(mod.ns.1)
aic.mod.ns.2 <- AIC(mod.ns.2)
aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)



models_aic<- data.frame( c( "1 df", "2 df", "3 df","4 df", "5 df"),
                         c(aic.mod.ns.1, aic.mod.ns.2, aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5))
names(models_aic) <- c("ModelName", "AIC")
models_aic
models_aic$ModelName[which(models_aic$AIC==min(models_aic$AIC))]


#48 hr: 2 df
#24 hr: 1
#12 hr:
#6 hr:

#Temp by itself
#Construct models 

mod.ns.2 <- clogit(Case ~ ns(MeanT_6hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.3 <- clogit(Case ~ ns(MeanT_6hr, df=3)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.4 <- clogit(Case ~ ns(MeanT_6hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.5 <- clogit(Case ~ ns(MeanT_6hr, df=5)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
#Extract AIC of each model 

aic.mod.ns.2 <- AIC(mod.ns.2)
aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)
aic.mod.ns.5 <- AIC(mod.ns.5)
models_aic<- data.frame( c( "2 df", "3 df", "4 df","5 df"),
                         c(aic.mod.ns.2, aic.mod.ns.3, aic.mod.ns.4, aic.mod.ns.5))
names(models_aic) <- c("ModelName", "AIC")
models_aic

#48 hr: 3 
#24 hr: 3
#12 hr: 3
#6 hr: 3


###############################################################################

#Temp and RH together

mod.ns.2.2 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                     ns(MeanR_12hr, df=2)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)
mod.ns.2.3 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                     ns(MeanR_12hr, df=3)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.2.4 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                     ns(MeanR_12hr, df=4)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.2.5 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                     ns(MeanR_12hr, df=5)+ 
                     strata(EventID),     
                   method = "efron",
                   data = df_3)

mod.ns.2.6 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.2.7 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.2.2 <- AIC(mod.ns.2.2)
aic.mod.ns.2.3 <- AIC(mod.ns.2.3)
aic.mod.ns.2.4 <- AIC(mod.ns.2.4)
aic.mod.ns.2.5 <- AIC(mod.ns.2.5)
aic.mod.ns.2.6 <- AIC(mod.ns.2.6)
aic.mod.ns.2.7 <- AIC(mod.ns.2.7)

mod.ns.3.2 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.3.3 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.3.4 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.3.5 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.3.6 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.3.5 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.3.2 <- AIC(mod.ns.3.2)
aic.mod.ns.3.3 <- AIC(mod.ns.3.3)
aic.mod.ns.3.4 <- AIC(mod.ns.3.4)
aic.mod.ns.3.5 <- AIC(mod.ns.3.5)
aic.mod.ns.3.6 <- AIC(mod.ns.3.6)
aic.mod.ns.3.7 <- AIC(mod.ns.3.7)

mod.ns.4.2 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.4.3 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.4.4 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.4.5 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.4.6 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.4.7 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.4.2 <- AIC(mod.ns.4.2)
aic.mod.ns.4.3 <- AIC(mod.ns.4.3)
aic.mod.ns.4.4 <- AIC(mod.ns.4.4)
aic.mod.ns.4.5 <- AIC(mod.ns.4.5)
aic.mod.ns.4.6 <- AIC(mod.ns.4.6)
aic.mod.ns.4.7 <- AIC(mod.ns.4.7)

mod.ns.5.2 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.5.3 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.5.4 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.5.5 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.5.6 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.5.7 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.5.2 <- AIC(mod.ns.5.2)
aic.mod.ns.5.3 <- AIC(mod.ns.5.3)
aic.mod.ns.5.4 <- AIC(mod.ns.5.4)
aic.mod.ns.5.5 <- AIC(mod.ns.5.5)
aic.mod.ns.5.6 <- AIC(mod.ns.5.6)
aic.mod.ns.5.7 <- AIC(mod.ns.5.7)

mod.ns.6.2 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.6.3 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.6.4 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.6.5 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.6.6 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.6.7 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.6.2 <- AIC(mod.ns.6.2)
aic.mod.ns.6.3 <- AIC(mod.ns.6.3)
aic.mod.ns.6.4 <- AIC(mod.ns.6.4)
aic.mod.ns.6.5 <- AIC(mod.ns.6.5)
aic.mod.ns.6.6 <- AIC(mod.ns.6.6)
aic.mod.ns.6.7 <- AIC(mod.ns.6.7)

mod.ns.7.2 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.7.3 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.7.4 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.7.5 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.7.6 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.7.7 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 

aic.mod.ns.7.2 <- AIC(mod.ns.7.2)
aic.mod.ns.7.3 <- AIC(mod.ns.7.3)
aic.mod.ns.7.4 <- AIC(mod.ns.7.4)
aic.mod.ns.7.5 <- AIC(mod.ns.7.5)
aic.mod.ns.7.6 <- AIC(mod.ns.7.6)
aic.mod.ns.7.7 <- AIC(mod.ns.7.7)


# Put AIC's into a table 

models_aic<- data.frame( c( "2 2 df", "2 3 df", "2 4 df","2 5 df", "2 6 df", "2 7 df",
                            "3 2 df", "3 3 df", "3 4 df","3 5 df", "3 6 df", "3 7 df",
                            "4 2 df", "4 3 df", "4 4 df","4 5 df","4 6 df", "4 7 df",
                            "5 2 df", "5 3 df", "5 4 df","5 5 df","5 6 df", "5 7 df",
                            "6 2 df", "6 3 df", "6 4 df","6 5 df","6 6 df", "6 7 df",
                            "7 2 df", "7 3 df", "7 4 df","7 5 df","7 6 df", "7 7 df"),
                         c(aic.mod.ns.2.2, aic.mod.ns.2.3, aic.mod.ns.2.4, aic.mod.ns.2.5,aic.mod.ns.2.6,aic.mod.ns.2.7,
                           aic.mod.ns.3.2, aic.mod.ns.3.3, aic.mod.ns.3.4, aic.mod.ns.3.5,aic.mod.ns.3.6,aic.mod.ns.3.7,
                           aic.mod.ns.4.2, aic.mod.ns.4.3, aic.mod.ns.4.4, aic.mod.ns.4.5,aic.mod.ns.4.6,aic.mod.ns.4.7,
                           aic.mod.ns.5.2, aic.mod.ns.5.3, aic.mod.ns.5.4, aic.mod.ns.5.5,aic.mod.ns.5.6,aic.mod.ns.5.7,
                           aic.mod.ns.6.2, aic.mod.ns.6.3, aic.mod.ns.6.4, aic.mod.ns.6.5,aic.mod.ns.6.6,aic.mod.ns.6.7,
                           aic.mod.ns.7.2, aic.mod.ns.7.3, aic.mod.ns.7.4, aic.mod.ns.7.5,aic.mod.ns.7.6,aic.mod.ns.7.7))
names(models_aic) <- c("ModelName", "AIC")
models_aic
models_aic$ModelName[which(models_aic$AIC==min(models_aic$AIC))]

#6 hr - 3,5