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

###############################################################################

#Temp and RH together

mod.ns.1.1 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)


mod.ns.1.2 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=2)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
mod.ns.1.3 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=3)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.1.4 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=4)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.1.5 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=5)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.1.6 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=6)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

mod.ns.1.7 <- clogit(Case ~ ns(MeanT_12hr, df=1)+
                       ns(MeanR_12hr, df=7)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)
#Extract AIC of each model 
aic.mod.ns.1.1 <- AIC(mod.ns.1.1)
aic.mod.ns.1.2 <- AIC(mod.ns.1.2)
aic.mod.ns.1.3 <- AIC(mod.ns.1.3)
aic.mod.ns.1.4 <- AIC(mod.ns.1.4)
aic.mod.ns.1.5 <- AIC(mod.ns.1.5)
aic.mod.ns.1.6 <- AIC(mod.ns.1.6)
aic.mod.ns.1.7 <- AIC(mod.ns.1.7)

#clean up
rm(mod.ns.1.1)
rm(mod.ns.1.2)
rm(mod.ns.1.3)
rm(mod.ns.1.4)
rm(mod.ns.1.5)
rm(mod.ns.1.6)
rm(mod.ns.1.7)



mod.ns.2.1 <- clogit(Case ~ ns(MeanT_12hr, df=2)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.2.1 <- AIC(mod.ns.2.1)
aic.mod.ns.2.2 <- AIC(mod.ns.2.2)
aic.mod.ns.2.3 <- AIC(mod.ns.2.3)
aic.mod.ns.2.4 <- AIC(mod.ns.2.4)
aic.mod.ns.2.5 <- AIC(mod.ns.2.5)
aic.mod.ns.2.6 <- AIC(mod.ns.2.6)
aic.mod.ns.2.7 <- AIC(mod.ns.2.7)

#clean up
rm(mod.ns.2.1)
rm(mod.ns.2.2)
rm(mod.ns.2.3)
rm(mod.ns.2.4)
rm(mod.ns.2.5)
rm(mod.ns.2.6)
rm(mod.ns.2.7)

mod.ns.3.1 <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.3.1 <- AIC(mod.ns.3.1)
aic.mod.ns.3.2 <- AIC(mod.ns.3.2)
aic.mod.ns.3.3 <- AIC(mod.ns.3.3)
aic.mod.ns.3.4 <- AIC(mod.ns.3.4)
aic.mod.ns.3.5 <- AIC(mod.ns.3.5)
aic.mod.ns.3.6 <- AIC(mod.ns.3.6)
aic.mod.ns.3.7 <- AIC(mod.ns.3.7)

#clean up
rm(mod.ns.3.1)
rm(mod.ns.3.2)
rm(mod.ns.3.3)
rm(mod.ns.3.4)
rm(mod.ns.3.5)
rm(mod.ns.3.6)
rm(mod.ns.3.7)


mod.ns.4.1 <- clogit(Case ~ ns(MeanT_12hr, df=4)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.4.1 <- AIC(mod.ns.4.1)
aic.mod.ns.4.2 <- AIC(mod.ns.4.2)
aic.mod.ns.4.3 <- AIC(mod.ns.4.3)
aic.mod.ns.4.4 <- AIC(mod.ns.4.4)
aic.mod.ns.4.5 <- AIC(mod.ns.4.5)
aic.mod.ns.4.6 <- AIC(mod.ns.4.6)
aic.mod.ns.4.7 <- AIC(mod.ns.4.7)

#clean up
rm(mod.ns.4.1)
rm(mod.ns.4.2)
rm(mod.ns.4.3)
rm(mod.ns.4.4)
rm(mod.ns.4.5)
rm(mod.ns.4.6)
rm(mod.ns.4.7)

mod.ns.5.1 <- clogit(Case ~ ns(MeanT_12hr, df=5)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.5.1 <- AIC(mod.ns.5.1)
aic.mod.ns.5.2 <- AIC(mod.ns.5.2)
aic.mod.ns.5.3 <- AIC(mod.ns.5.3)
aic.mod.ns.5.4 <- AIC(mod.ns.5.4)
aic.mod.ns.5.5 <- AIC(mod.ns.5.5)
aic.mod.ns.5.6 <- AIC(mod.ns.5.6)
aic.mod.ns.5.7 <- AIC(mod.ns.5.7)

#clean up
rm(mod.ns.5.1)
rm(mod.ns.5.2)
rm(mod.ns.5.3)
rm(mod.ns.5.4)
rm(mod.ns.5.5)
rm(mod.ns.5.6)
rm(mod.ns.5.7)

mod.ns.6.1 <- clogit(Case ~ ns(MeanT_12hr, df=6)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.6.1 <- AIC(mod.ns.6.1)
aic.mod.ns.6.2 <- AIC(mod.ns.6.2)
aic.mod.ns.6.3 <- AIC(mod.ns.6.3)
aic.mod.ns.6.4 <- AIC(mod.ns.6.4)
aic.mod.ns.6.5 <- AIC(mod.ns.6.5)
aic.mod.ns.6.6 <- AIC(mod.ns.6.6)
aic.mod.ns.6.7 <- AIC(mod.ns.6.7)

#clean up
rm(mod.ns.6.1)
rm(mod.ns.6.2)
rm(mod.ns.6.3)
rm(mod.ns.6.4)
rm(mod.ns.6.5)
rm(mod.ns.6.6)
rm(mod.ns.6.7)

mod.ns.7.1 <- clogit(Case ~ ns(MeanT_12hr, df=7)+
                       ns(MeanR_12hr, df=1)+ 
                       strata(EventID),     
                     method = "efron",
                     data = df_3)

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
aic.mod.ns.7.1 <- AIC(mod.ns.7.1)
aic.mod.ns.7.2 <- AIC(mod.ns.7.2)
aic.mod.ns.7.3 <- AIC(mod.ns.7.3)
aic.mod.ns.7.4 <- AIC(mod.ns.7.4)
aic.mod.ns.7.5 <- AIC(mod.ns.7.5)
aic.mod.ns.7.6 <- AIC(mod.ns.7.6)
aic.mod.ns.7.7 <- AIC(mod.ns.7.7)

#clean up
rm(mod.ns.7.1)
rm(mod.ns.7.2)
rm(mod.ns.7.3)
rm(mod.ns.7.4)
rm(mod.ns.7.5)
rm(mod.ns.7.6)
rm(mod.ns.7.7)


# Put AIC's into a table 

models_aic<- data.frame( c( "1 1 df", "1 2 df", "1 3 df", "1 4 df","1 5 df", "1 6 df", "1 7 df",
                            "2 1 df", "2 2 df", "2 3 df", "2 4 df","2 5 df", "2 6 df", "2 7 df",
                            "3 1 df", "3 2 df", "3 3 df", "3 4 df","3 5 df", "3 6 df", "3 7 df",
                            "4 1 df", "4 2 df", "4 3 df", "4 4 df","4 5 df","4 6 df", "4 7 df",
                            "5 1 df", "5 2 df", "5 3 df", "5 4 df","5 5 df","5 6 df", "5 7 df",
                            "6 1 df",  "6 2 df", "6 3 df", "6 4 df","6 5 df","6 6 df", "6 7 df",
                            "7 1 df", "7 2 df", "7 3 df", "7 4 df","7 5 df","7 6 df", "7 7 df"),
                         c(aic.mod.ns.1.1, aic.mod.ns.1.2, aic.mod.ns.1.3, aic.mod.ns.1.4, aic.mod.ns.1.5,aic.mod.ns.1.6,aic.mod.ns.1.7,
                           aic.mod.ns.2.1, aic.mod.ns.2.2, aic.mod.ns.2.3, aic.mod.ns.2.4, aic.mod.ns.2.5,aic.mod.ns.2.6,aic.mod.ns.2.7,
                           aic.mod.ns.3.1, aic.mod.ns.3.2, aic.mod.ns.3.3, aic.mod.ns.3.4, aic.mod.ns.3.5,aic.mod.ns.3.6,aic.mod.ns.3.7,
                           aic.mod.ns.4.1, aic.mod.ns.4.2, aic.mod.ns.4.3, aic.mod.ns.4.4, aic.mod.ns.4.5,aic.mod.ns.4.6,aic.mod.ns.4.7,
                           aic.mod.ns.5.1, aic.mod.ns.5.2, aic.mod.ns.5.3, aic.mod.ns.5.4, aic.mod.ns.5.5,aic.mod.ns.5.6,aic.mod.ns.5.7,
                           aic.mod.ns.6.1, aic.mod.ns.6.2, aic.mod.ns.6.3, aic.mod.ns.6.4, aic.mod.ns.6.5,aic.mod.ns.6.6,aic.mod.ns.6.7,
                           aic.mod.ns.7.1, aic.mod.ns.7.2, aic.mod.ns.7.3, aic.mod.ns.7.4, aic.mod.ns.7.5,aic.mod.ns.7.6,aic.mod.ns.7.7))
names(models_aic) <- c("ModelName", "AIC")
models_aic
models_aic$ModelName[which(models_aic$AIC==min(models_aic$AIC))]

#6 hr - 3,5
#12 hr - 3,2