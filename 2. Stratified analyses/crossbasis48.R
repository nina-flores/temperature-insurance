#install.packages("fst")
require(fst)
require(dplyr)
require(tidyr)
require(gtsummary)
require(usethis)
require(survival)
require(splines)
require(ggplot2)
require(dlnm)

#generate values of interest from overall dataset 6hr
setwd("~/Marianthis_Lab/Data_2")

df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")


df_3 <- rbind(df_1, df_2)

median.Temp48hr <- median(df_3$MeanT_48hr)
min.Temp48hr <- min(df_3$MeanT_48hr)
max.Temp48hr <- max(df_3$MeanT_48hr)
percentile.95.Temp48hr <-  quantile(df_3$MeanT_48hr, 0.95)

percentile.5.Temp48hr <- quantile(df_3$MeanT_48hr, 0.05)
temps.of.interest <-quantile(df_3$MeanT_48hr, c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1))


#input data 

unins_data <- read.fst("./uninsured.fst")


# make the onebasis object
ob.TempVar <- onebasis(unins_data$MeanT_48hr, fun = "ns", df = 3)

# fit model
mod <- clogit(Case ~ ob.TempVar +
               ns(MeanT_48hr,3) + ns(MeanR_48hr,2)  +
               strata(EventID),unins_data, ties = "efron")

# get the predictions
pred <- crosspred(ob.TempVar,
                  mod,
                  cen = median.Temp48hr, bylag = 0, df_3, # reference exposure value
                  at = temps.of.interest)


# 5c Extract coefficient fit  
fit.table <- as.data.frame(pred$matRRfit)  
colnames(fit.table) <- paste0("fit.or")
fit.table <- fit.table %>%  
  mutate(TempVar = as.numeric(row.names(fit.table)))

# 5d Extract 95% CI  
lci.table <- as.data.frame(pred$matRRlow)  
colnames(lci.table) <- paste0("lci.or")
uci.table <- as.data.frame(pred$matRRhigh)  
colnames(uci.table) <- paste0("uci.or")

# 5e Combine fit and se
# note that all OR are relative to the mean of that variability metric
pred.table <- bind_cols(fit.table, lci.table, uci.table)


Percentile <- c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1)
# 5e Combine fit and se
# note that all OR are relative to the median of that variability metric
pred.table <- bind_cols(Percentile, fit.table, lci.table, uci.table)
colnames(pred.table)
names(pred.table)[1] <- "Percentile"

# 5f Save prediction tables
setwd("~/Marianthis_Lab/Data_2/generating_predictions")

write.csv(pred.table, "unins.48hr.pred.full.csv")