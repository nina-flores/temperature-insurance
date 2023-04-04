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
#set working directory
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data")

df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")


df_3 <- rbind(df_1, df_2)

median.Temp6hr <- median(df_3$MeanT_6hr)
min.Temp6hr <- min(df_3$MeanT_6hr)
max.Temp6hr <- max(df_3$MeanT_6hr)
percentile.95.Temp6hr <-  quantile(df_3$MeanT_6hr, 0.95)

percentile.5.Temp6hr <- quantile(df_3$MeanT_6hr, 0.05)

temps.of.interest <-quantile(df_3$MeanT_6hr, c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1))


#input data 
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data")
unins_data <- read.fst("./uninsured.fst")

unins_data <- unins_data %>% filter(Age == "< 65")

# make the onebasis object
ob.TempVar <- onebasis(unins_data$MeanT_6hr, fun = "ns", df = 3)

# fit model
mod <- clogit(Case ~ ob.TempVar +
               ns(MeanT_6hr,3) + ns(MeanR_6hr,2)  +
               strata(EventID),unins_data, ties = "efron")

# get the predictions
pred <- crosspred(ob.TempVar,
                  mod,
                  cen = median.Temp6hr, bylag = 0, df_3, # reference exposure value
                  at = temps.of.interest)
                  

# 5c Extract coefficient fit  
Percentile<- c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1)

fit.table <- as.data.frame(pred$matRRfit)  
colnames(fit.table) <- paste0("fit.or")
fit.table <- fit.table %>%  
  mutate(TempVar = as.numeric(row.names(fit.table)))
Percentile <- as.data.frame(Percentile)

# 5d Extract 95% CI  
lci.table <- as.data.frame(pred$matRRlow)  
colnames(lci.table) <- paste0("lci.or")
uci.table <- as.data.frame(pred$matRRhigh)  
colnames(uci.table) <- paste0("uci.or")

# 5e Combine fit and se
# note that all OR are relative to the median of that variability metric
pred.table <- bind_cols(Percentile,fit.table, lci.table, uci.table,)

# 5f Save prediction tables
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/analyses/stratified analyses/stratified < 65/output_65-")

write.csv(pred.table, "strat.unins.6hr.pred.full.csv")



