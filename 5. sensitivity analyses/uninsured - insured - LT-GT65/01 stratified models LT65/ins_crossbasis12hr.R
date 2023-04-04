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

#generate values of interest from overall dataset 12hr
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data")

df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")


df_3 <- rbind(df_1, df_2)

median.Temp12hr <- median(df_3$MeanT_12hr)
min.Temp12hr <- min(df_3$MeanT_12hr)
max.Temp12hr <- max(df_3$MeanT_12hr)
percentile.95.Temp12hr <-  quantile(df_3$MeanT_12hr, 0.95)

percentile.5.Temp12hr <- quantile(df_3$MeanT_12hr, 0.05)

temps.of.interest <-quantile(df_3$MeanT_12hr, c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1))

#input data 

ins_data <- read.fst("./insured.fst")
ins_data <- ins_data %>% filter(Age == "< 65")


# make the onebasis object
ob.TempVar <- onebasis(ins_data$MeanT_12hr, fun = "ns", df = 3)

# fit model
mod <- clogit(Case ~ ob.TempVar +
               ns(MeanT_12hr,3) + ns(MeanR_12hr,2)  +
               strata(EventID),ins_data, ties = "efron")

# get the predictions
pred <- crosspred(ob.TempVar,
                  mod,
                  cen = median.Temp12hr, bylag = 0, df_3, # reference exposure value
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


Percentile <- c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1)
# 5e Combine fit and se
# note that all OR are relative to the median of that variability metric
pred.table <- bind_cols(Percentile, fit.table, lci.table, uci.table)
colnames(pred.table)
names(pred.table)[1] <- "Percentile"

# 5f Save prediction tables
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/analyses/stratified analyses/stratified < 65/output_65-")

write.csv(pred.table, "strat.ins.12hr.pred.full.csv")
