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
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured")

unins_data <- read.fst("./uninsuredLT65.fst")
pub_data <- read.fst("./pubLT65.fst")
priv_data <- read.fst("./privLT65.fst")



# Define a function that accepts a dataset as an argument
run_predictions <- function(data) {
  
  # make the onebasis object
  ob.TempVar <- onebasis(data$MeanT_6hr, fun = "ns", df = 3)
  
  # fit model
  mod <- clogit(Case ~ ob.TempVar +
                  ns(MeanT_6hr,3) + ns(MeanR_6hr,2)  +
                  strata(EventID), data, ties = "efron")
  
  # get the predictions
  pred <- crosspred(ob.TempVar,
                    mod,
                    cen = median.Temp6hr, bylag = 0, df_3, # reference exposure value
                    at = temps.of.interest)
  
  
  # Extract coefficient fit  
  Percentile<- c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1)
  
  fit.table <- as.data.frame(pred$matRRfit)  
  colnames(fit.table) <- paste0("fit.or")
  fit.table <- fit.table %>%  
    mutate(TempVar = as.numeric(row.names(fit.table)))
  Percentile <- as.data.frame(Percentile)
  
  # Extract 95% CI  
  lci.table <- as.data.frame(pred$matRRlow)  
  colnames(lci.table) <- paste0("lci.or")
  uci.table <- as.data.frame(pred$matRRhigh)  
  colnames(uci.table) <- paste0("uci.or")
  
  # Combine fit and se
  # note that all OR are relative to the median of that variability metric
  pred.table <- bind_cols(Percentile,fit.table, lci.table, uci.table,)
  
  # Save prediction tables
  filename <- paste0("pred_", deparse(substitute(data)), "_6-LT65.csv")
  setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured/predictions")
  write.csv(pred.table, filename)
  
}

# Call the function for each dataset
run_predictions(unins_data)
run_predictions(pub_data)
run_predictions(priv_data)
