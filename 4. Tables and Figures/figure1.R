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

unins_data <- read.fst("./uninsured.fst")


#Running the MOdel
#on 6 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC
model_uninsured_6hr <- clogit(Case ~ ns(MeanT_6hr, df=3)+
                                ns(MeanR_6hr, df=2)+ 
                                strata(EventID),     
                              method = "efron",
                              data = unins_data) 


summary(model_uninsured_6hr)

#Pull out the coefficients
coeff.mat <- summary(model_uninsured_6hr)$coef
coeff.mat

#Plot the results
#for 6 hours
uninsured_ptemp6h <- predict(model_uninsured_6hr, type = "terms", se = TRUE)
uninsured_ptemp6h <- as.data.frame(uninsured_ptemp6h) 


colnames(uninsured_ptemp6h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                 "seTemperature.ns.3", "seRH.ns.2")
uninsured_dta.pred6h  <- unins_data %>% bind_cols(uninsured_ptemp6h)


uninsured_dta.pred6h <- uninsured_dta.pred6h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


unins_plot.6h <-uninsured_dta.pred6h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_6hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-6 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Uninsured")

unins_plot.6h 


#Running the MOdel
#on 12 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC

model_uninsured_12hr <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                                 ns(MeanR_12hr, df=2)+ 
                                 strata(EventID),     
                               method = "efron",
                               data = unins_data) 




summary(model_uninsured_12hr)

#Pull out the coefficients
coeff.mat <- summary(model_uninsured_12hr)$coef
coeff.mat

#Plot the results
#for 12 hours 
uninsured_ptemp12h <- predict(model_uninsured_12hr, type = "terms", se = TRUE)
uninsured_ptemp12h <- as.data.frame(uninsured_ptemp12h) 


colnames(uninsured_ptemp12h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                  "seTemperature.ns.3", "seRH.ns.2")
uninsured_dta.pred12h  <- unins_data %>% bind_cols(uninsured_ptemp12h)


uninsured_dta.pred12h <- uninsured_dta.pred12h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


unins_plot.12h <-uninsured_dta.pred12h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_12hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-12 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Uninsured")

unins_plot.12h 



#Running the MOdel
#on 24 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC

model_uninsured_24hr <- clogit(Case ~ ns(MeanT_24hr, df=3)+
                                 ns(MeanR_24hr, df=2)+ 
                                 strata(EventID),     
                               method = "efron",
                               data = unins_data) 


summary(model_uninsured_24hr)

#Pull out the coefficients
coeff.mat <- summary(model_uninsured_24hr)$coef
coeff.mat

#Plot the results
#for 24 hours 
uninsured_ptemp24h <- predict(model_uninsured_24hr, type = "terms", se = TRUE)
uninsured_ptemp24h <- as.data.frame(uninsured_ptemp24h) 


colnames(uninsured_ptemp24h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                  "seTemperature.ns.3", "seRH.ns.2")
uninsured_dta.pred24h  <- unins_data %>% bind_cols(uninsured_ptemp24h)


uninsured_dta.pred24h <- uninsured_dta.pred24h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


unins_plot.24h <-uninsured_dta.pred24h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_24hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-24 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Uninsured")

unins_plot.24h 


#Running the MOdel
#on 48 hour mean temp, with 3 degrees of 
#freedom for temp and 4 for RH

model_uninsured_48hr <- clogit(Case ~ ns(MeanT_48hr, df=3)+
                                 ns(MeanR_48hr, df=2)+ 
                                 strata(EventID),     
                               method = "efron",
                               data = unins_data) 


summary(model_uninsured_48hr)

#Pull out the coefficients
coeff.mat <- summary(model_uninsured_48hr)$coef
coeff.mat

#Plot the results
#for 48 hours 
uninsured_ptemp48h <- predict(model_uninsured_48hr, type = "terms", se = TRUE)
uninsured_ptemp48h <- as.data.frame(uninsured_ptemp48h) 


colnames(uninsured_ptemp48h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                  "seTemperature.ns.3", "seRH.ns.2")
uninsured_dta.pred48h  <- unins_data %>% bind_cols(uninsured_ptemp48h)


uninsured_dta.pred48h <- uninsured_dta.pred48h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


unins_plot.48h <-uninsured_dta.pred48h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_48hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-48 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Uninsured")

unins_plot.48h 





#######################Insured##############################


ins_data <- read.fst("./insured.fst")


#Running the MOdel
#on 6 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC
model_insured_6hr <- clogit(Case ~ ns(MeanT_6hr, df=3)+
                              ns(MeanR_6hr, df=2)+ 
                              strata(EventID),     
                            method = "efron",
                            data = ins_data) 


summary(model_insured_6hr)


#Pull out the coefficients
coeff.mat <- summary(model_insured_6hr)$coef
coeff.mat


#Plot the results
#for 6 hours
insured_ptemp6h <- predict(model_insured_6hr, type = "terms", se = TRUE)
insured_ptemp6h <- as.data.frame(insured_ptemp6h) 


colnames(insured_ptemp6h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                               "seTemperature.ns.3", "seRH.ns.2")
insured_dta.pred6h  <- ins_data %>% bind_cols(insured_ptemp6h)


insured_dta.pred6h <- insured_dta.pred6h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


ins_plot.6h <-insured_dta.pred6h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_6hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-6 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Insured")

ins_plot.6h 



#Running the MOdel
#on 12 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC

model_insured_12hr <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                               ns(MeanR_12hr, df=2)+ 
                               strata(EventID),     
                             method = "efron",
                             data = ins_data) 




summary(model_insured_12hr)

#Pull out the coefficients
coeff.mat <- summary(model_insured_12hr)$coef
coeff.mat

#Plot the results
#for 12 hours 
insured_ptemp12h <- predict(model_insured_12hr, type = "terms", se = TRUE)
insured_ptemp12h <- as.data.frame(insured_ptemp12h) 


colnames(insured_ptemp12h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                "seTemperature.ns.3", "seRH.ns.2")
insured_dta.pred12h  <- ins_data %>% bind_cols(insured_ptemp12h)


insured_dta.pred12h <- insured_dta.pred12h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


ins_plot.12h <-insured_dta.pred12h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_12hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-12 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Insured")

ins_plot.12h 



#Running the MOdel
#on 24 hour mean temp, with 3 degrees of 
#freedom for temp and 2 for RH
#chosen with AIC

model_insured_24hr <- clogit(Case ~ ns(MeanT_24hr, df=3)+
                               ns(MeanR_24hr, df=2)+ 
                               strata(EventID),     
                             method = "efron",
                             data = ins_data) 


summary(model_uninsured_24hr)

#Pull out the coefficients
coeff.mat <- summary(model_insured_24hr)$coef
coeff.mat

#Plot the results
#for 24 hours 
insured_ptemp24h <- predict(model_insured_24hr, type = "terms", se = TRUE)
insured_ptemp24h <- as.data.frame(insured_ptemp24h) 


colnames(insured_ptemp24h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                "seTemperature.ns.3", "seRH.ns.2")
insured_dta.pred24h  <- ins_data %>% bind_cols(insured_ptemp24h)


insured_dta.pred24h <- insured_dta.pred24h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


ins_plot.24h <-insured_dta.pred24h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_24hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-24 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Insured")

ins_plot.24h 


#Running the MOdel
#on 48 hour mean temp, with 3 degrees of 
#freedom for temp and 4 for RH

model_insured_48hr <- clogit(Case ~ ns(MeanT_48hr, df=3)+
                               ns(MeanR_48hr, df=2)+ 
                               strata(EventID),     
                             method = "efron",
                             data = ins_data) 


summary(model_uninsured_48hr)

#Pull out the coefficients
coeff.mat <- summary(model_insured_48hr)$coef
coeff.mat

#Plot the results
#for 48 hours 
insured_ptemp48h <- predict(model_insured_48hr, type = "terms", se = TRUE)
insured_ptemp48h <- as.data.frame(insured_ptemp48h) 


colnames(insured_ptemp48h) <- c("termTemperature.ns.3",  "termRH.ns.2", 
                                "seTemperature.ns.3", "seRH.ns.2")
insured_dta.pred48h  <- ins_data %>% bind_cols(insured_ptemp48h)


insured_dta.pred48h <- insured_dta.pred48h  %>%
  mutate(fit.or = exp(termTemperature.ns.3),
         lci.or = exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3), 
         uci.or = exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3))


ins_plot.48h <-insured_dta.pred48h   %>% 
  sample_frac(0.1) %>%
  ggplot(aes(x=MeanT_48hr)) +  
  geom_line(aes(y = fit.or), color = "lightsalmon2", size = 1) +
  geom_line(aes(y = lci.or), color = "grey", linetype = "dashed") + 
  geom_line(aes(y = uci.or), color = "grey", linetype = "dashed") +  
  xlab(expression("Mean Temperature 0-48 Hours Prior to Event")) + 
  ylab("Odds of MI admission")+ ggtitle("Insured")

ins_plot.48h 




#Now I'm going to plot all of these together 
#install.packages("patchwork")
require(patchwork)


#6h
plot.6h.full <-  unins_plot.6h +
                 ins_plot.6h & 
                 scale_y_continuous(limits = c(.9, 1.15)) & 
                 scale_x_continuous(limits = c(-30, 40))

plot.6h.full

#12h
plot.12.full <-  unins_plot.12h +
  ins_plot.12h & 
  scale_y_continuous(limits = c(.9, 1.15)) & 
  scale_x_continuous(limits = c(-30, 40))

plot.12.full

#24h
plot.24.full <-  unins_plot.24h +
  ins_plot.24h & 
  scale_y_continuous(limits = c(.9, 1.15)) & 
  scale_x_continuous(limits = c(-30, 40))

plot.24.full

#48h
plot.48.full <-  unins_plot.48h +
  ins_plot.48h & 
  scale_y_continuous(limits = c(.9, 1.15)) & 
  scale_x_continuous(limits = c(-30, 40))

plot.48.full

require(ggpubr)
full_plot <- ggarrange(plot.6h.full, plot.12.full, plot.24.full, plot.48.full,
                       ncol = 1,
                       nrow = 4) 

fp2 <- annotate_figure(full_plot, top = text_grob("Figure 1. Ambient Temperature and MI Admissions by Insurance Status", size = 18), 
                       bottom = text_grob("Data Source: SPARCS", size = 7))

fp2