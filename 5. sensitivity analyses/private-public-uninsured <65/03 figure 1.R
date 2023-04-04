
require(fst)
require(dplyr)
require(tidyr)
require(gtsummary)
require(usethis)
require(survival)
require(splines)
require(ggplot2)
require(pspline)
require(ggtext)
require(cowplot)

setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data")

df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")

df_3 <- rbind(df_1, df_2)

percentiles_6hr <-quantile(df_3$MeanT_6hr, c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1))
percentiles_12hr <-quantile(df_3$MeanT_12hr, c(0,.05, .1,.15, .2, .25,.3, .35, .4,.45,.5,.55,.6,.65, .7, .75,.80,.85,.90,.95,1))


#input data 
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured")

unins_data <- read.fst("./uninsuredLT65.fst")
pub_data <- read.fst("./pub.fstLT65")
priv_data <- read.fst("./privLT65.fst")


# define empty list to store plots
plots_list <- list()

# define a function to run the analysis on a given dataset
run_analysis <- function(data_file, title) {

  
  # read data from file
  data <- data_file
  
  #Running the Model on 6 hour mean temp, with 3 degrees of freedom for temp and 2 for RH chosen with AIC
  model_6hr <- clogit(Case ~ ns(MeanT_6hr, df=3)+
                        ns(MeanR_6hr, df=2)+ 
                        strata(EventID),     
                      method = "efron",
                      data = data) 
  summary(model_6hr)
  
  #Pull out the coefficients
  coeff.mat <- summary(model_6hr)$coef
  coeff.mat
  
  #Plot the results for 6 hours
  pred_6hr <- predict(model_6hr, type = "terms", se = TRUE)
  pred_6hr <- as.data.frame(pred_6hr) 
  colnames(pred_6hr) <- c("termTemperature.ns.3",  "termRH.ns.2", "seTemperature.ns.3", "seRH.ns.2")
  data.pred_6hr <- data %>% bind_cols(pred_6hr)
  data.pred_6hr <- data.pred_6hr %>%
    mutate(fit.or =100*( exp(termTemperature.ns.3)-1),
           lci.or = 100*(exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3)-1), 
           uci.or = 100*(exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3)-1))
  
  
 
  
  plot_6hr <- data.pred_6hr %>% 
    sample_frac(0.1) %>%
    ggplot(aes(x=MeanT_6hr)) +  
    geom_line(aes(y = fit.or), color = "black", size = 1) +
    geom_line(aes(y = lci.or), color = "black", linetype = "dashed") + 
    geom_line(aes(y = uci.or), color = "black", linetype = "dashed") +  
    xlab(expression("Mean Temperature 0-6 Hours Prior to MI")) + 
    ylab("Percent change in rate of MI")+ ggtitle(title)+
    geom_rug()+
    geom_hline(yintercept = 0,linetype = "dotted" ,color = "#d2b48c" )+
    geom_vline(xintercept = percentiles_6hr[19], color = "#ef8a62")+  
    geom_vline(xintercept = percentiles_6hr[20], color = "#b2182b", alpha=.7)+
    geom_vline(xintercept = percentiles_6hr[2], color = "#2166ac", alpha=.7)+  
    geom_vline(xintercept = percentiles_6hr[3], color = "#67a9cf", alpha=.7)+
    geom_vline(xintercept = percentiles_6hr[11], color = "#d2b48c", alpha=.7)+
    theme_minimal() + theme(panel.grid.major.x = element_blank() ,  panel.grid.minor = element_blank()) +
    ylim(-10,20)
  
 # append(plots_list, list(plot_6hr)) # add plot_6hr to plots_list
  
  model_12hr <- clogit(Case ~ ns(MeanT_12hr, df=3)+
                        ns(MeanR_12hr, df=2)+ 
                        strata(EventID),     
                      method = "efron",
                      data = data) 
  summary(model_12hr)
  
  #Pull out the coefficients
  coeff.mat <- summary(model_12hr)$coef
  coeff.mat
  
  #Plot the results for 6 hours
  pred_12hr <- predict(model_12hr, type = "terms", se = TRUE)
  pred_12hr <- as.data.frame(pred_12hr) 
  colnames(pred_12hr) <- c("termTemperature.ns.3",  "termRH.ns.2", "seTemperature.ns.3", "seRH.ns.2")
  data.pred_12hr <- data %>% bind_cols(pred_12hr)
  data.pred_12hr <- data.pred_12hr %>%
    mutate(fit.or =100*( exp(termTemperature.ns.3)-1),
           lci.or = 100*(exp(termTemperature.ns.3 - 1.96 * seTemperature.ns.3)-1), 
           uci.or = 100*(exp(termTemperature.ns.3 + 1.96 * seTemperature.ns.3)-1))
  

  plot_12hr <- data.pred_12hr %>% 
    sample_frac(0.1) %>%
    ggplot(aes(x=MeanT_12hr)) +  
    geom_line(aes(y = fit.or), color = "black", size = 1) +
    geom_line(aes(y = lci.or), color = "black", linetype = "dashed") + 
    geom_line(aes(y = uci.or), color = "black", linetype = "dashed") +  
    xlab(expression("Mean Temperature 0-12 Hours Prior to MI")) + 
    ylab("Percent change in rate of MI")+ ggtitle(title)+
    geom_rug()+
    geom_hline(yintercept = 0,linetype = "dotted" ,color = "#d2b48c" )+
    geom_vline(xintercept = percentiles_6hr[19], color = "#ef8a62")+  
    geom_vline(xintercept = percentiles_6hr[20], color = "#b2182b", alpha=.7)+
    geom_vline(xintercept = percentiles_6hr[2], color = "#2166ac", alpha=.7)+  
    geom_vline(xintercept = percentiles_6hr[3], color = "#67a9cf", alpha=.7)+
    geom_vline(xintercept = percentiles_6hr[11], color = "#d2b48c", alpha=.7)+
    theme_minimal() + theme(panel.grid.major.x = element_blank() ,  panel.grid.minor = element_blank()) +
    ylim(-10,20)
  

  plots_list <- c(list(plot_6hr), list(plot_12hr))

}

# Call the function for each dataset
#run_analysis(unins_data)
#run_analysis(pub_data)
#run_analysis(priv_data)

plots_list[[1]] <- run_analysis(unins_data, "Uninsured")
plots_list[[2]] <- run_analysis(pub_data, "Publicly Insured") 
plots_list[[3]] <- run_analysis(priv_data, "Privately Insured") 
# combine the plots in a grid

a <- plot_grid(plots_list[[1]][[1]],plots_list[[2]][[1]],plots_list[[3]][[1]],
          plots_list[[1]][[2]],plots_list[[2]][[2]],plots_list[[3]][[2]], ncol = 3)

ggsave(file= "figure1_sensLT65.pdf", a , width = 11, height = 8.5)

