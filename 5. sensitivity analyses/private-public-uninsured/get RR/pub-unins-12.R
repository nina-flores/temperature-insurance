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

#read in data
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/analyses/private-public-uninsured/pred")

unins.pred.12hr <- read.csv("pred_unins_data_12.csv")
pub.pred.12hr <- read.csv("pred_pub_data_12.csv")

#get the log format

unins.pred.12hr <- unins.pred.12hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
unins.pred.12hr <- unins.pred.12hr %>% mutate(log.lci.or = log(lci.or))
unins.pred.12hr <- unins.pred.12hr %>% mutate(log.uci.or = log(uci.or))
unins.pred.12hr <- unins.pred.12hr %>% mutate(log.width = log.uci.or - log.lci.or)
unins.pred.12hr <- unins.pred.12hr %>% mutate(log.se = log.width / (2*1.96))
unins.pred.12hr <- unins.pred.12hr %>% mutate(ins.type = "uninsured")


pub.pred.12hr <- pub.pred.12hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
pub.pred.12hr <- pub.pred.12hr %>% mutate(log.lci.or = log(lci.or))
pub.pred.12hr <- pub.pred.12hr %>% mutate(log.uci.or = log(uci.or))
pub.pred.12hr <- pub.pred.12hr %>% mutate(log.width = log.uci.or - log.lci.or)
pub.pred.12hr <- pub.pred.12hr %>% mutate(log.se = log.width / (2*1.96))
pub.pred.12hr <- pub.pred.12hr %>% mutate(pub.type = "public")

#join together
predictions<- full_join(unins.pred.12hr, pub.pred.12hr)


#generate predictions
#alpha-- subtact log )) in group 2 from group 1
alpha <- unins.pred.12hr$log.fit.or - pub.pred.12hr$log.fit.or

#se(alpha) -- sqrt(se1^2 + se2^2)
se.alpha <- sqrt(unins.pred.12hr$log.se**2 + pub.pred.12hr$log.se**2)

#calculating condfidence intervals
upper.alpha <- alpha + 1.96*se.alpha
lower.alpha <- alpha - 1.96*se.alpha

#calculating z

z <- alpha/se.alpha

#ratio of relative OR
ROR <- exp(alpha)
ROR.upper <- exp(upper.alpha)
ROR.lower <- exp(lower.alpha)

Percentile <- unins.pred.12hr %>% select(Percentile, TempVar)

pvalue2sided=2*pnorm(-abs(z))
#save into table
strat_results_12h <- as.data.frame(cbind(Percentile$Percentile, Percentile$TempVar, alpha, se.alpha, lower.alpha, 
                                        upper.alpha, z, ROR, ROR.lower, ROR.upper,pvalue2sided))


names(strat_results_12h)[1] <- "Percentile"
names(strat_results_12h)[2] <- "Temperature"


#export
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured/predictions/RR/output")

write.csv(strat_results_12h, "results.12h.pub.unins.csv")



