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


pub.pred.6hr <- read.csv("pred_pub_data_6.csv")
priv.pred.6hr <- read.csv("pred_priv_data_6.csv")

#get the log format

pub.pred.6hr <- pub.pred.6hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
pub.pred.6hr <- pub.pred.6hr %>% mutate(log.lci.or = log(lci.or))
pub.pred.6hr <- pub.pred.6hr %>% mutate(log.uci.or = log(uci.or))
pub.pred.6hr <- pub.pred.6hr %>% mutate(log.width = log.uci.or - log.lci.or)
pub.pred.6hr <- pub.pred.6hr %>% mutate(log.se = log.width / (2*1.96))
pub.pred.6hr <- pub.pred.6hr %>% mutate(priv.type = "public")


priv.pred.6hr <- priv.pred.6hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
priv.pred.6hr <- priv.pred.6hr %>% mutate(log.lci.or = log(lci.or))
priv.pred.6hr <- priv.pred.6hr %>% mutate(log.uci.or = log(uci.or))
priv.pred.6hr <- priv.pred.6hr %>% mutate(log.width = log.uci.or - log.lci.or)
priv.pred.6hr <- priv.pred.6hr %>% mutate(log.se = log.width / (2*1.96))
priv.pred.6hr <- priv.pred.6hr %>% mutate(priv.type = "private")

#join together
predictions<- full_join(pub.pred.6hr, priv.pred.6hr)


#generate predictions
#alpha-- subtact log )) in group 2 from group 1
alpha <- pub.pred.6hr$log.fit.or - priv.pred.6hr$log.fit.or

#se(alpha) -- sqrt(se1^2 + se2^2)
se.alpha <- sqrt(pub.pred.6hr$log.se**2 + priv.pred.6hr$log.se**2)

#calculating condfidence intervals
upper.alpha <- alpha + 1.96*se.alpha
lower.alpha <- alpha - 1.96*se.alpha

#calculating z

z <- alpha/se.alpha

#ratio of relative OR
ROR <- exp(alpha)
ROR.upper <- exp(upper.alpha)
ROR.lower <- exp(lower.alpha)

Percentile <- pub.pred.6hr %>% select(Percentile, TempVar)

pvalue2sided=2*pnorm(-abs(z))
#save into table
strat_results_6h <- as.data.frame(cbind(Percentile$Percentile, Percentile$TempVar, alpha, se.alpha, lower.alpha, 
                                        upper.alpha, z, ROR, ROR.lower, ROR.upper,pvalue2sided))


names(strat_results_6h)[1] <- "Percentile"
names(strat_results_6h)[2] <- "Temperature"


#
#export
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured/predictions/RR/output")

write.csv(strat_results_6h, "results.6h.pub.priv.csv")


