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
setwd("~/Marianthis_Lab/Data_2/generating_predictions")

unins.pred.48hr <- read.csv("unins.48hr.pred.full.csv")
ins.pred.48hr <- read.csv("ins.48hr.pred.full.csv")

#get the log format

unins.pred.48hr <- unins.pred.48hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
unins.pred.48hr <- unins.pred.48hr %>% mutate(log.lci.or = log(lci.or))
unins.pred.48hr <- unins.pred.48hr %>% mutate(log.uci.or = log(uci.or))
unins.pred.48hr <- unins.pred.48hr %>% mutate(log.width = log.uci.or - log.lci.or)
unins.pred.48hr <- unins.pred.48hr %>% mutate(log.se = log.width / (2*1.96))
unins.pred.48hr <- unins.pred.48hr %>% mutate(ins.type = "uninsured")


ins.pred.48hr <- ins.pred.48hr %>% mutate(log.fit.or = log(fit.or)) %>% select(-1)
ins.pred.48hr <- ins.pred.48hr %>% mutate(log.lci.or = log(lci.or))
ins.pred.48hr <- ins.pred.48hr %>% mutate(log.uci.or = log(uci.or))
ins.pred.48hr <- ins.pred.48hr %>% mutate(log.width = log.uci.or - log.lci.or)
ins.pred.48hr <- ins.pred.48hr %>% mutate(log.se = log.width / (2*1.96))
ins.pred.48hr <- ins.pred.48hr %>% mutate(ins.type = "insured")

#join together
predictions<- full_join(unins.pred.48hr, ins.pred.48hr)


#generate predictions
#alpha-- subtact log )) in group 2 from group 1
alpha <- unins.pred.48hr$log.fit.or - ins.pred.48hr$log.fit.or

#se(alpha) -- sqrt(se1^2 + se2^2)
se.alpha <- sqrt((unins.pred.48hr$log.se**2) + (ins.pred.48hr$log.se**2))

#calculating condfidence intervals
upper.alpha <- alpha + (1.96*se.alpha)
lower.alpha <- alpha - (1.96*se.alpha)

#calculating z

z <- alpha/se.alpha

#ratio of relative OR
ROR <- exp(alpha)
ROR.upper <- exp(upper.alpha)
ROR.lower <- exp(lower.alpha)

Percentile <- ins.pred.48hr %>% select(Percentile, TempVar)

pvalue2sided=2*pnorm(-abs(z))
#save into table
strat_results_48h <- as.data.frame(cbind(Percentile$Percentile, Percentile$TempVar, alpha, se.alpha, lower.alpha, 
                                         upper.alpha, z, ROR, ROR.lower, ROR.upper,pvalue2sided))


names(strat_results_48h)[1] <- "Percentile"
names(strat_results_48h)[2] <- "Temperature"

#export
setwd("~/Marianthis_Lab/Data_2/calculating_OR")
write.csv(strat_results_48h, "results.48h.full.csv")


strat_results_48h 


