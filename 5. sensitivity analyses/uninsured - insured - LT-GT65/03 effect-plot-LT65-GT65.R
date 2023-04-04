require(dplyr)
require(tidyr)
require(ggplot2)
require(cowplot)


setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/analyses/calculating OR/stratified < 65/output-")
###########################
#6 hr

results6 <- read.csv("./strat.results.6h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "6 hours") %>%
  mutate(Percentile = Percentile*100)

#12 hr

results12 <- read.csv("./strat.results.12h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "12 hours") %>%
  mutate(Percentile = Percentile*100)

#24 hr

results24 <- read.csv("./strat.results.24h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "24 hours") %>%
  mutate(Percentile = Percentile*100)

#48 hr

results48 <- read.csv("./strat.results.48h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "48 hours") %>%
  mutate(Percentile = Percentile*100)



df <- rbind(results6, results12,results24,results48)

df$Window <- factor(df$Window, levels = c("6 hours","12 hours","24 hours","48 hours"))

ggplot(df, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. insured)")+
  xlab("Percentile of temperature")+
  theme(legend.position=c(.85,.85))




################################################################################
df2 <- rbind(results6, results12)

df2$Window <- factor(df2$Window, levels = c("6 hours","12 hours"))


LT65 <- ggplot(df2, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. insured)")+
  xlab("Percentile of temperature") +
  theme(legend.position=c(.85,.85)) + ggtitle("<65")+ ylim(-35,85)



require(dplyr)
require(tidyr)
require(ggplot2)



setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/analyses/calculating OR/stratified > 65/output-")
###########################
#6 hr

results6 <- read.csv("./strat.results.6h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "6 hours") %>%
  mutate(Percentile = Percentile*100)

#12 hr

results12 <- read.csv("./strat.results.12h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "12 hours") %>%
  mutate(Percentile = Percentile*100)

#24 hr

results24 <- read.csv("./strat.results.24h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "24 hours") %>%
  mutate(Percentile = Percentile*100)

#48 hr

results48 <- read.csv("./strat.results.48h.full.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "48 hours") %>%
  mutate(Percentile = Percentile*100)



df <- rbind(results6, results12,results24,results48)

df$Window <- factor(df$Window, levels = c("6 hours","12 hours","24 hours","48 hours"))

ggplot(df, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. insured)")+
  xlab("Percentile of temperature")+
  theme(legend.position=c(.85,.85)) 




################################################################################
df2 <- rbind(results6, results12)

df2$Window <- factor(df2$Window, levels = c("6 hours","12 hours"))


GTE65 <- ggplot(df2, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. insured)")+
  xlab("Percentile of temperature") +
  theme(legend.position=c(.85,.85)) + ggtitle("65+")+ylim(-35,85)

a <- plot_grid(LT65, GTE65, ncol = 2)

ggsave(file= "age_sens.pdf", a , width = 11, height = 8.5)






