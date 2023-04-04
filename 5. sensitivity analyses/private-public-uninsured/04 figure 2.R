require(dplyr)
require(tidyr)
require(ggplot2)
require(cowplot)



setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured/predictions/RR/output")
###########################
#6 hr

results6 <- read.csv("./results.6h.priv.unins.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "6 hours") %>%
  mutate(Percentile = Percentile*100)

#12 hr

results12 <- read.csv("./results.12h.priv.unins.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "12 hours") %>%
  mutate(Percentile = Percentile*100)

#24 hr

#results24 <- read.csv("./results.24h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "24 hours") %>%
#  mutate(Percentile = Percentile*100)

#48 hr

#results48 <- read.csv("./results.48h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "48 hours") %>%
#  mutate(Percentile = Percentile*100)



df <- rbind(results6, results12)

df$Window <- factor(df$Window, levels = c("6 hours","12 hours"))

a <- ggplot(df, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. privately insured)")+
  xlab("Percentile of temperature")+
  theme(legend.position=c(.85,.85))+
  ylim(-25,40)


################################################################################
#6 hr

results6 <- read.csv("./results.6h.pub.unins.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "6 hours") %>%
  mutate(Percentile = Percentile*100)

#12 hr

results12 <- read.csv("./results.12h.pub.unins.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "12 hours") %>%
  mutate(Percentile = Percentile*100)

#24 hr

#results24 <- read.csv("./results.24h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "24 hours") %>%
#  mutate(Percentile = Percentile*100)

#48 hr

#results48 <- read.csv("./results.48h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "48 hours") %>%
#  mutate(Percentile = Percentile*100)



df <- rbind(results6, results12)

df$Window <- factor(df$Window, levels = c("6 hours","12 hours"))

b <- ggplot(df, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (uninsured vs. publicly insured)")+
  xlab("Percentile of temperature")+
  theme(legend.position=c(.85,.85))+
  ylim(-25,40)


################################################################################
###########################
#6 hr

results6 <- read.csv("./results.6h.pub.priv.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "6 hours") %>%
  mutate(Percentile = Percentile*100)

#12 hr

results12 <- read.csv("./results.12h.pub.priv.csv") %>%
  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
  mutate(Window = "12 hours") %>%
  mutate(Percentile = Percentile*100)

#24 hr

#results24 <- read.csv("./results.24h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "24 hours") %>%
#  mutate(Percentile = Percentile*100)

#48 hr

#results48 <- read.csv("./results.48h.full.csv") %>%
#  select(Percentile, ROR, ROR.lower, ROR.upper) %>%
#  mutate(Window = "48 hours") %>%
#  mutate(Percentile = Percentile*100)



df <- rbind(results6, results12)

df$Window <- factor(df$Window, levels = c("6 hours","12 hours"))

c <- ggplot(df, aes(y = log(ROR)*100, x = Percentile, color = Window, shape = Window)) +
  geom_point(position = position_dodge(width = 2), size = .7) +
  scale_color_manual(values = c("#FC4E07", "#E7B800", "#00AFBB", "#c2a5cf"))+
  geom_pointrange(aes(ymin=log(ROR.lower)*100, ymax=log(ROR.upper)*100),position = position_dodge(width = 2), size = 0.7) +
  theme_minimal(base_size = 15)+ 
  geom_hline(aes(yintercept = 0 ),color = "black" )+ 
  ylab("Percent difference in estimates (publicly insured vs. privately insured)")+
  xlab("Percentile of temperature")+
  theme(legend.position=c(.85,.85))+
  ylim(-25,40)


d <- plot_grid(a,b,c, ncol = 3)


ggsave(file= "figure1_sens.pdf", d , width = 11, height = 8.5)

