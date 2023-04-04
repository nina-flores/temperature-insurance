# creating insured and uninsured datasets:


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
setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_v_pub")


df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")

df_3 <- rbind(df_1, df_2)
df_3 <- df_3 %>% rename( "Insurance Type" = PublicIns,
                         Sex = sex,
                         Age = ageGroup)
df_3$Age <- ifelse(df_3$Age == "AgeGTE65", ">= 65", "< 65")

pub <- df_3 %>% filter(`Insurance Type` == "Public Insurance")
priv <-  df_3 %>% filter(`Insurance Type` == "Not Public Insurance")

setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured")
write_fst(pub, "pub.fst")
write_fst(priv , "priv.fst")


setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data")

df_1 <- read.fst("./de_identified_MI_for_Nina_after2004.fst")
df_2 <- read.fst("./de_identified_MI_for_Nina_before2005.fst")

df_3 <- rbind(df_1, df_2)
df_3 <- df_3 %>% rename( "Insurance Status" = AnyIns,
                         Sex = sex,
                         Age = ageGroup)
df_3$Age <- ifelse(df_3$Age == "AgeGTE65", ">= 65", "< 65")

uninsured <- df_3 %>% filter(`Insurance Status` == "No Insurance (self-pay or work comp)")
insured <-  df_3 %>% filter(`Insurance Status` == "Any Insurance")

setwd("~/Desktop/projects/casey cohort/temperature insurance status emm/data/priv_pub_uninsured")
write_fst(uninsured, "uninsured.fst")
