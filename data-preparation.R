# load packages
library(tidyverse)
library(sjmisc)
library(scales)

rm(list = ls())

# load data (so far pre-test data)
#load("Z:/3_current_Pretest/Pretest_W53.RData")

# internal release
load("//sfb884-share.ad.uni-mannheim.de/data$/2_Data/data_for_R_users/GIP_W53_V1.Rdata")
# later, the data will be made public

data <- GIP_W53_V1
names(data)


# pre-attitude
frq(data$CE53482)

data$pre_attitude <- ifelse(data$CE53482 < 0, NA, data$CE53482)
frq(data$pre_attitude)

# random assignment
frq(data$expCE53483)

data$exp_group <- fct_drop(recode(data$expCE53483,
                         "1. Gruppe 1" = "control",
                         "2. Gruppe 2" = "neutral-correction",
                         "3. Gruppe 3" = "counter-frame",
                         "4. Gruppe 4" = "low-misinfo",
                         "5. Gruppe 5" = "low-misinfo-neutral-correction",
                         "6. Gruppe 6" = "low-misinfo-counter-frame",
                         "7. Gruppe 7" = "high-misinfo",
                         "8. Gruppe 8" = "high-misinfo-neutral-correction",
                         "9. Gruppe 9" = "high-misinfo-counter-frame"))
frq(data$exp_group)

# exclude cases with no random assignment
data <- data[complete.cases(data$exp_group),]

data <- data %>% 
  mutate(correction = case_when(
    exp_group == "control" | exp_group == "low-misinfo" | exp_group == "high-misinfo" ~ "none",
    exp_group == "neutral-correction" | exp_group == "low-misinfo-neutral-correction" | exp_group == "high-misinfo-neutral-correction" ~ "neutral",
    exp_group == "counter-frame" | exp_group == "low-misinfo-counter-frame" | exp_group == "high-misinfo-counter-frame" ~ "counterframe"
  ) %>% 
    fct_relevel("none", "neutral", "counterframe"),
  misinfo = case_when(
    exp_group == "control" | exp_group == "neutral-correction" | exp_group == "counter-frame" ~ "none",
    exp_group == "low-misinfo" | exp_group == "low-misinfo-neutral-correction" | exp_group == "low-misinfo-counter-frame" ~ "low",
    exp_group == "high-misinfo" | exp_group == "high-misinfo-neutral-correction"| exp_group == "high-misinfo-counter-frame" ~ "high",
  ) %>% 
    fct_relevel("none", "low", "high")
  )

# knowledge (safe)
frq(data$CE53484)
data$knowledge_safe <- ifelse(data$CE53484 < 0, NA, data$CE53484)
frq(data$knowledge_safe)

# knowledge (new) --> but I will leave this one out, because it is not clear what to expect
frq(data$CE53485)
data$knowledge_new <- ifelse(data$CE53485 < 0, NA, data$CE53485) 
frq(data$knowledge_new)

cor(data$CE53484, data$CE53485, use = "pairwise.complete.obs")

# opinion for policy restriction
frq(data$CE53486)
data$policy_opinion <- ifelse(data$CE53486 < 0, NA, data$CE53486)
frq(data$policy_opinion)

# extremity of policy opinion
data$opinion_extremity <- abs(data$policy_opinion - 4) 

# change pre-attitude to policy opinion
data$attitude_change <- data$policy_opinion - data$pre_attitude

# moral conviction
frq(data$CE53487)
data$moral_conviction1 <- ifelse(data$CE53487 == "-90. item nonresponse", NA, data$CE53487)
frq(data$moral_conviction1)

frq(data$CE53488)
data$moral_conviction2 <- ifelse(data$CE53488 == "-90. item nonresponse", NA, data$CE53488)
frq(data$moral_conviction2)

cor(data$moral_conviction1, data$moral_conviction2, use = "pairwise.complete.obs")


data$moral_conviction <- data$moral_conviction1-5 + data$moral_conviction2-5
frq(data$moral_conviction)

# disgust
frq(data$CE53489)
data$disgust <- ifelse(data$CE53489 == "-90. item nonresponse", NA, as.numeric(data$CE53489)-5)
frq(data$disgust)

# anger
frq(data$CE53490)
data$anger <- ifelse(data$CE53490 == "-90. item nonresponse", NA, as.numeric(data$CE53490)-5)
frq(data$anger)

# anxiety
frq(data$CE53491)
data$anxiety <- ifelse(data$CE53491 == "-90. item nonresponse", NA, as.numeric(data$CE53491)-5)
frq(data$anxiety)

# sadness
frq(data$CE53492)
data$sadness <- ifelse(data$CE53492 == "-90. item nonresponse", NA, as.numeric(data$CE53492)-5)
frq(data$sadness)

# amb positive
frq(data$CE53493)
data$positive <- ifelse(as.numeric(data$CE53493)-5 < 0, NA,as.numeric(data$CE53493)-5) 
frq(data$positive)

# amb negative
frq(data$CE53494)
data$negative <- ifelse(as.numeric(data$CE53494)-5 < 0, NA,as.numeric(data$CE53494)-5) 
frq(data$negative)

# index potential ambivalence
data$potential_ambivalence <- (data$positive + data$negative)/2 - abs(data$positive-data$negative)
frq(data$potential_ambivalence)

data %>% 
  group_by(exp_group) %>% 
  summarize(mean = mean(rescale(potential_ambivalence), na.rm = T)) %>% 
  ggplot(aes(x = exp_group, y = mean)) +
  geom_col() +
  coord_flip()


# amb conflicted
frq(data$CE53495)
data$conflicted <- ifelse(data$CE53495 < 0, NA, data$CE53495)
frq(data$conflicted)

# amb mixed
frq(data$CE53496)
data$mixed <- ifelse(data$CE53496 < 0, NA, data$CE53496)
frq(data$mixed)

# amb indecisive
frq(data$CE53497)
data$indecisive <- ifelse(data$CE53497 < 0, NA, data$CE53497)
frq(data$indecisive)

cor(data$CE53495, data$CE53496, use = "pairwise.complete.obs")
cor(data$CE53496, data$CE53497, use = "pairwise.complete.obs")
cor(data$CE53495, data$CE53497, use = "pairwise.complete.obs")

# felt ambivalence
data$felt_ambivalence <- (data$conflicted + data$mixed + data$indecisive)/3
frq(data$felt_ambivalence)

data %>% 
  group_by(exp_group) %>% 
  summarize(mean = mean(rescale(felt_ambivalence), na.rm = T)) %>% 
  ggplot(aes(x = exp_group, y = mean)) +
  geom_col() +
  coord_flip()


# info search
frq(data$CE53498)
data$info_search <- ifelse(data$CE53498 == "1. Stellungnahme fÃ¼r den Einsatz neuer gentechnischer Verfahren" |
                             data$CE53498 == "2. Stellungnahme gegen den Einsatz neuer gentechnischer Verfahren", 1, 0)
frq(data$info_search)

# select relevant data
data <- subset(data, select= c(id_g, pre_attitude:info_search))

#save(data, file = "./data/pre-test.R")
