# load packages
library(tidyverse)
library(sjmisc)
library(scales)
library(here)

# make sure to start with a clean workspace
rm(list = ls())

# load data 
# so far, internal release on the sfb drive
# later (in december) data will be made public through the GESIS data archive
# https://dbk.gesis.org/dbksearch/GDesc2.asp?no=0109&tab=&ll=10&notabs=1&db=E

# if data is already on hard drive, load into workspace
if(file.exists(here("GIP_W53_V1.RData"))) {
load(here("GIP_W53_V1.RData"))
}

# if data is not in the workspace, load from internal data release
if(!exists("GIP_W53_V1")) {
  load("//sfb884-share.ad.uni-mannheim.de/data$/2_Data/data_for_R_users/GIP_W53_V1.Rdata")
}

# put into data object for more intuitive handling while leacing the raw data intact
data <- GIP_W53_V1
names(data)

############################
# recode variables
############################

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

############################
# add demographics from core questionnaire
############################

# if data is already on hard drive, load into workspace
if(file.exists(here("GIP_W49_V21.RData"))) {
  load(here("GIP_W49_V2.RData"))
}

# if data is not in the workspace, load from internal data release
if(!exists("GIP_W49_V21")) {
  load("//sfb884-share.ad.uni-mannheim.de/data$/2_Data/data_for_R_users/GIP_W49_V2.Rdata")
}

names(GIP_W49_V2)

core <- GIP_W49_V2

# gender
frq(GIP_W49_V2$gender_20)
core$gender <- fct_collapse(core$gender_20,
                            "male" = "1. mÃ¤nnlich",
                            "female" = "2. weiblich",
                            NULL = c("-97. trifft nicht zu",
                                     "-91. 'bitte wÃ¤hlen'-Platzhalter",
                                     "-90. item nonresponse",
                                     "-80. Wert nicht plausibel")) 
frq(core$gender)

# age
frq(core$year_of_birth_cat_20)

core$age <- fct_collapse(core$year_of_birth_cat_20,
                         "76-85" = c("1. 1935-1939",
                                     "2. 1940-1944"),
                         "66-75" = c("3. 1945-1949",
                                     "4. 1950-1954"),
                         "56-65" = c("5. 1955-1959",
                                     "6. 1960-1964"),
                         "46-55" = c("7. 1965-1969",
                                     "8. 1970-1974"),
                         "36-45" = c("9. 1975-1979",
                                     "10. 1980-1984"),
                         "26-35" = c("11. 1985-1989",
                                     "12. 1990-1994"),
                         "16-25" = c("13. 1995-1999",
                                     "14. 2000 und spÃ¤ter"),
                         NULL = c("-97. trifft nicht zu",
                                  "-91. 'bitte wÃ¤hlen'-Platzhalter",
                                  "-90. item nonresponse",
                                  "-80. Wert nicht plausibel"))
frq(core$age)

# education
frq(core$educ_school_20)

core$education <- fct_collapse(core$educ_school_20,
                               "low" = c("2. Schule beendet ohne Abschluss",
                                         "3. Volks-/Hauptschulabschluss bzw. Polytechnische Oberschule mit Abschluss 8. oder 9. Klasse"),
                               "mid" = "4. Mittlere Reife, Realschulabschluss bzw. Polytechnische Oberschule mit Abschluss 10. Klasse", 
                               "high1" = "5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)",
                               "high2" = "6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)",
                               "other" = "7. Anderen Schulabschluss: Bitte tragen Sie Ihren Schulabschluss ein: __________________",
                               NULL = c("1. Noch SchÃ¼ler/-in",
                                        "-97. trifft nicht zu",
                                        "-91. 'bitte wÃ¤hlen'-Platzhalter",
                                        "-90. item nonresponse",
                                        "-80. Wert nicht plausibel"))

frq(core$education)

core <- core %>% 
  select(id_g, gender, age, education)

# join recoded core data to main dataframe
data <- left_join(data, core, by = "id_g")

summary(data)
