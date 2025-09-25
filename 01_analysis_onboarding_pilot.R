library(data.table)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)
library(tibble)
library(psych)
library(careless)

wd <- list()
wd$data   <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/output/"

raw <- fread(paste0(wd$data, "pilot_onboarding_complete_20250923.csv"))

# check attention check
failed_att <- raw %>% 
  filter(Status == 0 & Finished == 1 & consent == 1) %>% 
  filter(motivation_10 != 3)
# 3 people have failed the attention check, though their replies do still seem serious

# Q32 only has a value for the identified condition. Now condition = 0 for intrinsic and condition = 1 for identified.
df <- raw %>% 
  filter(Status == 0 & Finished == 1 & consent == 1) %>% 
  select(36,37,45:55,57:61) %>% 
  mutate(across(c(3, 5:15,17), as.numeric),
         mot_ex = (motivation_1 + motivation_2 + motivation_3) / 3,
         mot_id = (motivation_4 + motivation_5 + motivation_6) / 3,
         mot_in = (motivation_7 + motivation_8 + motivation_9) / 3,
         condition = ifelse(is.na(`Q32_Click Count`), 0, 1),
         condition = factor(condition, labels = c("Intrinsic", "Identified")),
         gender = factor(gender, levels = c(4:7), labels = c("Male", "Female", "Non-binary", "Other")),
         employ = factor(employ)) %>% 
  rownames_to_column(var = "id")

# checking the factor structure of the motivation scale, looks pretty good. just motivation_9 loading could've been higher
df %>% 
  select(starts_with("motivation")) %>% 
  fa(nfactors = 3)

# descriptives
df_long <- df %>%
  pivot_longer(cols = c(mot_ex, mot_id, mot_in),
               names_to = "motivation_type",
               values_to = "score")

ggplot(df_long, aes(x = motivation_type, y = score, fill = condition)) +
  stat_summary(geom = "col", fun = mean, position = "dodge") +
  theme_minimal()

# Outliers / data inspection
boxplot(df$mot_ex)
boxplot(df$mot_id)
boxplot(df$mot_in)

describeBy(df$mot_ex, df$condition)
describeBy(df$mot_id, df$condition)
describeBy(df$mot_in, df$condition)

df %>% 
  select(motivation_1:motivation_3) %>% 
  alpha() #extrinsic alpha = .84

df %>% 
  select(motivation_4:motivation_6) %>% 
  alpha() #identified alpha = .94

df %>% 
  select(motivation_7:motivation_9) %>% 
  alpha() #intrinsic alpha = .82

hist(df$mot_ex)
hist(df$mot_id)
hist(df$mot_in)

# INFERENTIAL
aov_ex <- aov(mot_ex ~ condition, data = df)
aov_id <- aov(mot_id ~ condition + gender, data = df)
aov_in <- aov(mot_in ~ condition, data = df)
summary(aov_ex)
summary(aov_id)
summary(aov_in)

# there are no significant differences in motivation between the conditions - onboarding does not work to set apart these motivational frames

# check random variables
ex_soc <- aov(mot_ex ~ gender + age + employ, data = df) # younger people have a bit more extrinsic motivation, but the effect is very small
id_soc <- aov(mot_id ~ gender + age + employ, data = df)
in_soc <- aov(mot_in ~ gender + age + employ, data = df)
summary(ex_soc) # age is significant predictor for extrinsic motivation
summary(id_soc) # gender is significant predictor for identified motivation, may be slightly lower for men than women
summary(in_soc) # employment status is significant predictor?
coef(in_soc)

ggplot(df, aes(x = age, y = mot_ex)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df, aes(x = gender, y = mot_id)) +
  geom_boxplot()
