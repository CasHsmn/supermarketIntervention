library(data.table)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)
library(careless)
library(tibble)
library(psych)
library(kableExtra)
library(corrplot)

wd <- list()
wd$data   <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/output/"

raw <- fread(file = paste0(wd$data, "motivationtype_20250818.csv"))

# check attention check 
failed_check <- raw %>% # none failed attention check, 19 never walk or cycle short distances and did not see the question
  filter(beh_walk_mot_5 != 1) %>% 
  select(PID, beh_walk_mot_5)

# check flatlining, if any of the mot questions are flatlined. If all 7/8, remove response
ls_rec <- raw %>% 
  select(starts_with("beh_rec_mot")) %>% 
  longstring() %>% 
  as.data.frame() %>% 
  rowid_to_column()
which(ls$. == 9)
# 90, 102, 108, 133, 180
# 102, 108, *180* all 7
raw[180, beh_rec_mot_1:beh_rec_mot_9]

ls_walk <- raw %>% 
  select(starts_with("beh_walk_mot"), -beh_walk_mot_5) %>% 
  longstring() %>% 
  as.data.frame() %>% 
  rowid_to_column()
which(ls_walk$. == 9)
# 3   6  11  33  43  52  64  90 111 112 127 135 137 150 151 177 *180* 200
# only 180, all others did not answer this question
raw %>%
  slice(200) %>%
  select(starts_with("beh_walk_mot"), -beh_walk_mot_5)

ls_fw <- raw %>% 
  select(starts_with("beh_fw_mot")) %>% 
  longstring() %>% 
  as.data.frame() %>% 
  rowid_to_column()
which(ls_fw$. == 9)
# 7  15  77  82  98 136 155 170 180 185
raw[185, beh_fw_mot_1:beh_fw_mot_9]
raw[180, PID]
# remove 102, 108, 180
raw[108]

# create clean dataset
df <- raw %>% 
  filter(Status == 0 & Finished == 1 & consent == 1 & PID != 0, beh_walk_mot_5 == 1) %>% 
  select(12:ncol(.)) %>% 
  mutate(across(-PID, as.numeric)) %>% 
  slice(-180) %>% 
  rownames_to_column(var = "id")

# structure as theorised, maybe revisit as the structure does not hold up very well
df <- df %>% 
  mutate(
    beh_walk_hs = (beh_walk_habit_1 + beh_walk_habit_2 + beh_walk_habit_3 + beh_walk_habit_4)/4,
    beh_walk_mot_ex = (beh_walk_mot_1 + beh_walk_mot_2 + beh_walk_mot_3)/3,
    beh_walk_mot_id = (beh_walk_mot_6 + beh_walk_mot_7)/2,
    beh_walk_mot_int = (beh_walk_mot_8 + beh_walk_mot_9 + beh_walk_mot_10)/3,
    beh_rec_hs = (beh_rec_habit_1 + beh_rec_habit_2 + beh_rec_habit_3 + beh_rec_habit_4)/4,
    beh_rec_mot_ex = (beh_rec_mot_1 + beh_rec_mot_2 + beh_rec_mot_3)/3,
    beh_rec_mot_id = (beh_rec_mot_5 + beh_rec_mot_6)/2,
    beh_rec_mot_int = (beh_rec_mot_7 + beh_rec_mot_8 + beh_rec_mot_9)/3,
    beh_fw_hs = (beh_fw_habit_1 + beh_fw_habit_2 + beh_fw_habit_3 + beh_fw_habit_4)/4,
    beh_fw_mot_ex = (beh_fw_mot_1 + beh_fw_mot_2 + beh_fw_mot_3)/3,
    beh_fw_mot_id = (beh_fw_mot_5 + beh_fw_mot_6)/2,
    beh_fw_mot_int = (beh_fw_mot_7 + beh_fw_mot_8 + beh_fw_mot_9)/3,
    # combine all behaviours
    beh_mot_1 = (beh_walk_mot_1 + beh_fw_mot_1 + beh_rec_mot_1) / 3,
    beh_mot_2 = (beh_walk_mot_2 + beh_fw_mot_2 + beh_rec_mot_2) / 3,
    beh_mot_3 = (beh_walk_mot_3 + beh_fw_mot_3 + beh_rec_mot_3) / 3,
    beh_mot_4 = (beh_walk_mot_4 + beh_fw_mot_4 + beh_rec_mot_4) / 3,
    beh_mot_5 = (beh_walk_mot_6 + beh_fw_mot_5 + beh_rec_mot_5) / 3,
    beh_mot_6 = (beh_walk_mot_7 + beh_fw_mot_6 + beh_rec_mot_6) / 3,
    beh_mot_7 = (beh_walk_mot_8 + beh_fw_mot_7 + beh_rec_mot_7) / 3,
    beh_mot_8 = (beh_walk_mot_9 + beh_fw_mot_8 + beh_rec_mot_8) / 3,
    beh_mot_9 = (beh_walk_mot_10 + beh_fw_mot_9 + beh_rec_mot_9) / 3,
    beh_hs = (beh_walk_hs + beh_rec_hs + beh_fw_hs)/3,
    beh_mot_ex = (beh_mot_1 + beh_mot_2 + beh_mot_3)/3,
    beh_mot_id = (beh_mot_5 + beh_mot_6)/2, # removed item 4 as it has poorest fit
    beh_mot_int = (beh_mot_7 + beh_mot_8 + beh_mot_9)/3
  )

# check factor stucture of motivation construct
# for recycling, beh_rec_mot_4 does not load on anything cleanly
df %>% 
  select(starts_with("beh_rec_mot")) %>% 
  fa(nfactors = 3)

df %>% 
  select(beh_rec_mot_1:beh_rec_mot_9) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()

# for walking/cycling, mostly fine aside aside from item 4
df %>% 
  select(starts_with("beh_walk_mot"), -beh_walk_mot_5) %>% 
  fa(nfactors = 3)

df %>% 
  select(beh_walk_mot_1:beh_walk_mot_10, -beh_walk_mot_5) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()

# for food waste, the factor structure does not return neatly and identified and intrinsic go together
df %>% 
  select(starts_with("beh_fw_mot")) %>% 
  fa(nfactors = 3)

df %>% 
  select(beh_fw_mot_1:beh_fw_mot_9) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()

# item "it makes sense to me" fits better with intrinsic than identified
# the three factor structure isn't very strong. Intrinsic and identified are not well separated but correlate strongly
# item 5 "it reflects the kind of person I want to be", distinguished most strongly
# check if the results would be very different if I used only items 1, 5 and 8

# with all behaviours cominbed, the factor stucture is OK, just beh_mot_4 does not fit
df %>%
  select(starts_with("beh_mot"), -beh_mot_ex, -beh_mot_id, -beh_mot_int) %>%
  fa(nfactors = 4)
print.psych(fa_mot, sort = T)
fa_mot$loadings

df %>%
  select(starts_with("beh_mot"), -beh_mot_ex, -beh_mot_id, -beh_mot_int) %>%
  cor(use = "pairwise.complete.obs") %>% 
  corrplot()

# -----

##### ANALYSIS #####
# descriptives
# overall mean motivation scores
df_long <- df %>% 
  select(id, beh_mot_ex, beh_mot_id, beh_mot_int) %>% 
  pivot_longer(cols = starts_with("beh_mot"), 
               names_to = "mot_type", values_to = "mot_score") %>%
  mutate(
    mot_type = recode(
      mot_type,
      beh_mot_ex = "Extrinsic",
      beh_mot_id = "Identified",
      beh_mot_int = "Intrinsic"
    )
  )

ggplot(df_long, aes(x = mot_type, y = mot_score, fill = mot_type)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7) +
  theme_minimal() +
  labs(
    title = "Mean Motivation Scores by Type",
    x = "Motivation Type",
    y = "Mean Score"
  ) +
  scale_fill_brewer(palette = "Pastel1")

motivation_cols <- grep("^beh_(walk|rec|fw)_mot_(ex|id|int)$", names(df), value = TRUE)

df_long_new <- df %>%
  select(id, all_of(motivation_cols)) %>%
  pivot_longer(
    cols = -id,
    names_to = c("behaviour_type", "motivation_type"),
    names_pattern = "beh_(walk|rec|fw)_mot_(ex|id|int)", 
    values_to = "motivation_score"
  )
df_long_new_labeled <- df_long_new %>%
  mutate(
    behaviour_type = recode(
      behaviour_type,
      fw = "Food Waste",
      rec = "Recycling",
      walk = "Walk/Cycle"
    ),
    motivation_type = recode(
      motivation_type,
      ex = "Extrinsic",
      id = "Identified",
      int = "Intrinsic"
    )
  )
ggplot(df_long_new_labeled, aes(x = behaviour_type, y = motivation_score, fill = motivation_type)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "Mean Motivation Scores by Behaviour and Motivation Type",
    x = "Motivation Type",
    y = "Mean Score",
    fill = "Behaviour"
  ) +
  scale_fill_brewer(palette = "Pastel1")

# plot habit strength means
habit_long <- df %>%
  select(id, beh_rec_hs, beh_walk_hs, beh_fw_hs) %>%
  pivot_longer(
    cols = c(beh_rec_hs, beh_walk_hs, beh_fw_hs),
    names_to = "behaviour_type",
    values_to = "habit_strength"
  ) %>% 
  mutate(
    behaviour_type = recode(behaviour_type,
                            beh_fw_hs = "Food waste",
                            beh_rec_hs = "Recycling",
                            beh_walk_hs = "Walk/cycle")
  )

ggplot(habit_long, aes(x = behaviour_type, y = habit_strength, fill = behaviour_type)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

habit_long %>%
  group_by(behaviour_type) %>%
  summarise(
    mean_habit_strength = mean(habit_strength, na.rm = TRUE),
    sd_habit_strength = sd(habit_strength, na.rm = TRUE),
    n = n()
  )
# habit strength for each behaviour is quite high. highest for recycling at 5.92 (1.20), followed by food waste at 5.17 (1.72) 
# and walking or cycling (4.17, 1.84)

# expectation from visual inspection: both identified and intrinsic are significantly higher than external for all behaviours, 
# but there is no signficant difference between identified and intrinsic


freq_long <- df %>%
  select(beh_freq_1, beh_freq_2, beh_freq_3) %>%
  pivot_longer(
    cols = everything(),
    names_to = "behaviour_type",
    values_to = "frequency"
  ) %>%
  mutate(
    behaviour_type = recode(
      behaviour_type,
      beh_freq_2 = "Walking",
      beh_freq_1 = "Recycling",
      beh_freq_3 = "Food waste"
    )
  )

# Summary statistics for each behaviour
freq_summary <- freq_long %>%
  group_by(behaviour_type) %>%
  summarise(
    mean_frequency = mean(frequency, na.rm = TRUE),
    sd_frequency = sd(frequency, na.rm = TRUE),
    min_frequency = min(frequency, na.rm = TRUE),
    max_frequency = max(frequency, na.rm = TRUE),
    n = sum(!is.na(frequency))
  )

ggplot(freq_summary, aes(x = behaviour_type, y = mean_frequency, fill = behaviour_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_frequency - sd_frequency, ymax = mean_frequency + sd_frequency), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Mean Frequency by Behaviour (with SD error bars)",
    x = "Behaviour Type",
    y = "Mean Frequency"
  ) +
  scale_fill_brewer(palette = "Pastel1")
ggsave(paste0(wd$output, "frequency_summary.png"))

hist(df$beh_hs)

# median split habit check
str(df$beh_hs)
df_habit_split <- df %>%
  mutate(
    habit_group = if_else(beh_hs <= median(beh_hs, na.rm = TRUE), "Low Habit", "High Habit")
  )
median(df$beh_hs, na.rm = T)

df_long_habit <- df_long_new %>%
  left_join(
    df_habit_split %>% select(id, habit_group),
    by = "id"
  )

ggplot(df_long_habit, aes(x = motivation_type, y = motivation_score, fill = habit_group)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.7, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "Mean Motivation Scores by Habit Group (Combined Behaviours)",
    x = "Motivation Type",
    y = "Mean Motivation Score",
    fill = "Habit Strength Group"
  ) +
  scale_fill_brewer(palette = "Pastel1")

# INFERENTIAL
library(lme4)
library(car)
library(lmerTest)
library(ez)
library(emmeans)
model <- lmer(mot_score ~ mot_type + (1|id), data = df_long)
Anova(model)
summary(model)

emm <- emmeans(model, ~ mot_type)
pairs(emm, adjust = "bonferroni")

lm_hsbymot <- lm(beh_hs ~ beh_mot_ex + beh_mot_id + beh_mot_int, data  = df)
summary(lm_hsbymot)
head(df, 10)


library(ggplot2)
library(gridExtra)

# Create individual scatter plots for each predictor
ggplot(df, aes(x = beh_mot_ex, y = beh_hs)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(df, aes(x = beh_mot_id, y = beh_hs)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(df, aes(x = beh_mot_int, y = beh_hs)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

plot(lm_hsbymot)
# Display all plots together
grid.arrange(p1, p2, p3, ncol = 2)
cor(df$beh_mot_id, df$beh_mot_int, use = "pairwise.complete.obs")
