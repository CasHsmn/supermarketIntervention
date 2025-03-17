library(data.table)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)

wd <- list()
wd$data   <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/output/"

raw <- fread(paste0(wd$data, "PERSpilot_prelim_20250314.csv"))

colnames(df)

df <- raw %>% 
  filter(Status == 0 & Finished == 1 & consent == 1 & PID != 0) %>% 
  select(13:99) %>% 
  mutate(across(-c(18,25,28,33,36,42,45,48,54,57,60,65,68,75,78,81,86), as.numeric))  


df %>%
  select(1:11) %>%
  tbl_summary(
    type = list(where(is.numeric) ~ "continuous2"),
    statistic = list(all_continuous() ~ "{mean}  ({sd})"),
    label = list(food_freq_1 = "bread",
                 food_freq_2 = "potato",
                 food_freq_3 = "lettuce",
                 food_freq_4 = "cucumber",
                 food_freq_5 = "banana",
                 food_freq_6 = "apple",
                 food_freq_7 = "berries",
                 food_freq_8 = "herbs",
                 food_freq_9 = "bell pepper",
                 food_freq_10 = "courgette",
                 food_freq_11 = "broccoli")
  )

df %>%
  select(br_1_1:br_1_6) %>%
  summarise(across(everything(), list(Count = ~ sum(. == 1, na.rm = TRUE),
                                      Percent = ~ mean(. == 1, na.rm = TRUE) * 100)))

# ChatGPT pie-chart
summary_data <- df %>%
  select(br_1_1:br_1_6) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Option", values_to = "Count")


ggplot(summary_data, aes(x = "", y = Count, fill = Option)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Multiple Choice Selections (Count)") +
  theme(axis.text.x = element_blank()) +
  theme_minimal()
