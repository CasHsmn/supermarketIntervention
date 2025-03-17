library(data.table)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)

wd <- list()
wd$data   <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/data/"
wd$output <- "C:/Users/huism080/OneDrive - Wageningen University & Research/Research/Study-3/analysis/output/"

raw <- fread(paste0(wd$data, "PERSpilot_final_20250317.csv"))

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

food_freq_means <- df %>%
  summarise(across(1:11, \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "food", values_to = "mean")

food_labels <- c("food_freq_1" = "bread",
                 "food_freq_2" = "potato",
                 "food_freq_3" = "lettuce",
                 "food_freq_4" = "cucumber",
                 "food_freq_5" = "banana",
                 "food_freq_6" = "apple",
                 "food_freq_7" = "berries",
                 "food_freq_8" = "herbs",
                 "food_freq_9" = "bell pepper",
                 "food_freq_10" = "courgette",
                 "food_freq_11" = "broccoli")

food_freq_means$food <- food_labels[food_freq_means$food]

# Plot bar figure
ggplot(food_freq_means, aes(x = reorder(food, mean), y = mean, fill = food)) +
  geom_bar(stat = "identity") +
  labs(x = "Food", y = "Mean Frequency", title = "Average Food Frequency") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


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

# Calculate means and standard deviations for each food item
food_freq_summary <- df %>%
  select(1:11) %>%
  pivot_longer(cols = everything(), names_to = "food", values_to = "freq") %>%
  group_by(food) %>%
  summarise(
    mean = mean(freq, na.rm = TRUE),
    sd = sd(freq, na.rm = TRUE)
  )

# Rename variables for labeling
food_labels <- c("food_freq_1" = "bread",
                 "food_freq_2" = "potato",
                 "food_freq_3" = "lettuce",
                 "food_freq_4" = "cucumber",
                 "food_freq_5" = "banana",
                 "food_freq_6" = "apple",
                 "food_freq_7" = "berries",
                 "food_freq_8" = "herbs",
                 "food_freq_9" = "bell pepper",
                 "food_freq_10" = "courgette",
                 "food_freq_11" = "broccoli")

# Apply labels
food_freq_summary$food <- food_labels[food_freq_summary$food]

# Plot bar graph with error bars
ggplot(food_freq_summary, aes(x = reorder(food, mean), y = mean, fill = food)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(x = "Food type", y = "Mean Frequency", title = "Average Food Frequency with Variability") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

frequency_levels <- c("Never", 
                      "Less than once a month", 
                      "Once a month", 
                      "2-3 times a month", 
                      "Once a week", 
                      "Multiple times a week")

# Prepare the data for plotting
food_data <- df %>%
  select(1:11) %>%
  pivot_longer(cols = everything(), names_to = "Food", values_to = "Frequency") %>%
  mutate(Frequency = factor(Frequency, levels = 1:6, labels = frequency_levels))

# Rename variables for labeling
food_labels <- c("food_freq_1" = "bread",
                 "food_freq_2" = "potato",
                 "food_freq_3" = "lettuce",
                 "food_freq_4" = "cucumber",
                 "food_freq_5" = "banana",
                 "food_freq_6" = "apple",
                 "food_freq_7" = "berries",
                 "food_freq_8" = "herbs",
                 "food_freq_9" = "bell pepper",
                 "food_freq_10" = "courgette",
                 "food_freq_11" = "broccoli")

# Apply labels
food_data$Food <- food_labels[food_data$Food]

# Plot proportional bar graph
ggplot(food_data, aes(x = Food, fill = Frequency)) +
  geom_bar(position = "fill") +  # Proportional stacked bars
  scale_y_continuous(labels = scales::percent) +  # Show percentages on y-axis
  labs(x = "Food", y = "Proportion", fill = "Frequency",
       title = "Proportional Distribution of Consumption Frequency for Each Food Item") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

colnames(df)

loc_labels <- c("Fridge", "Freezer", "Pantry/cupboard", 
            "Kitchen Counter", "Cellar/Garage", "Other")

# bread
bread_long <- df %>%
  select(starts_with("br_1_") & !ends_with("TEXT")) %>% 
  pivot_longer(cols = everything(), names_to = "StorageOption", values_to = "Selected") %>%
  group_by(StorageOption) %>%
  summarise(Count = sum(Selected, na.rm = T)) %>%
  mutate(Proportion = (Count / sum(Count, na.rm = T))*100,  # Proportions of each storage option
         StorageOption = factor(StorageOption, levels = paste0("br_1_", 1:6), labels = loc_labels))

ggplot(bread_long, aes(x = StorageOption, y = Proportion, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for Bread", fill = "Storage Option") +
  theme_minimal() + 
  theme(legend.position = "right")

# broccoli
bro_summary <- df %>%
  filter(!is.na(bro_1)) %>% 
  count(bro_1) %>%
  mutate(StorageOption = factor(bro_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(bro_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for Broccoli", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# potatoes
pot_summary <- df %>%
  filter(!is.na(pot_1)) %>% 
  count(pot_1) %>%
  mutate(StorageOption = factor(pot_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(pot_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for potato", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# lettuce
let_summary <- df %>%
  filter(!is.na(let_1)) %>% 
  count(let_1) %>%
  mutate(StorageOption = factor(let_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(let_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for lettuce", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# cucumber
cu_summary <- df %>%
  filter(!is.na(cu_1)) %>% 
  count(cu_1) %>%
  mutate(StorageOption = factor(cu_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(cu_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for cucumber", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# banana
ba_summary <- df %>%
  filter(!is.na(ba_1)) %>% 
  count(ba_1) %>%
  mutate(StorageOption = factor(ba_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(ba_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for banana", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# apple
ap_summary <- df %>%
  filter(!is.na(ap_1)) %>% 
  count(ap_1) %>%
  mutate(StorageOption = factor(ap_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(ap_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for apple", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# berries
be_summary <- df %>%
  filter(!is.na(be_1)) %>% 
  count(be_1) %>%
  mutate(StorageOption = factor(be_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(be_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for berries", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# herbs
he_summary <- df %>%
  filter(!is.na(he_1)) %>% 
  count(he_1) %>%
  mutate(StorageOption = factor(he_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(he_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for herbs", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# bell pepper
co_summary <- df %>%
  filter(!is.na(co_1)) %>% 
  count(co_1) %>%
  mutate(StorageOption = factor(co_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(co_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for courgette", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# courgette
pep_summary <- df %>%
  filter(!is.na(pep_1)) %>% 
  count(pep_1) %>%
  mutate(StorageOption = factor(pep_1, levels = 1:6, labels = loc_labels)) %>%
  select(StorageOption, n)

ggplot(pep_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
  geom_bar(stat = "identity") +
  labs(title = "Storage Locations for bell pepper", x = "Storage Option", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")


# chatGPT function
# Define a function to handle the summarization and plotting
plot_storage <- function(df, food_code, food_name, food_freq_col) {
  # Generate the corresponding food storage column dynamically (e.g., br_1, pot_1)
  food_col <- paste0(food_code, "_1")
  
  # Filter the data: Only include rows where the corresponding food_freq column is not 1
  food_summary <- df %>%
    filter(!is.na(!!sym(food_col)) & !!sym(food_freq_col) != 1) %>%  # Filter out rows where food_freq is 1
    count(!!sym(food_col)) %>%
    mutate(StorageOption = factor(!!sym(food_col), levels = 1:6, labels = loc_labels)) %>%
    select(StorageOption, n)
  
  # Plot the bar plot
  ggplot(food_summary, aes(x = StorageOption, y = n, fill = StorageOption)) + 
    geom_bar(stat = "identity") +
    labs(title = paste("Storage Locations for", food_name), x = "Storage Option", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Define food labels and frequency columns
food_labels <- c("food_freq_1" = "bread",
                 "food_freq_2" = "potato",
                 "food_freq_3" = "lettuce",
                 "food_freq_4" = "cucumber",
                 "food_freq_5" = "banana",
                 "food_freq_6" = "apple",
                 "food_freq_7" = "berries",
                 "food_freq_8" = "herbs",
                 "food_freq_9" = "bell pepper",
                 "food_freq_10" = "courgette",
                 "food_freq_11" = "broccoli")

# Define the list of food items with their corresponding food_freq column
food_list <- list(
  #list(food_code = "br", food_name = "bread", food_freq_col = "food_freq_1"),
  list(food_code = "pot", food_name = "potato", food_freq_col = "food_freq_2"),
  list(food_code = "let", food_name = "lettuce", food_freq_col = "food_freq_3"),
  list(food_code = "cu", food_name = "cucumber", food_freq_col = "food_freq_4"),
  list(food_code = "ba", food_name = "banana", food_freq_col = "food_freq_5"),
  list(food_code = "ap", food_name = "apple", food_freq_col = "food_freq_6"),
  list(food_code = "be", food_name = "berries", food_freq_col = "food_freq_7"),
  list(food_code = "he", food_name = "herbs", food_freq_col = "food_freq_8"),
  list(food_code = "co", food_name = "bell pepper", food_freq_col = "food_freq_9"),
  list(food_code = "pep", food_name = "courgette", food_freq_col = "food_freq_10"),
  list(food_code = "bro", food_name = "broccoli", food_freq_col = "food_freq_11")
)

# Apply the function to each food type in the list
lapply(food_list, function(x) plot_storage(df, x$food_code, x$food_name, x$food_freq_col))

# boxplots
# Columns to plot
discard_item <- c("br_3", "pot_2", "let_3", "cu_2", "ba_3", "ap_2", "be_3", "he_3", "pep_2", "co_2", "bro_3")

# Gather the data for the selected columns
data_for_plot <- df %>%
  select(all_of(discard_item)) %>%
  gather(key = "FoodType", value = "Rating")

data_for_plot$FoodType <- factor(data_for_plot$FoodType, 
                                 levels = discard_item, 
                                 labels = food_labels)

# Calculate means and standard deviations for each group
mean_sd_data <- data_for_plot %>%
  group_by(FoodType) %>%
  summarise(
    Mean = mean(Rating, na.rm = TRUE),
    SD = sd(Rating, na.rm = TRUE),
    N = n()
  ) %>%
  mutate(SE = SD / sqrt(N))  # Standard Error (for error bars)

# Plot the means with error bars
ggplot(mean_sd_data, aes(x = FoodType, y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean frequency of food going bad",
       x = "Food Types",
       y = "Mean Rating (1-7)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# code correct location
df <- df %>% 
  mutate(pot_loc = recode_factor(pot_1, `3` = "yes", `5` = "yes", .default = "no"))

