library(data.table)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(tidyr)
library(purrr)
library(patchwork)

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
df <- df %>% mutate(
  br_loc = case_when(
    is.na(br_1_1) & is.na(br_1_2) & is.na(br_1_3) & is.na(br_1_4) & is.na(br_1_5) ~ NA_character_,
    br_1_1 == 1 ~ "wrong", 
    br_1_2 == 1 ~ "correct", 
    br_1_3 == 1 ~ "correct", 
    br_1_4 == 1 ~ "correct", 
    br_1_5 == 1 ~ "correct", 
    TRUE ~ "wrong"
  ),
  pot_loc = case_when(
    is.na(pot_1) ~ NA_character_,
    pot_1 %in% c(3, 5) ~ "correct",
    TRUE ~ "wrong"
  ),
  bro_loc = case_when(
    is.na(bro_1) ~ NA_character_,
    bro_1 %in% c(1, 2) ~ "correct",
    TRUE ~ "wrong"
  ),
  let_loc = case_when(
    is.na(let_1) ~ NA_character_,
    let_1 == 1 ~ "correct",
    TRUE ~ "wrong"
  ),
  cu_loc = case_when(
    is.na(cu_1) ~ NA_character_,
    cu_1 %in% c(3, 4, 5) ~ "correct",
    TRUE ~ "wrong"
  ),
  ba_loc = case_when(
    is.na(ba_1) ~ NA_character_,
    ba_1 %in% c(2, 3, 4, 5) ~ "correct",
    TRUE ~ "wrong"
  ),
  ap_loc = case_when(
    is.na(ap_1) ~ NA_character_,
    ap_1 %in% c(1, 2) ~ "correct",
    TRUE ~ "okay"
  ),
  be_loc = case_when(
    is.na(be_1) ~ NA_character_,
    be_1 %in% c(1, 2) ~ "correct",
    TRUE ~ "wrong"
  ),
  he_loc = case_when(
    is.na(he_1) ~ NA_character_,
    he_1 %in% c(1, 2) ~ "correct",
    TRUE ~ "wrong"
  ),
  pep_loc = case_when(
    is.na(pep_1) ~ NA_character_,
    pep_1 %in% c(2, 3, 4, 5) ~ "correct",
    TRUE ~ "wrong"
  ),
  co_loc = case_when(
    is.na(co_1) ~ NA_character_,
    co_1 %in% c(2, 3, 4, 5) ~ "correct",
    TRUE ~ "wrong"
  )
)

df <- df %>%
  mutate(across(88:98, ~ factor(., levels = c("wrong", "correct")))) # 1 = wrong; 2 = correct


# Prepare the data
loc_summary <- df %>%
  select(88:98) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  group_by(Question, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(
    df %>%
      select(88:98) %>%
      pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
      filter(!is.na(Response)) %>%
      group_by(Question) %>%
      summarise(Total = n(), .groups = "drop"),
    by = "Question"
  ) %>%
  mutate(Percent = (Count / Total) * 100)

# Map food codes to food names
loc_summary <- loc_summary %>%
  mutate(
    Product = recode(Question,
                     `br_loc` = "bread",
                     `pot_loc` = "potato",
                     `let_loc` = "lettuce",
                     `cu_loc` = "cucumber",
                     `ba_loc` = "banana",
                     `ap_loc` = "apple",
                     `be_loc` = "berries",
                     `he_loc` = "herbs",
                     `co_loc` = "bell pepper",
                     `pep_loc` = "courgette",
                     `bro_loc` = "broccoli"
    )
  )

# Plot
ggplot(loc_summary, aes(x = Product, y = Percent, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("wrong" = "#8c1212", "correct" = "#128c37")) +
  coord_flip() +
  labs(
    title = "Percentage of Correct storage",
    x = "Food product",
    y = "Percentage"
  ) +
  theme_minimal()


# CLAUDE STACKED BAR STORAGE LOCATIONS

# Define location labels
loc_labels <- c("Refrigerator", "Freezer", "Pantry/Cupboard", "Counter", "Cellar", "Other")

# Create a function to process data for regular food items
process_food_data <- function(df, food_code, food_name, food_freq_col) {
  # Generate the corresponding food storage column dynamically
  food_col <- paste0(food_code, "_1")
  
  # Filter and prepare the data
  food_summary <- df %>%
    filter(!is.na(!!sym(food_col)) & !!sym(food_freq_col) != 1) %>%
    count(!!sym(food_col)) %>%
    mutate(
      StorageOption = factor(!!sym(food_col), levels = 1:6, labels = loc_labels),
      Food = food_name,
      Proportion = (n / sum(n)) * 100
    ) %>%
    select(Food, StorageOption, Proportion)
  
  return(food_summary)
}

# Process bread data (special case with multiple columns)
bread_summary <- df %>%
  select(starts_with("br_1_") & !ends_with("TEXT")) %>% 
  pivot_longer(cols = everything(), names_to = "StorageOption", values_to = "Selected") %>%
  group_by(StorageOption) %>%
  summarise(Count = sum(Selected, na.rm = TRUE)) %>%
  filter(Count > 0) %>%
  mutate(
    Proportion = (Count / sum(Count)) * 100,
    StorageOption = factor(StorageOption, 
                           levels = paste0("br_1_", 1:6), 
                           labels = loc_labels),
    Food = "Bread"
  ) %>%
  select(Food, StorageOption, Proportion)

# Define the list of food items with their corresponding food_freq column
food_list <- list(
  list(food_code = "pot", food_name = "Potato", food_freq_col = "food_freq_2"),
  list(food_code = "let", food_name = "Lettuce", food_freq_col = "food_freq_3"),
  list(food_code = "cu", food_name = "Cucumber", food_freq_col = "food_freq_4"),
  list(food_code = "ba", food_name = "Banana", food_freq_col = "food_freq_5"),
  list(food_code = "ap", food_name = "Apple", food_freq_col = "food_freq_6"),
  list(food_code = "be", food_name = "Berries", food_freq_col = "food_freq_7"),
  list(food_code = "he", food_name = "Herbs", food_freq_col = "food_freq_8"),
  list(food_code = "co", food_name = "Bell Pepper", food_freq_col = "food_freq_9"),
  list(food_code = "pep", food_name = "Courgette", food_freq_col = "food_freq_10"),
  list(food_code = "bro", food_name = "Broccoli", food_freq_col = "food_freq_11")
)

# Process all regular food items
all_food_data <- map_dfr(food_list, ~process_food_data(df, .$food_code, .$food_name, .$food_freq_col))

# Combine bread data with other food data
combined_data <- bind_rows(bread_summary, all_food_data)

# Convert Food to a factor with specific order to control the order in the plot
combined_data$Food <- factor(combined_data$Food, 
                             levels = c("Bread", "Potato", "Lettuce", "Cucumber", "Banana", 
                                        "Apple", "Berries", "Herbs", "Bell Pepper", "Courgette", "Broccoli"))

# Create a stacked bar chart
ggplot(combined_data, aes(x = Food, y = Proportion, fill = StorageOption)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Food Storage Locations", 
       x = "Food Item", 
       y = "Percentage", 
       fill = "Storage Location") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  coord_flip()

# Create a faceted version with each food as a separate panel
# This provides an alternative view if needed
ggplot(combined_data, aes(x = StorageOption, y = Proportion, fill = StorageOption)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Food, ncol = 4) +
  labs(title = "Storage Locations by Food Type", 
       x = "Storage Location", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold")
  ) +
  coord_flip()

# STORAGE PACKAGING
df$br

# Calculate the total valid responses - those who answered food_freq_1 with anything but 1
br_responses <- sum(df$food_freq_1 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for bread storage
bread_storage_data <- df %>%
  select(starts_with("br_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
br_frequencies <- sapply(bread_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
br_plot <- data.frame(
  storage_method = c("Loose", "Airtight packaging", "Allowing airflow", "Other"),
  frequency = br_frequencies,
  percentage = (br_frequencies / br_responses) * 100
)

# Create a bar plot
ggplot(br_plot, aes(x = storage_method, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "Bread Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", br_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# broccoli
bro_responses <- sum(df$food_freq_11 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for broead storage
broc_storage_data <- df %>%
  select(starts_with("bro_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
bro_frequencies <- sapply(broc_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
bro_plot <- data.frame(
  storage_method = c("Loose", "Airtight packaging", "Allowing airflow", "Other"),
  frequency = bro_frequencies,
  percentage = (bro_frequencies / bro_responses) * 100
)

# Create a bar plot
ggplot(bro_plot, aes(x = storage_method, y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "broccoli Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", bro_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# lettuce
let_responses <- sum(df$food_freq_3 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for letead storage
let_storage_data <- df %>%
  select(starts_with("let_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
let_frequencies <- sapply(let_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
let_plot <- data.frame(
  storage_method = c("Loose", "Airtight packaging", "Allowing airflow", "Stalk in water", "Other"),
  frequency = let_frequencies,
  percentage = (let_frequencies / let_responses) * 100
)

# Create a bar plot
ggplot(let_plot, aes(x = storage_method, y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "lettuce Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", let_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# banana
ba_responses <- sum(df$food_freq_5 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for baead storage
ba_storage_data <- df %>%
  select(starts_with("ba_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
ba_frequencies <- sapply(ba_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
ba_plot <- data.frame(
  storage_method = c("Loose", "Airtight packaging", "Allowing airflow", "Top wrapped up", "Other"),
  frequency = ba_frequencies,
  percentage = (ba_frequencies / ba_responses) * 100
)

# Create a bar plot
ggplot(ba_plot, aes(x = storage_method, y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "banana Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", ba_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# berries
be_responses <- sum(df$food_freq_7 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for beead storage
be_storage_data <- df %>%
  select(starts_with("be_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
be_frequencies <- sapply(be_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
be_plot <- data.frame(
  storage_method = c("Airtight packaging", "Allowing airflow", "Washed", "Other"),
  frequency = be_frequencies,
  percentage = (be_frequencies / be_responses) * 100
)

# Create a bar plot
ggplot(be_plot, aes(x = storage_method, y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "berries Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", be_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# herbs
he_responses <- sum(df$food_freq_8 != 1, na.rm = TRUE)

# Use your existing dataframe 'df' and column selection for heead storage
he_storage_data <- df %>%
  select(starts_with("he_2")) %>%
  select(-ends_with("TEXT"))

# Calculate the frequency of each option (counting the number of 1s in each column)
he_frequencies <- sapply(he_storage_data, function(x) sum(!is.na(x)))

# Create a data frame for plotting with proper labels
he_plot <- data.frame(
  storage_method = c("Loose", "Airtight packaging", "Allowing airflow", "In a glass of water", "Wrapped in moist towel", "Other"),
  frequency = he_frequencies,
  percentage = (he_frequencies / he_responses) * 100
)

# Create a bar plot
ggplot(he_plot, aes(x = storage_method, y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            vjust = -0.5, 
            color = "black") +
  labs(
    title = "herbs Storage Methods",
    subtitle = paste("Base: Respondents who answered food_freq_1 ≠ 1,", "n =", he_responses),
    x = "Storage Method",
    y = "Frequency",
    caption = "Note: Respondents could select multiple options"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# recode storage packaging
df <- df %>% mutate(
  br_pack = case_when(
    is.na(br_2_1) & is.na(br_2_2) & is.na(br_2_3) & is.na(br_2_4) ~ NA_character_,
    br_2_1 == 1 ~ "wrong", 
    br_2_2 == 1 ~ "wrong", 
    br_2_3 == 1 ~ "correct", 
    br_2_4 == 1 ~ "wrong", 
    TRUE ~ "wrong"
  ),
  bro_pack = case_when(
    is.na(bro_2_1) & is.na(bro_2_2) & is.na(bro_2_3) & is.na(bro_2_4) ~ NA_character_,
    br_2_1 == 1 ~ "wrong", 
    br_2_2 == 1 ~ "wrong", 
    br_2_3 == 1 ~ "correct", 
    br_2_4 == 1 ~ "wrong", 
    TRUE ~ "wrong"
  ),
  let_pack = case_when(
    is.na(let_2_1) & is.na(let_2_2) & is.na(let_2_3) & is.na(let_2_4) & is.na(let_2_5) ~ NA_character_,
    let_2_1 == 1 ~ "wrong", 
    let_2_2 == 1 ~ "wrong", 
    let_2_3 == 1 ~ "correct", 
    let_2_4 == 1 ~ "correct", 
    let_2_5 == 1 ~ "wrong",
    TRUE ~ "wrong"
  ),
  ba_pack = case_when(
    is.na(ba_2_1) & is.na(ba_2_2) & is.na(ba_2_3) & is.na(ba_2_4) & is.na(ba_2_5) ~ NA_character_,
    ba_2_1 == 1 ~ "correct", 
    ba_2_2 == 1 ~ "wrong", 
    ba_2_3 == 1 ~ "correct", 
    ba_2_4 == 1 ~ "correct", 
    ba_2_5 == 1 ~ "wrong",
    TRUE ~ "wrong"
  ),
  be_pack = case_when(
    is.na(be_2_1) & is.na(be_2_2) & is.na(be_2_3) & is.na(be_2_4) ~ NA_character_,
    be_2_1 == 1 ~ "wrong", 
    be_2_2 == 1 ~ "correct", 
    be_2_3 == 1 ~ "wrong", 
    be_2_4 == 1 ~ "wrong", 
    TRUE ~ "wrong"
  ),
  he_pack = case_when(
    is.na(he_2_1) & is.na(he_2_2) & is.na(he_2_3) & is.na(he_2_4) & is.na(he_2_5) & is.na(he_2_6) ~ NA_character_,
    he_2_1 == 1 ~ "wrong", 
    he_2_2 == 1 ~ "correct", 
    he_2_3 == 1 ~ "correct", 
    he_2_4 == 1 ~ "correct", 
    he_2_5 == 1 ~ "correct",
    he_2_6 == 1 ~ "wrong",
    TRUE ~ "wrong"
  )
)

df <- df %>%
  mutate(across(99:104, ~ factor(., levels = c("wrong", "correct")))) # 1 = wrong; 2 = correct


pack_summary <- df %>%
  select(99:104) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  group_by(Question, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(
    df %>%
      select(99:104) %>%
      pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
      filter(!is.na(Response)) %>%
      group_by(Question) %>%
      summarise(Total = n(), .groups = "drop"),
    by = "Question"
  ) %>%
  mutate(Percent = (Count / Total) * 100)

# Map food codes to food names
pack_summary <- pack_summary %>%
  mutate(
    Product = recode(Question,
                     `br_pack` = "bread",
                     `bro_pack` = "broccoli",
                     `let_pack` = "lettuce",
                     `ba_pack` = "banana",
                     `be_pack` = "berries",
                     `he_pack` = "herbs",
    )
  )

# Plot
ggplot(pack_summary, aes(x = Product, y = Percent, fill = Response)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("wrong" = "#8c1212", "correct" = "#128c37")) +
  coord_flip() +
  labs(
    title = "Percentage of Correct storage",
    x = "Food product",
    y = "Percentage"
  ) +
  theme_minimal()
