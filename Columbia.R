---
Title: "Colombia_Analysis"
author: "Meroona Gopang"
format: html
editor: visual
---
  
  Colombia

{r}
library(dplyr)
library(readr)

# Step 1: Read all CSV files
Q1 <- read_csv("Q1.csv")
Q2 <- read_csv("Q2.csv")
Q3 <- read_csv("Q3.csv")
Q4 <- read_csv("Q4.csv")

# Step 2: Define columns to remove (based on previous merge errors)
cols_to_remove <- c(
  "exploitationsex",
  "eviction",
  "pregnantweeks",
  "pregnantlastdoc",
  "abductionORkidnapping"
)

# Step 3: Define a function to clean, filter, and label each dataset
clean_dataset <- function(df, quarter_name) {
  df %>%
    mutate(nationality = tolower(trimws(as.character(nationality)))) %>%
    filter(nationality == "venezuela") %>%
    select(-any_of(cols_to_remove)) %>%
    mutate(Quarter = quarter_name)
}

# Step 4: Apply the cleaning function to each quarter
Q1_clean <- clean_dataset(Q1, "Q1")
Q2_clean <- clean_dataset(Q2, "Q2")
Q3_clean <- clean_dataset(Q3, "Q3")
Q4_clean <- clean_dataset(Q4, "Q4")

# Step 5: Get common columns across all datasets
common_columns <- Reduce(intersect, list(
  names(Q1_clean), names(Q2_clean), names(Q3_clean), names(Q4_clean)
))

# Step 6: Select only common columns and bind rows
Q1_common <- Q1_clean[common_columns]
Q2_common <- Q2_clean[common_columns]
Q3_common <- Q3_clean[common_columns]
Q4_common <- Q4_clean[common_columns]

combined_venezuelan <- bind_rows(Q1_common, Q2_common, Q3_common, Q4_common)
df <- combined_venezuelan

{r}
library(dplyr)
library(ggplot2)

{r}
# Load required library
library(dplyr)

# Step 1: Filter the data
df_both<- df %>%
  filter(Childinschool == "Sí", Childvirtualed == "Sí")

{r}
# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Step 1: Select SpecificNeeds columns, excluding 'none'
df_needs <- df_both %>%
  select(starts_with("SpecificNeeds_")) %>%
  select(-SpecificNeeds_none)

# Step 2: Summarize counts of 'yes' responses and clean label names
needs_summary <- df_needs %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Need", values_to = "Count") %>%
  mutate(
    Need = str_remove(Need, "SpecificNeeds_"),
    Need = case_when(
      Need == "victimsurvivor" ~ "Survivor of Violence",
      Need == "treatmentyesaccess" ~ "Access To Treatment",
      TRUE ~ str_to_title(Need)
    )
  ) %>%
  arrange(desc(Count))

# Step 3: Create the plot
plot_virtual <- ggplot(needs_summary, aes(x = reorder(Need, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Specific Needs of Households with Children in Regular and Virtual School",
    x = "Specific Need",
    y = "Count of 'Yes' Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 11, face = "bold"),  # Bold, dark y-axis
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 4: Display the plot
print(plot_virtual)

# Step 5: Save the plot as PNG
ggsave("specific_needs_virtual.png", plot = plot_virtual, width = 10, height = 6, dpi = 300, bg = "white")


{r}
# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Step 1: Filter the dataset for children born in 2015 and select SpecificNeeds columns
df_needs <- df %>%
  filter(childborn2015 == "Sí") %>%
  select(starts_with("SpecificNeeds_")) %>%
  select(-SpecificNeeds_none)  # Exclude 'none' category

# Step 2: Summarize the 'yes' responses (coded as 1) and clean label names
needs_summary <- df_needs %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Need", values_to = "Count") %>%
  mutate(
    Need = str_remove(Need, "SpecificNeeds_"),
    Need = case_when(
      Need == "victimsurvivor" ~ "Survivor of Violence",
      Need == "treatmentyesaccess" ~ "Access To Treatment",
      TRUE ~ str_to_title(Need)
    )
  ) %>%
  arrange(desc(Count))

# Step 3: Create the plot
plot_needs_born2015 <- ggplot(needs_summary, aes(x = reorder(Need, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Specific Needs of Households with Children Born In and After 2015",
    x = "Specific Need",
    y = "Count of 'Yes' Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 4: Print the plot
print(plot_needs_born2015)

# Step 5: Save the plot
ggsave("specific_needs_born2015.png", plot = plot_needs_born2015, width = 10, height = 6, dpi = 300, bg = "white")


{r}

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

# Step 1: Filter for children born in 2015
df_2015 <- df %>%
  filter(childborn2015 == "Sí")

# Step 2: Select 'Childwhynotschool_' columns
df_reasons <- df_2015 %>%
  select(starts_with("Childwhynotschool_"))

# Step 3: Summarize counts and manually clean labels
reasons_summary <- df_reasons %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Reason", values_to = "Count") %>%
  mutate(
    Reason = str_remove(Reason, "Childwhynotschool_"),
    Reason = case_when(
      Reason == "Nodocs" ~ "No Docs",
      Reason == "Nospot" ~ "No Spot",
      Reason == "Nomoney" ~ "No Money",
      Reason == "Recentlyarrive" ~ "Recently Arrived",
      Reason == "Toolate" ~ "Too Late",
      Reason == "Noinfo" ~ "No Info",
      Reason == "Noschools" ~ "No Schools",
      Reason == "Failedschool" ~ "Failed School",
      Reason == "Notransport" ~ "No Transport",
      Reason == "Notinterest" ~ "No Interest",
      Reason == "Fearschool" ~ "Fear Of School",
      Reason == "Discriminethnic" ~ "Ethnic Discrimination",
      Reason == "Intransit" ~ "In Transit",
      Reason == "Helphome" ~ "Help At Home",
      Reason == "Finished" ~ "Finished Education",
      Reason == "Childwork" ~ "Child Labor",
      Reason == "Familynot" ~ "Family Not Supportive",
      Reason == "Migration" ~ "Migration",
      Reason == "Other" ~ "Other",
      TRUE ~ str_to_title(Reason)
    )
  ) %>%
  filter(Count > 0) %>%
  mutate(Reason = fct_reorder(Reason, Count)) %>%  # Reorder reasons after fixing labels
  arrange(desc(Count))

# Step 4: Plot
plot_reasons <- ggplot(reasons_summary, aes(x = Reason, y = Count)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Reasons Children Born in and After 2015 Are Not in School",
    x = "Reason",
    y = "Count of 'Yes' Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(color = "black", size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 5: Show the plot
print(plot_reasons)

# Step 6: Save the plot
ggsave("childwhynotschool_born2015_finalcleaned.png",
       plot = plot_reasons,
       width = 10, height = 6, dpi = 300, bg = "white")



