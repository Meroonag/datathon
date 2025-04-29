---
Title: "datathon"
author: "Meroona"
format: html
editor: visual
---
  
  {r}
install.packages("dplyr")
install.packages("ggplot2")

{r}
# Load required package
library(dplyr)
library(readr)  # Just to ensure read_csv works if not already loaded

# Define the data path and read in the CSV files
data_path <- "/Users/meroona/Documents/datathon"
rba_q1 <- read_csv(file.path(data_path, "rba_q1.csv"))
rba_q2 <- read_csv(file.path(data_path, "rba_q2.csv"))
rba_q3 <- read_csv(file.path(data_path, "rba_q3.csv"))
rba_q4 <- read_csv(file.path(data_path, "rba_q4.csv"))

# Filter for Venezuelan nationality in each dataset
rba_q1_ven <- rba_q1 %>% filter(nationality == "Venezuela")
rba_q2_ven <- rba_q2 %>% filter(nationality == "Venezuela")
rba_q3_ven <- rba_q3 %>% filter(nationality == "Venezuela")
rba_q4_ven <- rba_q4 %>% filter(nationality == "Venezuela")

# Combine (bind) all the filtered datasets into one
rba_all_ven <- bind_rows(rba_q1_ven, rba_q2_ven, rba_q3_ven, rba_q4_ven)

{r}
# Load dplyr for piping and filtering
library(dplyr)

# Define the countries of interest
selected_countries <- c("Mexico", "Panama", "Costa Rica")

# Apply filters to stratify the dataset
rba_ven_allage <- rba_all_ven %>%
  filter(
    sex == "woman",
    interview_country %in% selected_countries
  )


{r}

# Load necessary libraries
library(tidyverse)

# Step 1: Define allowed categories
allowed_needs <- c(
  "lactating", 
  "singlehead", 
  "pregnant", 
  "treatment", 
  "treatmentyesaccess", 
  "unacompanied", 
  "disability", 
  "victimsurvivor", 
  "elderly"
)

# Step 2: Clean text and filter only allowed needs
needs_long_filtered <- rba_ven_allage %>%
  mutate(
    specific_needs_clean = specific_needs %>%
      tolower() %>%
      trimws() %>%
      str_squish()
  ) %>%
  filter(
    !is.na(specific_needs_clean),
    specific_needs_clean %in% allowed_needs
  ) %>%
  mutate(
    specific_needs_clean = case_when(
      specific_needs_clean == "singlehead" ~ "Single Head of Household",
      specific_needs_clean == "victimsurvivor" ~ "Survivor of Violence",  # updated here
      specific_needs_clean == "treatmentyesaccess" ~ "Access to Treatment",
      TRUE ~ str_to_title(specific_needs_clean)  # Capitalize others normally
    )
  )

# Step 3: Check the cleaned specific_needs (optional)
unique(needs_long_filtered$specific_needs_clean)

# Step 4: Create outputs folder (if not already created)
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 5: Define plotting function
plot_specific_needs_by_country <- function(country_name, fill_color = "#1f77b4") {
  
  country_data <- needs_long_filtered %>%
    filter(interview_country == country_name) %>%
    count(specific_needs_clean) %>%
    arrange(desc(n))
  
  p <- ggplot(country_data, aes(x = reorder(specific_needs_clean, n), y = n)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(
      title = paste("Specific Needs of Venezuelan Women in", country_name),
      x = "Specific Need",
      y = "Number of Mentions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(color = "black", size = 12, face = "bold"),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),
      plot.title = element_text(color = "black", size = 15, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save and show the plot
  ggsave(
    filename = file.path(data_path, paste0("specific_needs_", tolower(gsub(" ", "_", country_name)), "_filtered.png")),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(p)  # Show the plot immediately
}

# Step 6: Plot for all 3 countries
countries_to_plot <- c("Mexico", "Panama", "Costa Rica")

for (country in countries_to_plot) {
  plot_specific_needs_by_country(country)
}


{r}
library(tidyverse)
# Define the countries of interest
selected_countries <- c("Mexico", "Panama", "Costa Rica")

# Apply filters to stratify the dataset
rba_ven_adults <- rba_all_ven %>%
  filter(
    age == "18 - 24 years",
    sex == "woman",
    interview_country %in% selected_countries
  )



{r}
# Load libraries
library(tidyverse)

# Step 1: Define allowed categories
allowed_needs <- c(
  "lactating", 
  "singlehead", 
  "pregnant", 
  "treatment", 
  "treatmentyesaccess", 
  "unacompanied", 
  "disability", 
  "victimsurvivor", 
  "elderly"
)

# Step 2: Clean text and filter only allowed needs
needs_long_filtered <- rba_ven_adults %>%
  mutate(
    specific_needs_clean = specific_needs %>%
      tolower() %>%
      trimws() %>%
      str_squish()
  ) %>%
  filter(
    !is.na(specific_needs_clean),
    specific_needs_clean %in% allowed_needs
  ) %>%
  mutate(
    specific_needs_clean = case_when(
      specific_needs_clean == "victimsurvivor" ~ "Survivor of Violence",  # manual fix
      specific_needs_clean == "singlehead" ~ "Single Head of Household",
      specific_needs_clean == "treatmentyesaccess" ~ "Access to Treatment",
      TRUE ~ str_to_title(specific_needs_clean)   # capitalize other cases normally
    )
  )

# Step 3: Check cleaned values (optional)
unique(needs_long_filtered$specific_needs_clean)

# Step 4: Create outputs folder if not already created
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 5: Define plotting function
plot_specific_needs_by_country <- function(country_name, fill_color = "#1f77b4") {
  
  country_data <- needs_long_filtered %>%
    filter(interview_country == country_name) %>%
    count(specific_needs_clean) %>%
    arrange(desc(n))
  
  p <- ggplot(country_data, aes(x = reorder(specific_needs_clean, n), y = n)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(
      title = paste("Specific Needs of Venezuelan Female Youth in", country_name),
      x = "Specific Need",
      y = "Number of Mentions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(color = "black", size = 12, face = "bold"),
      plot.title = element_text(color = "black", size = 15, face = "bold"),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save and show the plot
  ggsave(
    filename = file.path(data_path, paste0("specific_needs_adults_", tolower(gsub(" ", "_", country_name)), "_filtered.png")),
    plot = p,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  print(p)
}

# Step 6: Plot for all 3 countries
countries_to_plot <- c("Mexico", "Panama", "Costa Rica")

for (country in countries_to_plot) {
  plot_specific_needs_by_country(country)
}


{r}
#Define the countries of interest
selected_countries <- c("Mexico", "Panama", "Costa Rica")
# Apply filters to stratify the dataset
rba_ven_child <- rba_all_ven %>%
  filter(
    age == "12 - 17 years",
    sex == "woman",
    interview_country %in% selected_countries
  )





{r}
# Load libraries
library(tidyverse)

# Step 1: Create output folder if it doesn't exist
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 2: Disaggregate and clean main_concerns for Costa Rica
concerns_costa_rica <- rba_ven_allage %>%
  filter(
    interview_country == "Costa Rica",
    !is.na(main_concerns)
  ) %>%
  separate_rows(main_concerns, sep = "\\s{2,}|\\s") %>%
  mutate(
    main_concerns = str_replace_all(main_concerns, "_", " "),       # Replace underscores with spaces
    main_concerns = str_to_title(trimws(main_concerns))             # Capitalize nicely
  ) %>%
  filter(main_concerns != "") %>%
  count(main_concerns) %>%
  arrange(desc(n))

# Step 3: Plot the cleaned, disaggregated concerns (NO NUMBERS on bars)
plot_cr <- ggplot(concerns_costa_rica, aes(x = reorder(main_concerns, n), y = n)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Main Concerns of Venezuelan Girls in Costa Rica",
    x = "Main Concern",
    y = "Number of Mentions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 4: Save and show the plot
ggsave(
  filename = file.path(data_path, "main_concerns_disaggregated_costa_rica_cleaned_nonumber.png"),
  plot = plot_cr,
  width = 10,
  height = 6,
  dpi = 300
)

print(plot_cr)

{r}
library(tidyverse)

# Step 1: Clean the 'meals_yesterday' column
rba_ven_child_cleaned <- rba_ven_child %>%
  mutate(
    meals_yesterday = meals_yesterday %>%
      tolower() %>%
      str_replace_all("_", " ") %>%
      str_squish() %>%
      str_to_title()
  )

# Step 2: Create outputs folder if not already existing
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 3: Define the plotting function (specific for Costa Rica)
plot_meals_yesterday_costa_rica <- function(fill_color = "#1f77b4") {
  
  country_data <- rba_ven_child_cleaned %>%
    filter(interview_country == "Costa Rica", !is.na(meals_yesterday)) %>%
    count(meals_yesterday) %>%
    arrange(desc(n))
  
  p <- ggplot(country_data, aes(x = reorder(meals_yesterday, n), y = n)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(
      title = "Meals Eaten Yesterday by Venezuelan Girls in Costa Rica",
      x = "Meals Yesterday",
      y = "Number of Mentions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(color = "black", size = 12, face = "bold"),
      plot.title = element_text(color = "black", size = 15, face = "bold"),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save and show the plot
  ggsave(
    filename = file.path(data_path, "meals_yesterday_children_costa_rica_cleaned.png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(p)
}

# Step 4: Run the plot
plot_meals_yesterday_costa_rica()


{r}
# Load libraries
library(tidyverse)

# Step 1: Create output folder if it doesn't exist
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 2: Disaggregate and clean main_concerns for Costa Rica
concerns_mexico <- rba_ven_child %>%
  filter(
    interview_country == "Mexico",
    !is.na(main_concerns)
  ) %>%
  separate_rows(main_concerns, sep = "\\s{2,}|\\s") %>%
  mutate(
    main_concerns = str_replace_all(main_concerns, "_", " "),       # Replace underscores with spaces
    main_concerns = str_to_title(trimws(main_concerns))             # Capitalize nicely
  ) %>%
  filter(main_concerns != "") %>%
  count(main_concerns) %>%
  arrange(desc(n))

# Step 3: Plot the cleaned, disaggregated concerns (NO NUMBERS on bars)
plot_cr <- ggplot(concerns_mexico, aes(x = reorder(main_concerns, n), y = n)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Main Concerns of Venezuelan Girls in Mexico",
    x = "Main Concern",
    y = "Number of Mentions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 4: Save and show the plot
ggsave(
  filename = file.path(data_path, "main_concerns_disaggregated_mexico_cleaned_nonumber.png"),
  plot = plot_cr,
  width = 10,
  height = 6,
  dpi = 300
)

print(plot_cr)


{r}
# Load libraries
library(tidyverse)

# Step 1: Create output folder if it doesn't exist
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 2: Disaggregate and clean main_concerns for Panama
concerns_panama <- rba_ven_child %>%
  filter(
    interview_country == "Panama",
    !is.na(main_concerns)
  ) %>%
  separate_rows(main_concerns, sep = "\\s{2,}|\\s") %>%
  mutate(
    main_concerns = str_replace_all(main_concerns, "_", " "),       # Replace underscores with spaces
    main_concerns = str_to_title(trimws(main_concerns))             # Capitalize nicely
  ) %>%
  filter(main_concerns != "") %>%
  count(main_concerns) %>%
  arrange(desc(n))

# Step 3: Plot the cleaned, disaggregated concerns (NO NUMBERS on bars)
plot_cr <- ggplot(concerns_panama, aes(x = reorder(main_concerns, n), y = n)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Main Concerns of Venezuelan Girls in Panama",
    x = "Main Concern",
    y = "Number of Mentions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Step 4: Save and show the plot
ggsave(
  filename = file.path(data_path, "main_concerns_panama.png"),
  plot = plot_cr,
  width = 12,
  height = 6,
  dpi = 300
)

print(plot_cr)

{r}
# Load libraries
library(tidyverse)

# Step 1: Clean the 'group_travel' column (in rba_ven_adults)
rba_ven_adults_cleaned <- rba_ven_adults %>%
  mutate(
    group_travel = group_travel %>%
      tolower() %>%
      str_replace_all("_", " ") %>%
      str_squish() %>%
      str_to_title()
  )

# Step 2: Create outputs folder if not already existing
data_path <- "outputs"
if (!dir.exists(data_path)) dir.create(data_path)

# Step 3: Define the plotting function for Panama (with blue color)
plot_group_travel_panama <- function(fill_color = "#1f77b4") {  # Blue color
  
  country_data <- rba_ven_adults_cleaned %>%
    filter(interview_country == "Panama", !is.na(group_travel)) %>%
    count(group_travel) %>%
    arrange(desc(n))
  
  p <- ggplot(country_data, aes(x = reorder(group_travel, n), y = n)) +
    geom_col(fill = fill_color) +
    coord_flip() +
    labs(
      title = "Group Travel Arrangement by Venezuelan Female Youth in Panama",
      x = "Group Travel Type",
      y = "Number of Mentions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(color = "black", size = 12, face = "bold"),   # Bold left labels
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),  # Bold Y axis title
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),  # Bold X axis title
      plot.title = element_text(color = "black", size = 15, face = "bold"),    # Bold title
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save and show the plot
  ggsave(
    filename = file.path(data_path, "group_travel_panama.png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(p)
}

# Step 4: Run the plot
plot_group_travel_panama()


