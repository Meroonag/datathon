library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)


### Upload files

# Define the folder path
folder_path <- "C:/Users/Ladina/Downloads/Peru/UNHCR_PER_2023_RMS_data_v2_1"

# Read the household data
households <- read.csv(file.path(folder_path, "UNHCR_PER_2023_RMS_data_household_v2.1.csv"), stringsAsFactors = FALSE)

# Read the member data
children <- read.csv(file.path(folder_path, "UNHCR_PER_2023_RMS_data_member_v2.1.csv"), stringsAsFactors = FALSE)


# -------------------------------------------------------------------------------
##### CHILDREN
# -------------------------------------------------------------------------------

### Step 1: Join data 

# Join on household ID
df <- left_join(children, households, by = "id")


### Step 2: Proportion of Children in School and Level

df <- df %>%
  mutate(in_school = ifelse(EDU02 == "yes", 1, 0))

# Overall proportion
in_school_rate <- mean(df$in_school, na.rm = TRUE)
print(paste("Proportion of children in school:", round(in_school_rate * 100, 2), "%"))

df %>%
  count(in_school) %>%
  mutate(status = ifelse(in_school == 1, "In School", "Not in School")) %>%
  ggplot(aes(x = status, y = n, fill = status)) +
  geom_col() +
  labs(title = "School Attendance - Current Year", y = "Number of Children", x = "") +
  theme_minimal()


### Step 2.1: Proportion of Children in School and Level

# Prepare the data
edu_levels <- df %>%
  filter(in_school == 1, !is.na(type_edu), type_edu != "") %>%  # Filter out blank values for children not in school
  mutate(type_edu = recode(type_edu,
                           "basica_regular" = "Regular Basic",
                           "basica_alternativa" = "Alternative Basic",
                           "basica_especial" = "Special Basic",
                           "nosabe" = "Don't know"
  )) %>%
  count(type_edu) %>%
  mutate(percentage = round(n / sum(n) * 100, 1),
         label = paste0(type_edu, " (", percentage, "%)"))

# Add cumulative positions for label placement
edu_levels <- edu_levels %>%
  arrange(desc(type_edu)) %>%
  mutate(ypos = cumsum(n) - 0.5 * n)

# Plot
ggplot(edu_levels, aes(x = 2, y = n, fill = type_edu)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.7) +
  theme_void() +
  geom_label_repel(
    aes(y = ypos, label = label),
    x = 2.7, # nudged further out
    nudge_x = 0.5,
    segment.color = "grey40",
    show.legend = FALSE,
    size = 4
  ) +
  labs(title = "Level of Basic Education Children Are Enrolled In") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")  # Remove the legend


### Step 3: Reasons for Not Being in School

# Filter not in school
not_in_school <- df %>% filter(in_school == 0)

# Select relevant columns (those that represent reasons)
reason_cols <- not_in_school %>% select(starts_with("Childwhynotschool__"))

# Summarize counts
reason_summary <- colSums(reason_cols, na.rm = TRUE) %>%
  sort(decreasing = TRUE)

# Convert to a data frame for plotting
reason_df <- data.frame(
  reason = names(reason_summary),
  count = as.numeric(reason_summary)
)

# Add clean, human-readable labels
reason_labels <- c(
  "Childwhynotschool__nodocs" = "No documents",
  "Childwhynotschool__nomoney" = "No money",
  "Childwhynotschool__failedschool" = "Failed school",
  "Childwhynotschool__childwork" = "Child labor",
  "Childwhynotschool__fearschool" = "Fear of school",
  "Childwhynotschool__disease" = "Disease",
  "Childwhynotschool__disability" = "Disability",
  "Childwhynotschool__helphome" = "Helps at home",
  "Childwhynotschool__familynot" = "Family doesn't allow",
  "Childwhynotschool__noschools" = "No schools available",
  "Childwhynotschool__nointerest" = "No interest",
  "Childwhynotschool__pregnancy" = "Pregnancy",
  "Childwhynotschool__nospot" = "No spot in school",
  "Childwhynotschool__notransport" = "No transport",
  "Childwhynotschool__discrimnation" = "Discrimination (national)",
  "Childwhynotschool__discrimethnic" = "Discrimination (ethnic)",
  "Childwhynotschool__finished" = "Finished schooling",
  "Childwhynotschool__recentlyarriv" = "Recently arrived",
  "Childwhynotschool__intransit" = "In transit",
  "Childwhynotschool__noinfo" = "No information",
  "Childwhynotschool__toolate" = "Too late to enroll",
  "Childwhynotschool__nolanguage" = "Language barrier",
  "Childwhynotschool__other" = "Other",
  "Childwhynotschool__novacancy" = "No vacancy",
  "Childwhynotschool__schoolaway" = "School too far",
  "Childwhynotschool__noresources" = "Lack of resources",
  "Childwhynotschool__documentation" = "Documentation issues",
  "Childwhynotschool__nopriority" = "No enrollment priority",
  "Childwhynotschool__traslate" = "Translation issues",
  "Childwhynotschool__discriminatio" = "Discrimination (other)"
)

# Add a new column for the readable label
reason_df$label <- reason_labels[reason_df$reason]

# Remove rows where count is zero
reason_df <- reason_df %>% filter(count > 0)
                                     # Plot using the clean label
ggplot(reason_df, aes(x = reorder(label, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Reasons Children Are Not in School",
       x = "", y = "Number of Children") +
  theme_minimal(base_size = 14)


### Step 3.1: Years out of School

# Filter out blank and NA values
df_yearsout <- df %>%
  filter(!is.na(yearsout) & yearsout != "") %>%
  mutate(yearsout = factor(yearsout,
                           levels = c("yearsoutless_1_year", "yearsout1_year", "yearsout2_years", "yearsout3_years", "yearsoutmore_3_years"),
                           labels = c("< 1 year", "1 year", "2 years", "3 years", "More than 3 years")))

# Bar plot
ggplot(df_yearsout, aes(x = yearsout)) +
  geom_bar(fill = "steelblue") +
  labs(title = "How Long Have Children Been Out of the Education System?",
       x = "Years Out of School",
       y = "Number of Children") +
  theme_minimal()


### Step 4: Difficulties for Children in School

# Filter children who are currently in school
in_school_children <- df %>% filter(in_school == 1)

# Select the columns related to education difficulties
difficulty_cols <- in_school_children %>% select(starts_with("edu_dificulties__"))

# Summarize how often each difficulty is selected
difficulty_summary <- colSums(difficulty_cols, na.rm = TRUE) %>%
  sort(decreasing = TRUE)

# Convert to data frame for plotting
difficulty_df <- data.frame(
  difficulty = names(difficulty_summary),
  count = as.numeric(difficulty_summary)
)

# Create clean readable labels
difficulty_labels <- c(
  "edu_dificulties__1" = "Document issues",
  "edu_dificulties__2" = "Economic difficulties",
  "edu_dificulties__3" = "Distance to school",
  "edu_dificulties__4" = "Lack of school supplies",
  "edu_dificulties__5" = "Bullying",
  "edu_dificulties__6" = "Poor academic performance",
  "edu_dificulties__7" = "Disability-related difficulties",
  "edu_dificulties__98" = "No difficulties",
  "edu_dificulties__96" = "Other"
)

# Add labels
difficulty_df$label <- difficulty_labels[difficulty_df$difficulty]

# Remove rows with zero counts
difficulty_df <- difficulty_df %>% filter(count > 0)

# Plot
ggplot(difficulty_df, aes(x = reorder(label, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Challenges for Children Continuing School",
       x = "", y = "Number of Children") +
  theme_minimal(base_size = 14)


### Step 5: Extracurricular Activities

# Filter and label COMM01
df_comm <- df %>%
  filter(!is.na(COMM01) & COMM01 != "" & !COMM01 %in% c("notell", "dontknow")) %>%
  mutate(COMM01 = factor(COMM01, levels = c("no", "yes"), labels = c("No", "Yes")))

# Bar plot
ggplot(df_comm, aes(x = COMM01, fill = COMM01)) +
  geom_bar() +
  scale_fill_manual(values = c("salmon", "seagreen")) +
  labs(title = "Participation in Extracurricular Activities",
       x = "Participated in Last Month?",
       y = "Number of Children") +
  theme_minimal() +
  theme(legend.position = "none")


# -------------------------------------------------------------------------------
##### HOUSEHOLD
# -------------------------------------------------------------------------------

### Step 1: Specific Needs in the Household

# Filter to unique households only
households_unique <- df %>% distinct(id, .keep_all = TRUE)

# Select relevant columns
specific_needs_cols <- households_unique %>%
  select(starts_with("SpecificNeeds__"))

# Summarize
specific_needs_summary <- colSums(specific_needs_cols, na.rm = TRUE) %>%
  sort(decreasing = TRUE)

# Label mapping
specific_needs_labels <- c(
  "SpecificNeeds__disability" = "Disability",
  "SpecificNeeds__treatment" = "Chronic condition (no treatment)",
  "SpecificNeeds__treatmentaccess" = "Chronic condition (with treatment)",
  "SpecificNeeds__unacompanied" = "Unaccompanied minor",
  "SpecificNeeds__elderly" = "Elderly (60+)",
  "SpecificNeeds__pregnant" = "Pregnant woman",
  "SpecificNeeds__lactating" = "Lactating woman",
  "SpecificNeeds__singlehead" = "Single parent",
  "SpecificNeeds__victimsurvivor" = "Survivor of violence",
  "SpecificNeeds__lgtbi" = "LGTBIQ",
  "SpecificNeeds__none" = "None"
)

# Prepare for plotting
specific_needs_df <- data.frame(
  need = names(specific_needs_summary),
  count = as.numeric(specific_needs_summary)
) %>%
  filter(count > 0) %>%
  mutate(label = specific_needs_labels[need])

# Plot
ggplot(specific_needs_df, aes(x = reorder(label, count), y = count)) +
  geom_col(fill = "#2b8cbe") +
  coord_flip() +
  labs(title = "Specific Needs in Households with Children",
       x = "", y = "Number of Households") +
  theme_minimal()


### Step 1.1: Types of Disabilities Reported

# Select relevant columns from distinct households
disability_cols <- households_unique %>%
  select(starts_with("disabilitytype__"))

# Summarize
disability_summary <- colSums(disability_cols, na.rm = TRUE) %>%
  sort(decreasing = TRUE)

# Label mapping
disability_labels <- c(
  "disabilitytype__physical" = "Physical",
  "disabilitytype__mentalemopsy" = "Mental/emotional",
  "disabilitytype__mentalcog" = "Mental/cognitive",
  "disabilitytype__visual" = "Visual",
  "disabilitytype__hearing" = "Hearing",
  "disabilitytype__verbal" = "Verbal",
  "disabilitytype__multi" = "Multiple disabilities"
)

# Prepare for plotting
disability_df <- data.frame(
  disability = names(disability_summary),
  count = as.numeric(disability_summary)
) %>%
  filter(count > 0) %>%
  mutate(label = disability_labels[disability])

# Plot
ggplot(disability_df, aes(x = reorder(label, count), y = count)) +
  geom_col(fill = "#fb6a4a") +
  coord_flip() +
  labs(title = "Types of Disabilities in Households with Children",
       x = "", y = "Number of Households") +
  theme_minimal()


### Step 2: Minors Left Behind

# Filter out blanks or NAs
minorsleft_summary <- households %>%
  filter(!is.na(minorsleft), minorsleft != "")

# Create a summary table
minorsleft_summary <- minorsleft_summary %>%
  group_by(minorsleft) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot
ggplot(minorsleft_summary, aes(x = reorder(minorsleft, count), y = count)) +
  geom_col(fill = "#6a51a3") +
  coord_flip() +
  labs(title = "Households with Minors Left Behind",
       x = "", y = "Number of Households") +
  theme_minimal()


### Step 3: Route Incidents

# Filter for relevant columns
route_incident_cols <- households_unique %>%
  select(starts_with("RouteIncType__")) %>%
  filter(complete.cases(.))  # Remove rows with missing data

# Summarize incidents
route_incident_summary <- colSums(route_incident_cols, na.rm = TRUE) %>%
  sort(decreasing = TRUE)

# Convert to data frame
route_incident_df <- data.frame(
  incident = names(route_incident_summary),
  count = as.numeric(route_incident_summary)
) %>%
  filter(count > 0)  # Remove rows with no incidents

# Create a named vector for label mapping
incident_labels <- c(
  RouteIncType__theft = "Theft",
  RouteIncType__eviction = "Eviction",
  RouteIncType__evictionthreat = "Threat of eviction",
  RouteIncType__threat = "Physical threat or intimidation",
  RouteIncType__fraud = "Scam or fraud",
  RouteIncType__homicide = "Homicide",
  RouteIncType__physicalAssault = "Physical assault/abuse",
  RouteIncType__sexualAssault = "Sexual assault",
  RouteIncType__abductionkidnap = "Abduction or kidnapping",
  RouteIncType__Exploitationsex = "Sexual exploitation",
  RouteIncType__Exploitationwork = "Labour exploitation",
  RouteIncType__arrestORdetention = "Arbitrary arrest/detention",
  RouteIncType__bribery = "Bribery from officials",
  RouteIncType__deportation = "Deportation",
  RouteIncType__destructionPropert = "Destruction of property",
  RouteIncType__notell = "Prefer not to tell",
  RouteIncType__other = "Other",
  RouteIncType__despojo = "Dispossession",
  RouteIncType__displaced = "Forced displacement",
  RouteIncType__confine = "Confinement or mobility restriction",
  RouteIncType__extors = "Extortion"
)

# Apply the mapping to clean labels
route_incident_df <- route_incident_df %>%
  mutate(
    clean_label = incident_labels[incident],
    prop = count / sum(count),
    label = paste0(clean_label, " (", scales::percent(prop, accuracy = 1), ")"),
    ypos = cumsum(prop) - 0.5 * prop
  )

# Convert clean_label to factor to keep order
route_incident_df$clean_label <- factor(route_incident_df$clean_label, levels = route_incident_df$clean_label)

# Updated Bar chart
ggplot(route_incident_df, aes(x = reorder(clean_label, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Types of Route Incidents", x = "", y = "Number of Incidents") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Adjust text size


### Step 4: Documentation and Regular Entry

### Step 4.1: REG02: Do you have any other document that establishes his/her legal identity?

# Filter and summarize REG02, removing blanks (NA values)
reg02_summary <- households_unique %>%
  count(REG02) %>%
  filter(!is.na(REG02) & REG02 != "")  # Remove NA and blank values

# Create a named vector for REG02 labels
reg02_labels <- c(
  "no" = "No",
  "yes" = "Yes",
  "notell" = "Prefer not to answer"
)

# Filter and summarize REG02, removing blanks (NA values)
reg02_summary <- households_unique %>%
  count(REG02) %>%
  filter(!is.na(REG02) & REG02 != "") %>%
  mutate(REG02_clean = reg02_labels[REG02]) %>%
  arrange(desc(n))  # Arrange by descending count

# Horizontal bar chart for REG02
ggplot(reg02_summary, aes(x = reorder(REG02_clean, n), y = n, fill = REG02_clean)) +
  geom_bar(stat = "identity") +
  labs(title = "Legal Identity Documents for Children", x = "", y = "Number of Children") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "darkred"),
    legend.position = "none"  # Hide legend since labels are obvious
  ) +
  coord_flip() +
  scale_fill_manual(values = c("No" = "firebrick", "Yes" = "forestgreen", "Prefer not to answer" = "grey50"))


### Step 4.2: REG03: Is there a child in the home who does not have a birth registration?

# Create label mapping
yes_no_labels <- c(
  "yes" = "Yes",
  "no" = "No"
)

# Filter and summarize REG03, removing blanks (NA values)
reg03_summary <- households_unique %>%
  count(REG03) %>%
  filter(!is.na(REG03) & REG03 != "") %>%
  mutate(REG03 = yes_no_labels[REG03])  # Just update labels here

# Bar chart for REG03 (keep your original style)
ggplot(reg03_summary, aes(x = REG03, y = n, fill = REG03)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(n, "responses")), vjust = -0.5, size = 4, fontface = "bold", color = "white") +
  labs(title = "Children Without Birth Registration", x = "Response", y = "Number of Children") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black")
  )

### Step 4.3: REG04: Has the birth been registered with civil authorities?

# Create label mapping
reg04_labels <- c(
  "yes" = "Yes",
  "no" = "No",
  "notell" = "Prefer not to answer"
)

# Filter and summarize REG04, removing blanks (NA values)
reg04_summary <- households_unique %>%
  count(REG04) %>%
  filter(!is.na(REG04) & REG04 != "") %>%
  mutate(REG04 = reg04_labels[REG04])  # Update labels

# Horizontal bar chart for REG04, ordered by count
ggplot(reg04_summary, aes(x = reorder(REG04, n), y = n, fill = REG04)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Birth Registration with Civil Authorities",
    x = "Response",
    y = "Number of Children"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none")


### Health / Access to medical services

### HACC01: During the past 30 days, has consulted a health practitioner, dentist, traditional healer, or pharmacist, or visited a health center?

hacc01_labels <- c(
  "yes" = "Yes",
  "no" = "No")

# Filter and summarize HACC01, removing blanks (NA values)
hacc01_summary <- households_unique %>%
  count(HACC01) %>%
  filter(!is.na(HACC01) & HACC01 != "") %>%
  mutate(HACC01 = hacc01_labels[HACC01])  # Update labels

# Bar chart for HACC01
ggplot(hacc01_summary, aes(x = HACC01, y = n, fill = HACC01)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(n, "responses")), vjust = -0.5, size = 4, fontface = "bold", color = "white") +
  labs(title = "Consultation with Health Practitioners in the Past 30 Days", x = "Response", y = "Number of Responses") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", color = "black"),
        legend.position = "none")

### 2. HACC02: For what reason(s) did seek consultation?

# Create label mapping
hacc02_labels <- c(
  "HACC02__1" = "Illness",
  "HACC02__2" = "Injury",
  "HACC02__3" = "General check-up",
  "HACC02__4" = "Pre/Postnatal check-up",
  "HACC02__5" = "Giving birth",
  "HACC02__96" = "Other"
)

# Select only the relevant columns
hacc02_data <- households_unique %>%
  select(all_of(names(hacc02_labels)))

# Sum the counts for each reason
hacc02_summary <- colSums(hacc02_data, na.rm = TRUE) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("reason") %>%
  rename(count = ".")

# Map the labels
hacc02_summary <- hacc02_summary %>%
  mutate(reason = hacc02_labels[reason]) %>%
  filter(count > 0)  # Optional: remove reasons with 0 responses

# Create a horizontal bar chart
ggplot(hacc02_summary, aes(x = reorder(reason, count), y = count, fill = reason)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Reasons for Seeking Consultation in the Past 30 Days",
    x = "Reason",
    y = "Number of Responses"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"  # Hide the legend for cleaner look
  )


### 3. HACC03: During the past 30 days, has needed health services that they could not have access to?

# Clean up the labels for HACC03
hacc03_summary <- households_unique %>%
  count(HACC03) %>%
  filter(!is.na(HACC03) & HACC03 != "") %>%
  mutate(HACC03 = recode(HACC03, 
                         'dontknow' = "Don't Know", 
                         'yes' = "Yes", 
                         'no' = "No"))

# Bar chart for HACC03
ggplot(hacc03_summary, aes(x = reorder(HACC03, n), y = n, fill = HACC03)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Make it horizontal
  labs(title = "Health Services Needed but Unavailable in the Past 30 Days", 
       x = "Response", 
       y = "Number of Responses") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"  # Remove the legend
  )

### 4. HACC04: Why has been unable to access medical treatment in the past 30 days?

# Select relevant columns
hacc04_cols <- households_unique %>%
  select(starts_with("HACC04__")) %>%
  filter(complete.cases(.))  # Only keep complete rows 

# Summarize the reasons
hacc04_summary <- colSums(hacc04_cols == 1, na.rm = TRUE) %>%
  sort(decreasing = FALSE)

# Convert to data frame
hacc04_df <- data.frame(
  reason = names(hacc04_summary),
  count = as.numeric(hacc04_summary)
)

# Remove reasons with zero count
hacc04_df <- hacc04_df %>%
  filter(count > 0)

# Replace the column names with readable labels
reason_labels <- c(
  "HACC04__1" = "Lack of money",
  "HACC04__2" = "No medical personnel available",
  "HACC04__3" = "Turned away because facility was full",
  "HACC04__4" = "Turned away because facility was closed",
  "HACC04__5" = "Hospital/Clinic not having enough supplies or tests",
  "HACC04__6" = "Health facility is too far",
  "HACC04__7" = "Fear of contracting a communicable disease (e.g. COVID-19)",
  "HACC04__8" = "Lockdown/Travel restrictions",
  "HACC04__9" = "Too Far/transport issues",
  "HACC04__10" = "Health facility is destroyed",
  "HACC04__11" = "Did not have health insurance",
  "HACC04__96" = "Other"
)

hacc04_df$reason <- recode(hacc04_df$reason, !!!reason_labels)

# Make 'reason' a factor to preserve the sorted order
hacc04_df$reason <- factor(hacc04_df$reason, levels = hacc04_df$reason)

# Bar chart
ggplot(hacc04_df, aes(x = reason, y = count, fill = reason)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Horizontal bars
  labs(title = "Reasons for Inability to Access Medical Treatment", x = "Reason", y = "Number of Responses") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", color = "black"),
        legend.position = "none",  # Remove legend
        axis.text.y = element_text(size = 10))  # Adjust y-axis text size

### 5. HEA01: When anyone in your household is sick, where do they go to seek care?
## no labels - not able to produce graph

### 6. HEA02: How do you reach this facility if you need to seek care?
## same as above - no labels - not able to produce graph

### 7. HEA03: How long does it take to go there when you use the mode of transport mentioned?

# Create categories
households_unique <- households_unique %>%
  mutate(
    HEA03_cat = case_when(
      HEA03 <= 15 ~ "0–15 min",
      HEA03 <= 30 ~ "16–30 min",
      HEA03 <= 60 ~ "31–60 min",
      HEA03 <= 120 ~ "1–2 hours",
      HEA03 <= 240 ~ "2–4 hours",
      HEA03 > 240 ~ "More than 4 hours",
      TRUE ~ NA_character_
    )
  )

# Summarize by categories
hea03_summary <- households_unique %>%
  count(HEA03_cat) %>%
  filter(!is.na(HEA03_cat))

# Make sure the categories are ordered logically
hea03_summary$HEA03_cat <- factor(hea03_summary$HEA03_cat, levels = c("0–15 min", "16–30 min", "31–60 min", "1–2 hours", "2–4 hours", "More than 4 hours"))

# Bar chart
ggplot(hea03_summary, aes(x = HEA03_cat, y = n, fill = HEA03_cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 4, fontface = "bold", color = "white") +
  labs(title = "Time Taken to Reach Health Facility", x = "Time Taken", y = "Number of Responses") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"  # remove legend
  )

### 8. mentalhealthsymp: Feelings/Behavior Changes in the Last 3 Months
## this is only for support for mental health, not actually symptoms changed - decided to exclude


### Food / Nutrition

### Social benefits:

# Social Benefits - Long format
social_benefits <- households_unique %>%
  select(starts_with("SocialBenefitType__")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "benefit", values_to = "count") %>%
  filter(count > 0)

# Relabel
social_benefits_labels <- c(
  "SocialBenefitType__cash" = "Cash subsidy (or prepaid card)",
  "SocialBenefitType__housing" = "Housing or housing payment",
  "SocialBenefitType__foodcard" = "Food card",
  "SocialBenefitType__foodbag" = "Delivery of foods",
  "SocialBenefitType__psych" = "Psychological assistance",
  "SocialBenefitType__med" = "Medical Assistance",
  "SocialBenefitType__relocate" = "Relocation support",
  "SocialBenefitType__hygiene" = "Hygiene kit",
  "SocialBenefitType__other" = "Other",
  "SocialBenefitType__foodration" = "Food ration",
  "SocialBenefitType__foodkit" = "Food kit",
  "SocialBenefitType__pss" = "Psychosocial support",
  "SocialBenefitType__medical" = "Medical assistance",
  "SocialBenefitType__education" = "Equipment to access education",
  "SocialBenefitType__livelihoods" = "Livelihoods training",
  "SocialBenefitType__passage" = "Mobility aids"
)

social_benefits$benefit <- recode(social_benefits$benefit, !!!social_benefits_labels)
social_benefits <- social_benefits %>% arrange(desc(count))

# Plot
ggplot(social_benefits, aes(x = reorder(benefit, count), y = count, fill = benefit)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Types of Aid or Social Protection Received", x = "Type of Aid", y = "Number of Responses") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"
  )

### Coping skills:

# Coping Skills - Long format
coping_skills <- households_unique %>%
  select(starts_with("CopingMechanism1__")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "coping", values_to = "count") %>%
  filter(count > 0)

# Relabel
coping_skills_labels <- c(
  "CopingMechanism1__limitAdultFood" = "Restrict food consumption (adults for children)",
  "CopingMechanism1__reducedFood" = "Reduced food quantity/quality",
  "CopingMechanism1__borrowMoney" = "Borrow money",
  "CopingMechanism1__workFood" = "Work for goods (not cash)",
  "CopingMechanism1__familySupport" = "Family support",
  "CopingMechanism1__useSavings" = "Use savings",
  "CopingMechanism1__reduceExp" = "Reduce non-essential expenses",
  "CopingMechanism1__sellAssets" = "Sell assets",
  "CopingMechanism1__aidAgency" = "Seek aid from agencies/NGO",
  "CopingMechanism1__aidHost" = "Receive donations",
  "CopingMechanism1__skippedRent" = "Skip rent payments",
  "CopingMechanism1__changedShelter" = "Move to less adequate shelter",
  "CopingMechanism1__none" = "None",
  "CopingMechanism1__notell" = "Prefer not to say",
  "CopingMechanism1__other" = "Other"
)

coping_skills$coping <- recode(coping_skills$coping, !!!coping_skills_labels)
coping_skills <- coping_skills %>% arrange(desc(count))

# Plot
ggplot(coping_skills, aes(x = reorder(coping, count), y = count, fill = coping)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Coping Mechanisms to Meet Basic Needs", x = "Coping Mechanism", y = "Number of Responses") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"
  )

### Risky behaviour:

# Risky Behaviors - Long format
risky_behaviors <- households_unique %>%
  select(starts_with("CopingMechanism2__")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "risky", values_to = "count") %>%
  filter(count > 0)

# Relabel
risky_behaviors_labels <- c(
  "CopingMechanism2__beg" = "Begging",
  "CopingMechanism2__foodscrap" = "Collect food scraps",
  "CopingMechanism2__sendchild" = "Children sent to other families",
  "CopingMechanism2__workchild" = "Child labor",
  "CopingMechanism2__survivalsex" = "Survival sex / prostitution",
  "CopingMechanism2__childmarriage" = "Child marriage",
  "CopingMechanism2__none" = "None",
  "CopingMechanism2__notell" = "Prefer not to say",
  "CopingMechanism2__other" = "Other"
)

risky_behaviors$risky <- recode(risky_behaviors$risky, !!!risky_behaviors_labels)
risky_behaviors <- risky_behaviors %>% arrange(desc(count))

# Plot
ggplot(risky_behaviors, aes(x = reorder(risky, count), y = count, fill = risky)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Risky or Harmful Coping Strategies", x = "Risky Behavior", y = "Number of Responses") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", color = "darkred"),
    legend.position = "none"
  )

### Meals per day

library(ggplot2)
library(dplyr)
library(stringr)

# Meals per day summary
meals_summary <- households_unique %>%
  count(mealsperday) %>%
  filter(!is.na(mealsperday) & mealsperday != "") %>%
  mutate(
    # Clean meal names
    mealsperday = case_when(
      mealsperday == "oneORless" ~ "One or less meals",
      mealsperday == "twoORmore" ~ "Two or more meals",
      mealsperday == "threeOrmore" ~ "Three or more meals",
      TRUE ~ mealsperday
    ),
    percentage = n / sum(n) * 100,
    label = paste0(round(percentage), "%")
  ) %>%
  arrange(desc(mealsperday))

# Plot
ggplot(meals_summary, aes(x = 2, y = n, fill = mealsperday)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Slight margin for donut effect
  theme_void() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Number of Meals Per Day",
    fill = "Meals per day"  # Legend title
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", color = "black")
  )



# -------------------------------------------------------------------------------
##### HOUSEHOLD - DISAGGREGATED BY SEX
# -------------------------------------------------------------------------------


# -- Set up progres_Sex labeling
households_unique <- households_unique %>%
  mutate(
    progres_Sex = case_when(
      progres_Sex == "male" ~ "Male",
      progres_Sex == "female" ~ "Female",
      is.na(progres_Sex) | progres_Sex == "" ~ "Unknown",
      TRUE ~ progres_Sex
    )
  )

# --- 1. Specific Needs ---

specific_needs_data <- households_unique %>%
  select(id, progres_Sex, starts_with("SpecificNeeds__")) %>%
  pivot_longer(
    cols = starts_with("SpecificNeeds__"),
    names_to = "specific_need",
    values_to = "value"
  ) %>%
  filter(value == 1)

specific_needs_labels <- c(
  "SpecificNeeds__disability" = "Disability",
  "SpecificNeeds__treatment" = "Chronic condition (no treatment)",
  "SpecificNeeds__treatmentaccess" = "Chronic condition (with treatment)",
  "SpecificNeeds__unacompanied" = "Unaccompanied minor",
  "SpecificNeeds__elderly" = "Elderly (60+)",
  "SpecificNeeds__pregnant" = "Pregnant woman",
  "SpecificNeeds__lactating" = "Lactating woman",
  "SpecificNeeds__singlehead" = "Single parent",
  "SpecificNeeds__victimsurvivor" = "Survivor of violence",
  "SpecificNeeds__lgtbi" = "LGTBIQ",
  "SpecificNeeds__none" = "None"
)

specific_needs_summary <- specific_needs_data %>%
  group_by(progres_Sex, specific_need) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(label = specific_needs_labels[specific_need])

ggplot(specific_needs_summary, aes(x = reorder(label, count), y = count, fill = progres_Sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(
    title = "Specific Needs in Households with Children by Sex",
    x = "", y = "Number of Households",
    fill = "Sex"
  ) +
  theme_minimal()

# --- 2. Disability types ---

disabilities_data <- households_unique %>%
  select(id, progres_Sex, starts_with("disabilitytype__")) %>%
  pivot_longer(
    cols = starts_with("disabilitytype__"),
    names_to = "disability_type",
    values_to = "value"
  ) %>%
  filter(value == 1)

disabilities_labels <- c(
  "disabilitytype__visual" = "Visual Disability",
  "disabilitytype__hearing" = "Hearing Disability",
  "disabilitytype__verbal" = "Verbal Disability",
  "disabilitytype__multi" = "Multiple Disabilities",
  "disabilitytype__physical" = "Physical disability",
  "disabilitytype__mentalcog" = "Intellectual / developmental disability",
  "disabilitytype__mentalemopsy" = "Mental health condition - emotional / psychological",
  "Disabilities__sensory" = "Sensory disability"
)

disabilities_summary <- disabilities_data %>%
  group_by(progres_Sex, disability_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(label = disabilities_labels[disability_type])

ggplot(disabilities_summary, aes(x = reorder(label, count), y = count, fill = progres_Sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(
    title = "Types of Disabilities in Households with Children by Sex",
    x = "", y = "Number of Households",
    fill = "Sex"
  ) +
  theme_minimal()

# --- 5. Problems accessing health services ---

health_access_data <- households_unique %>%
  select(id, progres_Sex, starts_with("HACC04__")) %>%
  pivot_longer(
    cols = starts_with("HACC04__"),
    names_to = "access_problem",
    values_to = "value"
  ) %>%
  filter(value == 1)

health_access_labels <- c(
  "HACC04__1"	= "Lack of money",
  "HACC04__2"	= "No medical personnel available",
  "HACC04__3"	= "Turned away because facility was full",
  "HACC04__4"	= "Turned away because facility was closed",
  "HACC04__5"	= "Hospital/Clinic not having enough supplies or tests",
  "HACC04__6"	= "Health facility is too far",
  "HACC04__7"	= "Fear of contracting a communicable disease (e.g. COVID-19)",
  "HACC04__8"	= "Lockdown/Travel restrictions",
  "HACC04__9"	= "Too Far/transport issues",
  "HACC04__10" = "Health facility is destroyed",
  "HACC04__11" = "Did not have health insurance",
  "HACC04__96" =	"Other"
)

health_access_summary <- health_access_data %>%
  group_by(progres_Sex, access_problem) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(label = health_access_labels[access_problem])

ggplot(health_access_summary, aes(x = reorder(label, count), y = count, fill = progres_Sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(
    title = "Problems Accessing Health Services by Sex",
    x = "", y = "Number of Households",
    fill = "Sex"
  ) +
  theme_minimal()
# 
# library(ggplot2)
# library(dplyr)
# library(stringr)
# 
# # Relabel progres_Sex first
# households_unique <- households_unique %>%
#   mutate(progres_Sex = case_when(
#     progres_Sex == "male" ~ "Male",
#     progres_Sex == "female" ~ "Female",
#     is.na(progres_Sex) | progres_Sex == "" ~ "Unknown",
#     TRUE ~ progres_Sex
#   ))

### --- Social Benefits ---

social_benefits_cols <- households_unique %>% select(starts_with("SocialBenefitType__")) %>% names()

if (length(social_benefits_cols) > 0) {
  
  social_benefits <- households_unique %>%
    select(progres_Sex, all_of(social_benefits_cols)) %>%
    mutate(across(starts_with("SocialBenefitType__"), ~ as.numeric(.))) %>%
    group_by(progres_Sex) %>%
    summarise(across(starts_with("SocialBenefitType__"), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = starts_with("SocialBenefitType__"), names_to = "benefit", values_to = "count") %>%
    filter(count > 0)
  
  social_benefits_labels <- c(
    "SocialBenefitType__cash" = "Cash subsidy (or prepaid card)",
    "SocialBenefitType__housing" = "Housing or housing payment",
    "SocialBenefitType__foodcard" = "Food card",
    "SocialBenefitType__foodbag" = "Delivery of foods",
    "SocialBenefitType__psych" = "Psychological assistance",
    "SocialBenefitType__med" = "Medical Assistance",
    "SocialBenefitType__relocate" = "Relocation support",
    "SocialBenefitType__hygiene" = "Hygiene kit",
    "SocialBenefitType__other" = "Other",
    "SocialBenefitType__foodration" = "Food ration",
    "SocialBenefitType__foodkit" = "Food kit",
    "SocialBenefitType__pss" = "Psychosocial support",
    "SocialBenefitType__medical" = "Medical assistance",
    "SocialBenefitType__education" = "Equipment to access education",
    "SocialBenefitType__livelihoods" = "Livelihoods training",
    "SocialBenefitType__passage" = "Mobility aids"
  )
  
  social_benefits$benefit <- recode(social_benefits$benefit, !!!social_benefits_labels)
  
  ggplot(social_benefits, aes(x = benefit, y = count, fill = progres_Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Types of Aid or Social Protection Received by Sex", x = "Type of Aid", y = "Number of Responses", fill = "Sex") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", color = "black")
    )
}

### --- Coping Skills ---

coping_skills_cols <- households_unique %>% select(starts_with("CopingMechanism1__")) %>% names()

if (length(coping_skills_cols) > 0) {
  
  coping_skills <- households_unique %>%
    select(progres_Sex, all_of(coping_skills_cols)) %>%
    mutate(across(starts_with("CopingMechanism1__"), ~ as.numeric(.))) %>%
    group_by(progres_Sex) %>%
    summarise(across(starts_with("CopingMechanism1__"), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = starts_with("CopingMechanism1__"), names_to = "coping", values_to = "count") %>%
    filter(count > 0)
  
  coping_skills_labels <- c(
    "CopingMechanism1__limitAdultFood" = "Restrict food consumption (adults for children)",
    "CopingMechanism1__reducedFood" = "Reduced food quantity/quality",
    "CopingMechanism1__borrowMoney" = "Borrow money",
    "CopingMechanism1__workFood" = "Work for goods (not cash)",
    "CopingMechanism1__familySupport" = "Family support",
    "CopingMechanism1__useSavings" = "Use savings",
    "CopingMechanism1__reduceExp" = "Reduce non-essential expenses",
    "CopingMechanism1__sellAssets" = "Sell assets",
    "CopingMechanism1__aidAgency" = "Seek aid from agencies/NGO",
    "CopingMechanism1__aidHost" = "Receive donations",
    "CopingMechanism1__skippedRent" = "Skip rent payments",
    "CopingMechanism1__changedShelter" = "Move to less adequate shelter",
    "CopingMechanism1__none" = "None",
    "CopingMechanism1__notell" = "Prefer not to say",
    "CopingMechanism1__other" = "Other"
  )
  
  coping_skills$coping <- recode(coping_skills$coping, !!!coping_skills_labels)
  
  ggplot(coping_skills, aes(x = coping, y = count, fill = progres_Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Coping Mechanisms to Meet Basic Needs by Sex", x = "Coping Mechanism", y = "Number of Responses", fill = "Sex") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", color = "black")
    )
}

### --- Risky Behavior ---

risky_behaviors_cols <- households_unique %>% select(starts_with("CopingMechanism2__")) %>% names()

if (length(risky_behaviors_cols) > 0) {
  
  risky_behaviors <- households_unique %>%
    select(progres_Sex, all_of(risky_behaviors_cols)) %>%
    mutate(across(starts_with("CopingMechanism2__"), ~ as.numeric(.))) %>%
    group_by(progres_Sex) %>%
    summarise(across(starts_with("CopingMechanism2__"), sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = starts_with("CopingMechanism2__"), names_to = "risky", values_to = "count") %>%
    filter(count > 0)
  
  risky_behaviors_labels <- c(
    "CopingMechanism2__beg" = "Begging",
    "CopingMechanism2__foodscrap" = "Collect food scraps",
    "CopingMechanism2__sendchild" = "Children sent to other families",
    "CopingMechanism2__workchild" = "Child labor",
    "CopingMechanism2__survivalsex" = "Survival sex / prostitution",
    "CopingMechanism2__childmarriage" = "Child marriage",
    "CopingMechanism2__none" = "None",
    "CopingMechanism2__notell" = "Prefer not to say",
    "CopingMechanism2__other" = "Other"
  )
  
  risky_behaviors$risky <- recode(risky_behaviors$risky, !!!risky_behaviors_labels)
  
  ggplot(risky_behaviors, aes(x = risky, y = count, fill = progres_Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Risky or Harmful Coping Strategies by Sex", x = "Risky Behavior", y = "Number of Responses", fill = "Sex") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", color = "darkred")
    )
}

### --- Meals per Day ---

meals_summary <- households_unique %>%
  filter(!is.na(mealsperday) & mealsperday != "") %>%
  mutate(
    mealsperday = case_when(
      mealsperday == "oneORless" ~ "One or less meals",
      mealsperday == "twoORmore" ~ "Two or more meals",
      mealsperday == "threeOrmore" ~ "Three or more meals",
      TRUE ~ mealsperday
    )
  ) %>%
  count(progres_Sex, mealsperday) %>%
  group_by(progres_Sex) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

if (nrow(meals_summary) > 0) {
  ggplot(meals_summary, aes(x = mealsperday, y = n, fill = progres_Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Number of Meals Per Day by Sex",
      x = "Meals per day",
      y = "Number of Responses",
      fill = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", color = "black")
    )
}

