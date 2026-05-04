###############################################
# 1. Setup and Data Import
###############################################

# Load the packages we need for data manipulation and visualization
library(tidyverse)

# Read in the two CSV files from the data folder
surveys <- read_csv("data/surveys.csv")
species <- read_csv("data/species.csv")

# Take a quick look at the structure of each table to confirm they loaded correctly
glimpse(surveys)
glimpse(species)

###############################################
# 2. Data Tidying and Cleaning
###############################################

# Clean the surveys table by removing rows with no species ID, combining the
# separate year, month, and day columns into a single date, and converting
# sex and species ID to categorical variables
surveys_clean <- surveys |>
  filter(!is.na(species_id)) |>
  mutate(date = make_date(year, month, day)) |>
  mutate(
    sex        = factor(sex, levels = c("M", "F")),
    species_id = factor(species_id)
  )

# Count how many rows are missing values in key columns so we know
# what we are working with before filtering
surveys_clean |>
  summarise(
    missing_sex             = sum(is.na(sex)),
    missing_hindfoot_length = sum(is.na(hindfoot_length)),
    missing_weight          = sum(is.na(weight))
  )

###############################################
# 3. Joining Tables
###############################################

# Join the cleaned surveys table to the species table so each observation
# has the full species name, genus, and taxa group attached to it
surveys_joined <- surveys_clean |>
  left_join(species, by = "species_id")

# Confirm the join worked and the new columns are present
glimpse(surveys_joined)

###############################################
# 4. Filter to Rodents and Handle Missing Values
###############################################

# Filter down to rodents only since they make up the vast majority of
# observations and are the ecological focus of this dataset
rodents <- surveys_joined |>
  filter(taxa == "Rodent")

# Remove rows missing weight, hindfoot length, or sex since these are
# the key variables we will be analyzing
rodents_clean <- rodents |>
  filter(
    !is.na(weight),
    !is.na(hindfoot_length),
    !is.na(sex)
  )

# Check how many rows remain after cleaning
nrow(rodents_clean)

###############################################
# 5. Data Transformation and Species Labels
###############################################

# Calculate the mean weight and average hindfoot length for each species,
# then rank them by how commonly they appear in the dataset
species_summary <- rodents_clean |>
  group_by(species_id, genus, species) |>
  summarise(
    mean_weight    = mean(weight),
    mean_hindfoot  = mean(hindfoot_length),
    count          = n(),
    .groups        = "drop"
  ) |>
  arrange(desc(count))

species_summary

# Create a clean species label by combining genus and species name into
# a string, and select only the columns we need for analysis
rodents_clean <- rodents_clean |>
  mutate(species_label = str_c(genus, species, sep = " "))

# Check the new column looks correct
rodents_clean |>
  select(genus, species, species_label) |>
  distinct() |>
  arrange(species_label)

# Identify the top 5 most common species to keep the plot readable
top5_species <- rodents_clean |>
  count(species_label, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(species_label)
