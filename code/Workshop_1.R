# =========================================
# 1.4 Ingesting Data into R ####
# =========================================

# Loading required packages 
library(tidyverse)
library(readxl)

# Import CSV file 
benthic_cover <- read_csv(here::here("data/workshop1/reef_cover_log.csv"))
# Import TSV file 
acoustic_stream <- read_tsv(here::here("data/workshop1/acoustic_telemetry_stream.txt"))
# Import excel file; specific sheet
fisheries_annual <- read_excel(here::here("data/workshop1/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# =========================================
# 1.4.1 Ingesting Data into R; Dealing with messy data ####
# =========================================

# Import survey data (messy data)
mangrove_data <- read_csv(file = here::here("data/workshop1/mangrove_survey_raw.csv"))
# Using arguments to alter import instructions; replacing values with NA
mangrove_data <- read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5, # Skip the first 5 lines of field notes
  na = c(".","NA","9999","ND","blank")) # Convert known text alts to true NA

# =========================================
# 1.5 data frame architectures: Tibbles verses legacy tables ####
# =========================================

# Turning modern tibble into legacy base R data frame 
benthic_cover_df <- as.data.frame(benthic_cover)
# view data frame 
print(benthic_cover_df)
# View alternative data frame; Modern tidyverse  
print(benthic_cover)

# =========================================
# 1.6 Wrangling out ecological signals using Palmer Penguins ####
# =========================================

# Install packages 
# install.packages("palmerpenguins")
# Load packages 
library(palmerpenguins)
data("penguins")

# Examine dataset structure 
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version

# Statistical overview; exploratory summary matrix 
summary(penguins)

# =========================================
# 1.7 Foundational grammar: Slicing, filtering, sorting, and transforming ####
# =========================================
# This sections introduces functionla verbs to shape tables 

# =========================================
# 1.7.1 Isolating attributes with select() ####
# =========================================

# Vertically slice data by name 
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g) 
glimpse(morphology_metrics)

# Selecting by a shorterned string by using colon 
spatial_block <- select(penguins, species:island)

# Discarding certain field using (-)
clean_scientific_fields <- select(penguins, -year)

# =========================================
# 1.7.2 Shifting rows with filter() ####
# =========================================

# Isolate observation to a single group
adelie_cohort <- filter(penguins, species == "Adelie")

# Filtering according to threshold for numerical fields 
# Below target is body mass over the 4500 boundary threshold 
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Filter according to multiple conditional parameters 
biscoe_gentoo <- filter(
  penguins, species == "Gentoo" & 
    island == "Biscoe")

#  Matching muliple targets 
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

# =========================================
# 1.7.3 Ordering sequences with arrange() ####
# =========================================

# Sort by ascending 
lightest_first <- arrange(penguins, body_mass_g)

# Sort by descending via desc()
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Nested sorting (by group), sorted by descending length 
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))

# =========================================
# 1.7.4 Introducing pipes (|>) ####
# =========================================
# Instead of creating variables for another string, using pipe passes data through transformations in a single linear flow.

# =========================================
# 1.7.5 Computing new attributes with mutate() ####
# =========================================

# Calculate morphological ratio 
penguin_ratios <- penguins |>
  mutate(body_mass_kg = body_mass_g / 1000, # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm # Bill ratio
  )
glimpse(penguin_ratios)

# =========================================
# 1.8 Data aggregation and ecological summarisation ####
# =========================================
# Viewing the data as a whole to identify the story such as allometry
# Achieved by using both group_by() and summarise()
# Groups according to interest then summaerising these groups 

# Grouping by species 
grouped_penguins <- group_by(penguins, species)
# Generating summary metrix from these groups 
species_mass_summary <- summarise(
  grouped_penguins,
  mean_mass_g = mean(body_mass_g)
)
# Delearing missing values to be dropped using na.rm = True
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(), # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE), # Calculate mean ignoring missing cells
    sd_mass_g = sd(body_mass_g, na.rm = TRUE) # Standard deviation calculation
  )

# =========================================
# 1.9 Intergrating grammar with visual diagnostics in qmd ####
# =========================================
