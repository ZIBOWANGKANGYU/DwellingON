library(dplyr)
library(magrittr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

# Read in data from the /data folder. The data is big in size, so only read the first few rows

data <- read.csv("data/98-401-X2021021_English_CSV_data.csv", nrows = 2500)

# Dwelling ----

# First. find characterstic ids of basic demographics: population and area
demographic_char_ids <- 1:7
# We are interested in different types of dwelling. 
dwelling_char_ids <- 41:50

# Combine the two sets of characterstic ids
char_ids <- c(dwelling_char_ids, demographic_char_ids)

# We will use the read_csv_chunked function to read in the data in chunks.
# We will use the callback function to process the data in each chunk

# Define a function to filter the data in each chunk

data_filtered <- read_csv_chunked("data/98-401-X2021021_English_CSV_data.csv", 
                                  callback = DataFrameCallback$new(function(x, pos) subset(x, CHARACTERISTIC_ID %in% char_ids)))

# Now we can use the filtered data to do some exploratory data analysis

## EDA
# (1) What are the GEO_LEVELs in the data?
unique(data_filtered$GEO_LEVEL)
# We have "Census subdivision" only

# (2) How many unique DGUIDs are there in the data?
unique(data_filtered$DGUID) %>% length()
# There are 577 unique DGUIDs in the data

# (3) Which census subdivision has the largest population?
data_filtered %>%
  filter(CHARACTERISTIC_NAME == "Population, 2021") %>%
  filter(!is.na(C1_COUNT_TOTAL)) %>%
  filter(C1_COUNT_TOTAL == max(C1_COUNT_TOTAL)) %>%
  select(GEO_NAME)
# TORONTO! Not surprising! 

# (4) Which census subdivision has the largest land?
data_filtered %>%
  filter(CHARACTERISTIC_NAME == "Land area in square kilometres") %>%
  filter(!is.na(C1_COUNT_TOTAL)) %>%
  filter(C1_COUNT_TOTAL == max(C1_COUNT_TOTAL)) %>%
  select(GEO_NAME)

# What about the smallet land?
data_filtered %>%
  filter(CHARACTERISTIC_NAME == "Land area in square kilometres") %>%
  filter(!is.na(C1_COUNT_TOTAL)) %>%
  filter(C1_COUNT_TOTAL == min(C1_COUNT_TOTAL)) %>%
  select(GEO_NAME)

## Data transformation
data_wide <- data_filtered %>%
  select(DGUID, ALT_GEO_CODE, GEO_NAME, DATA_QUALITY_FLAG, CHARACTERISTIC_NAME, C1_COUNT_TOTAL) %>%
  pivot_wider(names_from = CHARACTERISTIC_NAME, values_from = C1_COUNT_TOTAL)


## Data quality 
data_wide <- data_wide %>%
  filter(DATA_QUALITY_FLAG == "00000") %>%
  filter(!is.na(`Population, 2021`)) %>%
  filter(`Population, 2021` > 0)

## Remove outliers
data_wide %>%
  ggplot(aes(x=`Population, 2021`)) + geom_histogram(bins = 500)

  # Difficult to see the very first part of the graph
  data_wide %>%
    ggplot(aes(x=`Population, 2021`)) + geom_histogram(bins = 100) + scale_x_log10()
  # Distribution looks bell-shaped. 

  data_wide <- data_wide %>%
    mutate(log_pop = log10(`Population, 2021`)) %>%
    filter(log_pop > mean(log_pop) - 2 * sd(log_pop))
  # We don't want to remove some of the biggest cities

write_csv(data_wide, "data/data_cleaned.csv")

# Other characteristics ----
# We are interested in the following characteristics:
detail_char_ids <- c(1:7,
                     39, # average age
                     57, # average household size
                     128, # average income
                     1529, # immigrant population
                     1992:1994 # high school education
                     )

# The characteristics above are easy to find. We also want to get the most common mother tongue in each census subdivision.
mother_tongue_char_id <- c(396, 397, 399:717)
# Read in data for these characteristic ids for all census subdivisions
data_mother_tongue <- read_csv_chunked("data/98-401-X2021021_English_CSV_data.csv", 
                                  callback = DataFrameCallback$new(function(x, pos) subset(x, CHARACTERISTIC_ID %in% mother_tongue_char_id)))

# Among all languages, find the most commonly spoken across all census subdivisions
top_lang_char_ids <- data_mother_tongue %>%
  group_by(CHARACTERISTIC_ID) %>%
  summarise(on_total = sum(C1_COUNT_TOTAL, na.rm = TRUE)) %>%
  left_join(., unique(select(data_mother_tongue, CHARACTERISTIC_ID, CHARACTERISTIC_NAME)), by = "CHARACTERISTIC_ID") %>%
  filter(!str_detect(CHARACTERISTIC_NAME, "languages")) %>%
  arrange(-on_total) %>%
  head(10) %>%
  select(CHARACTERISTIC_ID) %>%
  pull()

detail_char_ids <- c(detail_char_ids, top_lang_char_ids)

data_details <- read_csv_chunked("data/98-401-X2021021_English_CSV_data.csv", 
                                  callback = DataFrameCallback$new(function(x, pos) subset(x, CHARACTERISTIC_ID %in% detail_char_ids)))

data_details_wide <- data_details %>%
  filter(DATA_QUALITY_FLAG == "00000") %>%
  select(DGUID, ALT_GEO_CODE, GEO_NAME, CHARACTERISTIC_NAME, C1_COUNT_TOTAL) %>%
  pivot_wider(names_from = CHARACTERISTIC_NAME, values_from = C1_COUNT_TOTAL)

write_csv(data_details_wide, "data/data_details_wide.csv")
