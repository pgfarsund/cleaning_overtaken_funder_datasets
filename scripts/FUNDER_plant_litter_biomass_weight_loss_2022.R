
# fix this before uploading to github and OSF: 
# 1. implement "target" data pipeline tool (see one of Aud's scripts on Funder github)

library(here) # for locating files
library(readxl) # for reading excel sheets
library(tidyverse) # for data manipulation
library(targets) # still haven't figured this one out yet

# load weight before burying:
before_burying <- data.frame(read_xlsx(here("Raw", "FUNDER_raw_beforeburrying_litter_biomass_2021.xlsx"),
  sheet = "clean_sheet", # select correct sheet
  col_types = c( # set correct variable types for each column in the excel sheet
    rep("text", times = 4),
    "numeric",
    "text",
    rep("numeric", times = 3),
    "text",
    rep("numeric", times = 2),
    rep("text", times = 2)
  )
)) %>%
  # correct siteID and blockID variable names
  rename(
    siteID = site
  ) %>%
  # recode the ID of every site
  mutate(
    blockID = paste0(substr(x = siteID, start = 1, stop = 3), block),
    siteID = recode(siteID,
      "Gud" = "Gudmedalen",
      "Lav" = "Lavisdalen",
      "Ram" = "Rambera",
      "Ulv" = "Ulvehaugen",
      "Skj" = "Skjelingahaugen",
      "Alr" = "Alrust",
      "Arh" = "Arhelleren",
      "Fau" = "Fauske",
      "Hog" = "Hogsete",
      "Ovs" = "Ovstedalen",
      "Vik" = "Vikesland",
      "Ves" = "Veskre"
    )
  ) %>%
  # pivot to long format
  pivot_longer(
    cols = c(
      native_forbs, added_forbs,
      native_graminoids, added_graminoids
    ),
    names_to = c(
      "native_or_added",
      "litter_type"
    ),
    names_sep = "_",
    values_to = "weight_before_burying"
  ) %>%
  # filter out rows with 0 grams of litter before burying
  filter(weight_before_burying > 0) %>%
  mutate(weight_before_burying = round(weight_before_burying, 5)) %>%
  # select columns to keep
  select(
    siteID,
    blockID,
    treatment,
    plotID,
    weight_before_burying,
    litter_type,
    native_or_added,
    forb_comments_before,
    graminoid_comments_before
  )

# load weight after burying
after_burying <- data.frame(read_xlsx(here("Raw", "FUNDER_mass_loss_2022.xlsx"),
  sheet = "litter_bags", # select correct sheet
  col_types = c( # set appropriate variable types for each column in the excel sheet
    rep("text", times = 4),
    "date",
    rep("numeric", times = 6),
    rep("text", times = 2)
  )
)) %>%
  # correct siteID and blockID variable names
  rename(
    siteID = site,
  ) %>%
  # recode the ID of every site
  mutate(
    blockID = paste0(substr(x = siteID, start = 1, stop = 3), block),
    siteID = recode(siteID,
      "Gud" = "Gudmedalen",
      "Lav" = "Lavisdalen",
      "Ram" = "Rambera",
      "Ulv" = "Ulvehaugen",
      "Skj" = "Skjelingahaugen",
      "Alr" = "Alrust",
      "Arh" = "Arhelleren",
      "Fau" = "Fauske",
      "Hog" = "Hogsete",
      "Ovs" = "Ovstedalen",
      "Vik" = "Vikesland",
      "Ves" = "Veskre"
    ),
    blockID = as.factor(blockID),
    blockID = sub(".0", "", blockID)
  ) %>%
  # make new columns with forb and graminoid dry weights - tube weights
  mutate(
    forbs = forb_dry_weight_g - forb_Falcon_weight_g,
    graminoids = graminoid_dry_weight_g - graminoid_Falcon_weight_g
  ) %>%
  # pivot to long format
  pivot_longer(
    cols = c(forbs, graminoids),
    names_to = "litter_type",
    values_to = "weight_after_burying"
  ) %>%
  mutate(weight_after_burying = round(weight_after_burying, 5)) %>%
  # select columns to keep
  select(siteID, blockID, treatment, plotID, litter_type, weight_after_burying) -> after_burying

# make final litter dataset
litter <- left_join(
  x = before_burying,
  y = after_burying
) %>%
  # calculate weight loss and  relative weight loss
  mutate(
    weight_loss = round(x = c(weight_before_burying - weight_after_burying), 5), # make a cloumn for weight loss
    rel_weight_loss = round(x = c(weight_loss / weight_before_burying), 5), # make a column for relative weight loss
    #rel_weight_loss = gsub(-Inf, NA, rel_weight_loss), # replace "-Inf" with NA (converts column to character)
    #rel_weight_loss = as.numeric(rel_weight_loss) # convert column back to numeric
  ) %>%
  # add mean annual summer temperature, mean annual precipition, and dates of retrieval and burial, via left_join by siteID (alphabetically)
  left_join(data.frame(
    siteID = c(
      "Alrust",
      "Arhelleren",
      "Fauske",
      "Gudmedalen",
      "Hogsete",
      "Lavisdalen",
      "Ovstedalen",
      "Rambera",
      "Skjelingahaugen",
      "Ulvehaugen",
      "Veskre",
      "Vikesland"
    ),
    mean_temp = factor(c(
      8.5, 10.5, 10.5, 6.5, 8.5, 6.5,
      10.5, 8.5, 6.5, 6.5, 8.5, 10.5
    )),
    mean_precip = factor(c(
      700, 2100, 700, 2100, 1400, 1400,
      2800, 2100, 2800, 700, 2800, 1400
    )), 
    burial_date = factor(c(
      "2021-08-05", "2021-08-19", "2021-08-04", "2021-08-10", 
      "2021-08-11", "2021-08-12", "2021-08-13", "2021-08-17", 
      "2021-08-16", "2021-08-06", "2021-08-18", "2021-08-09"
    )), 
    retrieval_date = factor(c(
      "2022-08-24", "2022-08-31", "2022-08-25", "2022-09-06", 
      "2022-08-23", "2022-09-05", "2022-08-29", "2022-09-01", 
      "2022-09-08", "2022-09-07", "2022-08-30", "2022-08-22"
    ))
  )) %>%
  # select variables to keep
  select(
    siteID, blockID, treatment, plotID, 
    burial_date, retrieval_date,
    litter_type, native_or_added, rel_weight_loss,
    mean_precip, mean_temp
  ) 

