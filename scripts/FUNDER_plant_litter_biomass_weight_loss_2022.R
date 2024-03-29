library(here) # for locating files
library(readxl) # for reading excel sheets
library(tidyverse) # for data manipulation

# load weight before burying:
before_burying <- data.frame(read_xlsx(here("Raw", "FUNDER_raw_beforeburrying_litter_biomass_2021.xlsx"),
  sheet = "clean_sheet", # select correct sheet
  col_types = c( # set appropriate variable types for ecah column in the excel sheet
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
    siteID = site,
    blockID = block
  ) %>%
  # recode the ID of every site
  mutate(
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
  # select columns to keep
  select(
    siteID,
    blockID,
    treatment,
    plotID,
    weight_before_burying,
    litter_type,
    native_or_added
  )

# load weight after burying
data.frame(read_xlsx(here("Raw", "FUNDER_mass_loss_2022.xlsx"),
  sheet = "litter_bags", # select correct sheet
  col_types = c( # set appropriate variable types for ecah column in the excel sheet
    rep("text", times = 4),
    "date",
    rep("numeric", times = 6),
    rep("text", times = 2)
  )
)) %>%
  # correct siteID and blockID variable names
  rename(
    siteID = site,
    blockID = block
  ) %>%
  # recode the ID of every site
  mutate(
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
  # discard superfluous columns
 # select(siteID, blockID, treatment, plotID)
#  select(-c(
#    forb_dry_weight_g, forb_Falcon_weight_g,
#    graminoid_dry_weight_g, graminoid_Falcon_weight_g,
#    forb_wet_weight_g, graminoid_wet_weight_g,
#    date_dried,
#    forb_comment, graminoid_comment
#  )) %>%
  # pivot to long format
  pivot_longer(
    cols = c(forbs, graminoids),
    names_to = "litter_type",
    values_to = "weight_after_burying"
  ) -> after_burying

# make a dataset 
litter <- left_join(
  x = before_burying,
  y = after_burying
) %>%
  # calculate weight loss and add relative weight loss as a variable
  mutate(
    weight_loss = weight_before_burying - weight_after_burying,
    rel_weight_loss = weight_loss / weight_before_burying
  ) %>%
  # add mean annual summer temperature and mean annual precipittion via left_join by siteID (alphabetically)
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
      8.5,
      10.5,
      10.5,
      6.5,
      8.5,
      6.5,
      10.5,
      8.5,
      6.5,
      6.5,
      8.5,
      10.5
    )),
    mean_precip = factor(c(
      700,
      2100,
      700,
      2100,
      1400,
      1400,
      2800,
      2100,
      2800,
      700,
      2800,
      1400
    ))
  )) %>% select(siteID, blockID, treatment, plotID, litter_type, native_or_added, 
                )
  # select variables to keep
  select()
