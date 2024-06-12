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
  mutate(forbs_before_burying = native_forbs + added_forbs, 
         graminoids_before_burying = native_graminoids + added_graminoids) %>% 
  pivot_longer(cols = c(forbs_before_burying, graminoids_before_burying), 
               names_to = "litter_type", 
               values_to = "weight_before_burying") %>% 
  # pivot to long format
  #pivot_longer(
  #  cols = c(
  #    native_forbs, added_forbs,
  #    native_graminoids, added_graminoids
  #  ),
  #  names_to = c(
  #    "native_or_added",
  #    "litter_type"
  #  ),
  #  names_sep = "_",
  #  values_to = "weight_before_burying"
  #) %>%
  # filter out rows with 0 grams of litter before burying
  filter(weight_before_burying > 0) %>%
  mutate(weight_before_burying = round(weight_before_burying, 5), 
         litter_type = sub("_before_burying", "", litter_type), 
         added_forbs = ifelse(added_forbs > 0, "TRUE", added_forbs),
         added_forbs = ifelse(added_forbs <= 0, "FALSE", added_forbs),
         added_graminoids = ifelse(added_graminoids > 0, "TRUE", added_graminoids),
         added_graminoids = ifelse(added_graminoids <= 0, "FALSE", added_graminoids)) %>%
  # select columns to keep
  select(
    siteID,
    blockID,
    treatment,
    plotID,
    weight_before_burying,
    litter_type,
    added_forbs, # include?
    added_graminoids, # include? 
    forb_comments_before,
    graminoid_comments_before
  )

before_burying %>% 
  ggplot(aes(x=weight_before_burying)) + 
  geom_histogram() + 
  facet_grid(.~litter_type)

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
litter_no_native_added <- left_join(
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
    #mean_temp = factor(c(
    #  8.5, 10.5, 10.5, 6.5, 8.5, 6.5,
    #  10.5, 8.5, 6.5, 6.5, 8.5, 10.5
    #)),
    #mean_precip = factor(c(
    #  700, 2100, 700, 2100, 1400, 1400,
    #  2800, 2100, 2800, 700, 2800, 1400
    #)), 
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
    litter_type, #native_or_added, 
    rel_weight_loss,
    #mean_precip, mean_temp, 
    added_forbs, added_graminoids
  ) 

litter_no_native_added %>% 
  filter(rel_weight_loss!="NA") %>% 
  ggplot(aes(x=rel_weight_loss)) + 
  geom_histogram()


litter_no_native_added %>% 
  ggplot(aes(x=added_t_f)) + 
  geom_histogram()


litter %>% 
  #filter(rel_weight_loss!="NA", 
  #       rel_weight_loss>0) %>% 
  mutate(npfg_removed = as.numeric(stri_replace_all_regex(treatment,
                                         pattern = c("FGB", "FB", "GB", "GF", 
                                                     "B", "F", "G", "C"), 
                                         replacement = c(3, 2, 2, 2, 
                                                         1, 1, 1, 0), 
                                         vectorize = FALSE))) %>%
  ggplot(aes(npfg_removed, rel_weight_loss, color=siteID, fill=siteID)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(mean_temp~mean_precip)


litter_no_native_added %>% 
  filter(rel_weight_loss!="NA", 
         rel_weight_loss>0) %>% 
  mutate(npfg_removed = as.numeric(stri_replace_all_regex(treatment,
                                                          pattern = c("FGB", "FB", "GB", "GF", 
                                                                      "B", "F", "G", "C"), 
                                                          replacement = c(3, 2, 2, 2, 
                                                                          1, 1, 1, 0), 
                                                          vectorize = FALSE))) %>%
  ggplot(aes(npfg_removed, rel_weight_loss, color=siteID, fill=siteID)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  geom_smooth(method = "lm") +
  facet_grid(mean_temp~mean_precip)


litter_no_native_added %>% 
  filter(rel_weight_loss>0) %>%
  pivot_longer(cols = c(added_forbs, added_graminoids), 
               values_to = "added_t_f") %>% 
  ggplot(aes(x=added_t_f, y=rel_weight_loss, color=added_t_f)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(height = 0, width = 0.3)) +
  facet_grid(.~siteID)


litter_no_native_added %>% 
  filter(rel_weight_loss>0) %>%
  pivot_longer(cols = c(added_forbs, added_graminoids), 
               values_to = "added_t_f", 
               names_to = "added_litter_type") %>% View()
  ggplot(aes(x=added_t_f, y=rel_weight_loss)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(width = 0.4, height = 0))
#

litter_no_native_added %>% 
  filter(rel_weight_loss>0) %>%
  pivot_longer(cols = c(added_forbs, added_graminoids), 
               values_to = "added_t_f") %>%
  filter(rel_weight_loss!="NA", 
         rel_weight_loss>0) %>% 
  mutate(npfg_removed = as.numeric(stri_replace_all_regex(treatment,
                                                          pattern = c("FGB", "FB", "GB", "GF", 
                                                                      "B", "F", "G", "C"), 
                                                          replacement = c(3, 2, 2, 2, 
                                                                          1, 1, 1, 0), 
                                                          vectorize = FALSE))) %>% 
  ggplot(aes(x=npfg_removed, y=rel_weight_loss, color=added_t_f)) + 
  geom_point(position = position_jitter(width = 0.2)) + 
  geom_smooth(method = "lm") + 
  facet_grid(mean_precip ~ mean_temp)


remotes::install_github("audhalbritter/dataDocumentation")
library(dataDocumentation)

data(funder)
funcabization(dat = funder, convert_to = "FunCaB")

litter_funcab_format <- funcabization(dat = litter_no_native_added, convert_to = "FunCaB")
write.csv(litter_funcab_format, here("Clean/FUNDER_clean_plant_litter_decomposition.csv"))
