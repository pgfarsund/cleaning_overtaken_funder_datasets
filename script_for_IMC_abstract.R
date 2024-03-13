library(tidyverse)
library(readxl)
library(here)

### load weight before burying
before_burying_raw <- data.frame(read_xlsx(here("Raw", "FUNDER_raw_beforeburrying_litter_biomass_2021.xlsx"), 
                                           sheet = "clean_sheet", 
                                           col_types = c(rep("text", times = 4), 
                                                         "numeric", 
                                                         "text", 
                                                         rep("numeric", times = 3),
                                                         "text", 
                                                         rep("numeric", times = 2),
                                                         rep("text", times = 2)))); str(raw); head(raw)

# fix siteID and other variables, and pivot to long format WITH NATIVE/ADDED:
before_burying_raw %>% 
  rename(siteID = site, blockID = block) %>%
  mutate(siteID = recode(siteID, 
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre"), 
         block = as.factor(blockID), 
         native_graminoids = as.numeric(native_graminoids), 
         graminoids_before_burying = as.numeric(graminoids_before_burying)) %>% 
  mutate(year = "2022", .before = 1) %>% 
  pivot_longer(
    cols = c(native_forbs, added_forbs, 
             native_graminoids, added_graminoids),
    names_to = c("native_or_added", 
                 "plant_functional_group"), 
    names_sep = "_", 
    values_to = "weight_before_burying") %>% 
  select(siteID, 
         blockID, 
         treatment, 
         plotID,
         weight_before_burying, 
         plant_functional_group, 
         native_or_added) -> before_burying_clean; str(before_burying_clean)


# fix siteID and other variables, and pivot to long format WITHOUT NATIVE/ADDED:
before_burying_raw %>% 
  rename(siteID = site, blockID = block) %>%
  mutate(siteID = recode(siteID, 
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre"), 
         block = as.factor(blockID), 
         native_graminoids = as.numeric(native_graminoids), 
         graminoids_before_burying = as.numeric(graminoids_before_burying)) %>% 
  mutate(year = "2022", .before = 1) %>% 
  pivot_longer(
    cols = c(forbs_before_burying, 
             graminoids_before_burying),
    names_to = c("plant_functional_group"), 
    #names_sep = "_", 
    values_to = "weight_before_burying") %>% 
  select(siteID, 
         blockID, 
         treatment, 
         plotID,
         weight_before_burying, 
         plant_functional_group, 
         #native_or_added
         ) %>% 
  mutate(plant_functional_group = sub("_before_burying", "", plant_functional_group))-> before_burying_clean; str(before_burying_clean)

### load weight after burying
after_burying_raw <- data.frame(read_xlsx(here("Raw", "FUNDER_mass_loss_2022.xlsx"), 
                                          sheet = "litter_bags",
                                          col_types = c(rep("text", times = 4), 
                                                        "date",
                                                        rep("numeric", times = 6),
                                                        rep("text", times = 2)))); str(after_burying)
# tidy up site and block IDs
after_burying_raw %>% 
  rename(siteID = site, 
         blockID = block) %>%
  mutate(siteID = recode(siteID, 
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre"), 
         blockID = as.factor(blockID),
         blockID  = sub(".0", "", blockID)) %>% 
  
  # make new columns with forb and graminoid dry weights - tube weights
  mutate(forbs = forb_dry_weight_g - forb_Falcon_weight_g, 
         graminoids = graminoid_dry_weight_g - graminoid_Falcon_weight_g) %>% 
  
  # discard superfluous columns
  select(-c(forb_dry_weight_g, forb_Falcon_weight_g, 
            graminoid_dry_weight_g, graminoid_Falcon_weight_g, 
            forb_wet_weight_g, graminoid_wet_weight_g, 
            date_dried, 
            forb_comment, graminoid_comment)) %>%
  
  # pivot to long format
  pivot_longer(cols = c(forbs, graminoids),
               names_to = "plant_functional_group", 
               values_to = "weight_after_burying") -> after_burying_clean

litter <- left_join(x = before_burying_clean, 
                    y = after_burying_clean) %>% 
  mutate(weight_loss = weight_before_burying - weight_after_burying, 
         rel_weight_loss = weight_loss / weight_before_burying) %>% 
left_join(data.frame(siteID = c("Alrust", "Arhelleren", "Fauske", 
                                "Gudmedalen", "Hogsete", "Lavisdalen", 
                                "Ovstedalen", "Rambera", "Skjelingahaugen",
                                "Ulvehaugen", "Veskre", "Vikesland"),
                     mean_temp = factor(c(8.5, 10.5, 10.5, 
                                   6.5, 8.5, 6.5, 
                                   10.5, 8.5, 6.5, 
                                   6.5, 8.5, 10.5)),
                     mean_precip = factor(c(700, 2100, 700, 
                                     2100, 1400, 1400, 
                                     2800, 2100, 2800,
                                     700, 2800, 1400))))

#
litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = mean_temp, y = rel_weight_loss, fill = mean_temp)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  scale_fill_brewer(palette = "Reds") + 
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab("Mean annual summer temperature") + 
  theme(legend.position = "none") -> temperature

litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = mean_precip, y = rel_weight_loss, fill = mean_precip)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab("Mean annual precipitation") + 
  theme(legend.position = "none") -> precipitation

litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = plant_functional_group, y = rel_weight_loss, fill = plant_functional_group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  scale_fill_manual(values = c("darkorchid1", "chartreuse4")) + 
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab("Litter type") + 
  theme(legend.position = "none") -> litter.type

litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = factor(treatment, levels = c("C", "B", "F", "G", 
                                              "FB", "GB", "GF", "FGB")), 
             y = rel_weight_loss, 
             fill = factor(treatment, levels = c("C", "B", "F", "G", 
                                                 "FB", "GB", "GF", "FGB")))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.2, size = 1.5, stroke = 1, alpha = 0.75) + 
  #scale_fill_brewer(palette = "Greys") + 
  scale_fill_manual(values = c("chartreuse3",
                               rep()))
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab("Litter type") + 
  theme(legend.position = "none")
#

litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x=factor(treatment, levels = c("C", "B", "F", "G", 
                                            "FB", "GB", "GF", "FGB")), 
             fill=plant_functional_group, 
             y=rel_weight_loss)) +
  geom_hline(yintercept = c(0, 0.25, 0.5, 0.75), 
             linetype = 2) +
  #geom_violin(alpha=0.25, linewidth=0.75) + 
  geom_boxplot(aes(fill=plant_functional_group, 
                   x=factor(treatment, levels = c("C", "B", "F", "G", 
                                                  "FB", "GB", "GF", "FGB"))), 
               outlier.shape = NA,
               width=0.5, 
               alpha=0.5,
               linewidth=0.5, 
               position = position_dodge(width = 0.5)) +
  geom_point(shape=21, size=2, alpha=1, position = position_dodge(width = 0.5)) + 
  #geom_jitter(shape=21, size=4, alpha=0.75, width=0.05) +
  scale_fill_manual(values = c("mediumpurple", "chartreuse3")) +
  #scale_fill_brewer() + 
  labs(fill = "litter_type")  +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  theme_bw(base_size = 12) +
  facet_grid(mean_temp~mean_precip) +
  xlab("treatment") -> site.specific.plot; site.specific.plot

#
library(ggpubr)
litter %>% 
  filter(rel_weight_loss > 0) -> litter2

ggdensity(litter2$rel_weight_loss)
ggqqplot(litter2$rel_weight_loss)
hist(litter2$rel_weight_loss); shapiro.test(litter2$rel_weight_loss)

library(fitdistrplus)
hist(litter2$rel_weight_loss)

fit <- fitdist(litter2$rel_weight_loss, distr = "gamma")
plot(fit, breaks = 30)
gof <- gofstat(fit); gof
#
library(glmmTMB)
library(effects)
library(stargazer)

model.dataset <- litter2 %>% 
  dplyr::select(rel_weight_loss, 
                mean_precip, 
                treatment, 
                plant_functional_group, 
                mean_temp)
mod <- glm(rel_weight_loss ~ mean_precip * mean_temp, data = model.dataset) # %>% stargazer(type = "text")

step_mod <- stepAIC(mod, direction = "backward")
stargazer(mod, 
          step_mod,
          type = "text")

plot(allEffects(mod))
plot(allEffects(step_mod))

#calculate McFadden's R-squared for model
stargazer(with(summary(mod), 1 - deviance/null.deviance), type = "text")
stargazer(with(summary(step_mod), 1 - deviance/null.deviance), type = "text")

