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
  #geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  scale_fill_brewer(palette = "Reds") + 
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab("Degrees celsius") + 
  theme(legend.position = "none") + 
  ggtitle("Mean annual summer temperature") -> temperature; temperature

litter %>% 
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = mean_precip, y = rel_weight_loss, fill = mean_precip)) +
  #geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  scale_fill_brewer(palette = "Blues") + 
  theme_gray() + 
  ylab(" ") +
  xlab("Millimeter") + 
  theme(legend.position = "none") +
  ggtitle("Mean annual precipitation") -> precipitation; precipitation 

litter %>% 
  mutate(n_PFGs = stri_replace_all_regex(treatment,
                                         pattern = c("FGB", "FB", "GB", "GF", "B", "F", "G", "C"), 
                                         replacement = c(3, 2, 2, 2, 1, 1, 1, 0), 
                                         vectorize = FALSE)) %>%
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = plant_functional_group, y = rel_weight_loss, fill = plant_functional_group)) +
  #geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  scale_fill_manual(values = c("darkorchid1", "chartreuse4")) + 
  scale_x_discrete(labels = c("Forbs", "Graminoids")) +
  theme_gray() + 
  ylab("Relative weight loss") +
  xlab(" ") + 
  theme(legend.position = "none") +
  facet_grid(.~n_PFGs) + 
  ggtitle("Plant litter type") -> litter.type; litter.type

litter %>% 
  mutate(n_PFGs = stri_replace_all_regex(treatment,
                                         pattern = c("FGB", "FB", "GB", "GF", "B", "F", "G", "C"), 
                                         replacement = c(3, 2, 2, 2, 1, 1, 1, 0), 
                                         vectorize = FALSE)) %>%
  filter(rel_weight_loss > 0) %>% 
  ggplot(aes(x = n_PFGs, 
             y = rel_weight_loss, 
             fill = n_PFGs)) +
  geom_jitter(shape = 21, width = 0.25, size = 1.5, stroke = 1, alpha = 0.75) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, linewidth = 1.25) + 
  scale_fill_brewer(palette = rev("Greens"), direction = -1) + 
  theme_gray() + 
  ylab(" ") +
  xlab(" ") + 
  theme(legend.position = "none") + 
  ggtitle("Number of PFGs removed") -> pfg; pfg

ggarrange(litter.type, pfg, temperature, precipitation, 
          nrow = 2, ncol = 2) # 6 x 8
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

ggarrange(litter.type, pfg, temperature, precipitation, 
          nrow = 2, ncol = 2)

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

model.dataset %>% 
  mutate(n_PFGs = stri_replace_all_regex(treatment,
                                         pattern = c("FGB", "FB", "GB", "GF", "B", "F", "G", "C"), 
                                         replacement = c(3, 2, 2, 2, 1, 1, 1, 0), 
                                         vectorize = FALSE)) -> model.dataset2

mod <- glm(rel_weight_loss ~ plant_functional_group + n_PFGs + mean_temp + mean_precip, 
           data = model.dataset2) # %>% stargazer(type = "text")



mod <- glm(rel_weight_loss ~ plant_functional_group * mean_precip, 
           data = model.dataset2) # %>% stargazer(type = "text")

step_mod <- stepAIC(mod, direction = "backward")
stargazer(mod, 
          step_mod,
          type = "text")

plot(allEffects(mod))
plot(allEffects(step_mod))

#calculate pseudo R-squared for model
stargazer(with(summary(mod), 1 - deviance/null.deviance), type = "text")
stargazer(with(summary(step_mod), 1 - deviance/null.deviance), type = "text")

#

colnames(path.mod.dataset)
fit <- glm(rel_weight_loss ~ plant_functional_group + treatment + mean_temp * mean_precip, 
           data = model.dataset)
summary(fit)
plot(effects::allEffects(fit))

fit <- glm(rel_weight_loss ~ treatment, 
           data = model.dataset)
summary(fit)
plot(effects::allEffects(fit))






##########


# path analysis
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)
library(stargazer)

# we have to recode our exogenous variables to numeric so that they can be used
# for path analysis: 
path.mod.dataset <- model.dataset %>% 
  mutate(temp = stri_replace_all_regex(mean_temp,
                                       pattern = c("6.5", "8.5", "10.5"), 
                                       replacement = c(6.5, 8.5, 10.5), 
                                       vectorize = FALSE),
         prec = stri_replace_all_regex(mean_precip,
                                       pattern = c("700", "1400", "2100", "2800"), 
                                       replacement = c(700, 1400, 2100, 2800), 
                                       vectorize = FALSE),
         n_PFGs = stri_replace_all_regex(treatment,
                                         pattern = c("FGB", "FB", "GB", "GF", "B", "F", "G", "C"), 
                                         replacement = c(0, 1, 1, 1, 2, 2, 2, 3), 
                                         vectorize = FALSE),
         lit = stri_replace_all_regex(plant_functional_group,
                                      pattern = c("forbs", "graminoids"), 
                                      replacement = c(0, 1), 
                                      vectorize = FALSE), 
         temp = as.numeric(temp), 
         prec = as.numeric(prec),
         n_PFGs = as.numeric(n_PFGs),
         lit = as.numeric(lit)) %>%
  rename(wtl = rel_weight_loss) %>%
  dplyr::select(wtl, temp, prec, n_PFGs, lit)

# check correlations in the data:
cor1 <- cor(path.mod.dataset); cor1
corrplot(cor1)
library(car)
scatterplotMatrix(path.mod.dataset, smooth = FALSE)
# build a path model:
pathmod <- 'wtl ~ lit + n_PFGs + temp + prec'

fit1 <- cfa(pathmod, data = path.mod.dataset) # fit a Confirmatory Factor Analysis (CFA)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

semPaths(fit1, "std", layout = "tree", nCharNodes = 0, style = "lisrel", 
         edge.label.cex = 1, )
