#02_species_richness.R

####Packages####

library(dplyr)
library(tidyr)
library(lme4)

####Load Data####

#Load processed dataset
lichen_data <- read.csv("data/processed/lichen_data.csv")

#####Create Presence/Absence Table#####

#Create presence/absence table with tree-level covariates
species_pa <- lichen_data %>%
  distinct(tree_id, lichen_species) %>%
  mutate(presabs = 1) %>%
  complete(
    tree_id = unique(lichen_data$tree_id),
    lichen_species = unique(lichen_data$lichen_species),
    fill = list(presabs = 0)
  ) %>%
  left_join(
    lichen_data %>%
      distinct(
        tree_id,
        site_id,
        WHIA,
        tree_species,
        dbh_cm,
        bark_roughness_class,
        avg_canopy_openness
      ),
    by = "tree_id"
  ) %>%
  mutate(
    dbh_scaled = as.numeric(scale(dbh_cm)),
    canopy_scaled = as.numeric(scale(avg_canopy_openness))
  )

####Species Richness Models####


#Does WHIA influence the probability that a given lichen species occurs on a tree?

#WHIA only model
m_rich <- glmer(
  presabs ~ WHIA +
    (1|lichen_species) +
    (1|site_id),
  data = species_pa,
  family = binomial
)

summary(m_rich)

#WHIA + Confounders model
m_rich_cov <- glmer(
  presabs ~ WHIA +
    tree_species +
    dbh_scaled +
    bark_roughness_class +
    canopy_scaled +
    (1 | lichen_species) +
    (1 | site_id),
  data = species_pa,
  family = binomial
)

#####Compare Models#####

AIC(m_rich, m_rich_cov)

anova(
  m_rich,
  m_rich_cov,
  test = "Chisq"
)