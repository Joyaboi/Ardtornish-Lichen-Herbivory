#01_data_preparation.R


####Packages####


library(dplyr)
library(tidyr)
library(ggplot2)


####Open & Process Data####


#####Open and Check CSV's#####

sites <- read.csv("data/raw/15_sites.csv")
head(sites)

trees <- read.csv("data/raw/15_trees.csv")
head(trees)

quads <- read.csv("data/raw/15_quadrats.csv")
head(quads)

lichens <- read.csv("data/raw/15_lichens.csv")
head(lichens)

site_coords <- read.csv("data/raw/site_coordinates.csv")
head(site_coords)

tree_coords <- read.csv("data/raw/tree_coordinates.csv")
head(tree_coords)

#####Clean Lichen Data#####

lichens <- lichens %>%
  rename(functional_group = morphotypes) %>%
  select(
    -any_of(c("morphotype", "photobiont"))
  )

lichens <- lichens %>%
  separate(
    functional_group,
    into = c("morphotype", "photobiont"),
    sep = "_(?=[^_]+$)",
    remove = FALSE
  )

lichens <- lichens %>%
  mutate(
    morphotype = factor(
      morphotype,
      levels = c(
        "crustose",
        "foliose",
        "fruticose",
        "leprose",
        "squamulose"
      )
    ),
    photobiont = factor(
      photobiont,
      levels = c(
        "chlorolichen",
        "cyanolichen",
        "unknown"
      )
    )
  )

#####Join Datasets#####

lichen_data <- lichens %>%
  left_join(
    quads %>% select(quadrat_id, tree_id, aspect),
    by = "quadrat_id"
  ) %>%
  left_join(
    trees %>% select(tree_id, site_id, tree_species, cbh_cm, bark_roughness_class),
    by = "tree_id"
  ) %>%
  left_join(
    sites %>% select(site_id, WHIA, avg_canopy_openness),
    by = "site_id"
  )

lichen_data <- lichen_data %>%
  rename(
    rainforest_indicator = rainforest_indicators_rainforest_indicator
  )

head(lichen_data)

#####Process Variables#####

#Convert cbh to dbh
lichen_data <- lichen_data %>%
  mutate(dbh_cm = cbh_cm / pi)

#Make DBH and Canopy continuous
lichen_data <- lichen_data %>%
  mutate(
    dbh_scaled = as.numeric(scale(dbh_cm)),
    canopy_scaled = as.numeric(scale(avg_canopy_openness))
  )

#Reorder herbivory levels
lichen_data <- lichen_data %>%
  mutate(
    WHIA = factor(
      WHIA,
      levels = c(
        "low_impact",
        "medium_impact",
        "high_impact"
      )
    )
  )


####Check Datasets####


#Make sure table join worked
sum(is.na(lichen_data$tree_id))
sum(is.na(lichen_data$site_id))
sum(is.na(lichen_data$tree_species))
sum(is.na(lichen_data$WHIA))

#All should return zero

#####Check for Missing Data#####

#Number of quadrats per tree
tree_quadrat_check <- lichen_data %>%
  distinct(tree_id, quadrat_id) %>%
  count(tree_id, name = "n_quadrats") %>%
  filter(n_quadrats != 4)

tree_quadrat_check

#INN2_TW_QN has no lichens so that's okay

#Number of trees per site
site_tree_check <- lichen_data %>%
  distinct(site_id, tree_id) %>%
  count(site_id, name = "n_trees") %>%
  filter(n_trees != 4)

site_tree_check

#Should return zero

#Number of trees per WHIA Category
tree_WHIA <- lichen_data %>%
  distinct(tree_id, WHIA) %>%
  count(WHIA)

ggplot(tree_WHIA, aes(WHIA, n)) +
  geom_col() +
  labs(
    x = "WHIA category",
    y = "Number of trees"
  ) +
  theme_classic()

#Each category should have 20 trees each

#####Quadrats Without Lichens#####

tree_list <- lichen_data %>%
  distinct(site_id, tree_id)

expected_quadrats <- tree_list %>%
  tidyr::expand_grid(
    quadrat_position = c("QE", "QW", "QS", "QN")
  ) %>%
  mutate(quadrat_id = paste(tree_id, quadrat_position, sep = "_"))

observed_quadrats <- lichen_data %>%
  distinct(quadrat_id)

missing_quadrats <- expected_quadrats %>%
  anti_join(observed_quadrats, by = "quadrat_id")

missing_quadrats

#Again, INN2_TW_QN just didn't have any lichens so that's alright


####Save Processed Dataset####


write.csv(
  lichen_data,
  "data/processed/lichen_data.csv",
  row.names = FALSE
)