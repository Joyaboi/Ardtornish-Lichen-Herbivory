#community comp

####Figures 5a & 5b: Species Community NMDS####

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

#####5a: Species Community NMDS#####

#Create tree x species matrix

species_matrix <- species_pa %>%
  select(tree_id, lichen_species, presabs) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#Convert to a matrix

species_comm <- species_matrix %>%
  column_to_rownames("tree_id")

#Run NMDS

set.seed(123)

nmds_species <- metaMDS(
  species_comm,
  distance = "jaccard",
  k = 2,
  trymax = 100
)

####Figure 8: Functional NMDS####

library(vegan)
library(ggplot2)
library(dplyr)

set.seed(123)

# Tree × morphotype presence/absence matrix
morph_matrix <- func_pa %>%
  select(tree_id, functional_group, presabs) %>%
  pivot_wider(
    names_from = functional_group,
    values_from = presabs,
    values_fill = 0
  )

# Community matrix for vegan
morph_comm <- morph_matrix %>%
  column_to_rownames("tree_id")

nmds_morph <- metaMDS(
  morph_comm,
  distance = "jaccard",
  k = 2,
  trymax = 100
)

nmds_morph$stress

scores_morph <- as.data.frame(
  scores(nmds_morph, display = "sites")
)

scores_morph$tree_id <- rownames(scores_morph)

scores_morph <- scores_morph %>%
  left_join(
    lichen_data %>%
      distinct(
        tree_id,
        WHIA,
        site_id,
        tree_species
      ),
    by = "tree_id"
  )

scores_morph$WHIA <- factor(
  scores_morph$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

adonis2(
  morph_comm ~ WHIA,
  data = scores_morph,
  method = "jaccard"
)