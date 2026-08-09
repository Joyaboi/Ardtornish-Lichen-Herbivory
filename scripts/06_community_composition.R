#06_community_composition.R


####Packages####


library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)


####Species Community Composition####


#####Create Tree x Species Matrix#####

species_matrix <- species_pa %>%
  select(tree_id, lichen_species, presabs) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#####Convert to Matrix#####

species_comm <- species_matrix %>%
  column_to_rownames("tree_id")

#####Species Community NMDS#####

set.seed(123)

nmds_species <- metaMDS(
  species_comm,
  distance = "jaccard",
  k = 2,
  trymax = 100
)

nmds_species$stress


####Functional Community Composition####


#####Create Tree x Functional Group Matrix#####

morph_matrix <- func_pa %>%
  select(tree_id, functional_group, presabs) %>%
  pivot_wider(
    names_from = functional_group,
    values_from = presabs,
    values_fill = 0
  )

#####Convert to Matrix#####

morph_comm <- morph_matrix %>%
  column_to_rownames("tree_id")

#####Functional Community NMDS#####

set.seed(123)

nmds_morph <- metaMDS(
  morph_comm,
  distance = "jaccard",
  k = 2,
  trymax = 100
)

nmds_morph$stress

#####Prepare Functional NMDS Scores#####

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

#####Functional Community PERMANOVA#####

adonis2(
  morph_comm ~ WHIA,
  data = scores_morph,
  method = "jaccard"
)