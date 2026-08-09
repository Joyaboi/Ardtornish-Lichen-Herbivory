#05_community_composition.R


####Packages####


library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

####Frequency-weighted community community composition analysis####

#####Tree x Species Abundance Matrix#####

#Create abundance/frequency table with occupancy frequency
species_abund <- lichen_data %>%
  group_by(tree_id, lichen_species) %>%
  summarise(
    frequency = sum(l_frequency),
    .groups = "drop"
  ) %>%
  complete(
    tree_id = unique(lichen_data$tree_id),
    lichen_species = unique(lichen_data$lichen_species),
    fill = list(frequency = 0)
  )

#Convert to tree x species matrix
species_abund_matrix <- species_abund %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = frequency,
    values_fill = 0
  )

#Convert to vegan format
species_abund_comm <- species_abund_matrix %>%
  column_to_rownames("tree_id")

#####Site x Species Abundance Matrix#####

#Aggregate lichen abundance at site level
site_species_abundance <- lichen_data %>%
  group_by(site_id, lichen_species) %>%
  summarise(
    abundance = sum(l_frequency),
    .groups = "drop"
  ) %>%
  complete(
    site_id = unique(lichen_data$site_id),
    lichen_species = unique(lichen_data$lichen_species),
    fill = list(abundance = 0)
  )

#Convert to site x species matrix
site_abundance_matrix <- site_species_abundance %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = abundance,
    values_fill = 0
  )

#Convert to vegan format
site_comm <- site_abundance_matrix %>%
  column_to_rownames("site_id")


#####Site-level Abundance Analysis#####


site_metadata <- lichen_data %>%
  distinct(site_id, WHIA)

table(site_metadata$WHIA)

#####NMDS#####

set.seed(123)

nmds_site_abund <- metaMDS(
  site_comm,
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = FALSE
)

nmds_site_abund$stress

#####PERMANOVA#####

adonis2(
  site_comm ~ WHIA,
  data = site_metadata,
  method = "bray",
  permutations = 999
)

#####Dispersion#####

dispersion <- betadisper(
  vegdist(site_comm, method = "bray"),
  site_metadata$WHIA
)

anova(dispersion)


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