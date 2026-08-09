#07_spatial_dissimilarity.R


####Packages####


library(dplyr)
library(tidyr)
library(tibble)
library(vegan)


####Site Dissimilarity####


#####Create Site x Species Matrix#####

site_species <- lichen_data %>%
  distinct(site_id, lichen_species) %>%
  mutate(presabs = 1) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#####Convert to Matrix#####

site_species_matrix <- site_species %>%
  column_to_rownames("site_id") %>%
  as.matrix()

#####Reorder Site Coordinates#####

site_order <- rownames(site_species_matrix)

site_coords_ordered <- site_coords %>%
  filter(site_id %in% site_order) %>%
  arrange(match(site_id, site_order))

#####Check Ordering#####

identical(site_order, site_coords_ordered$site_id)

#####Ecological Distance Matrix#####

ecological_dist <- vegdist(
  site_species_matrix,
  method = "jaccard",
  binary = TRUE
)

#####Geographic Distance Matrix#####

geo_dist <- dist(
  site_coords_ordered %>%
    select(xcoord, ycoord)
)

#####Combine Distances#####

distance_df <- data.frame(
  ecological_distance = as.vector(ecological_dist),
  geographic_distance = as.vector(geo_dist)
)

head(distance_df)

#####Site Dissimilarity Mantel Test#####

mantel(
  ecological_dist,
  geo_dist,
  permutations = 999
)


####Tree Dissimilarity####


#####Create Tree x Species Matrix#####

tree_species_matrix <- species_pa %>%
  select(tree_id, lichen_species, presabs) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#####Convert to Matrix#####

tree_comm <- tree_species_matrix %>%
  column_to_rownames("tree_id")

#####Ecological Distance Matrix#####

ecological_dist_tree <- vegdist(
  tree_comm,
  method = "jaccard"
)

#####Reorder Tree Coordinates#####

tree_order <- rownames(tree_comm)

tree_coords_ordered <- tree_coords %>%
  filter(tree_id %in% tree_order) %>%
  arrange(match(tree_id, tree_order))

#####Geographic Distance Matrix#####

geo_dist_tree <- dist(
  tree_coords_ordered %>%
    select(xcoord, ycoord)
)

#####Combine Distances#####

tree_decay <- data.frame(
  Geographic = as.vector(geo_dist_tree),
  Ecological = as.vector(ecological_dist_tree)
)

#####Tree Dissimilarity Mantel Test#####

mantel(
  ecological_dist_tree,
  geo_dist_tree,
  permutations = 999
)
