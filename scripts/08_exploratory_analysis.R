#08_exploratory_analysis.R

####Packages####

library(dplyr)
library(tidyr)

####Specific Morphotype Models####

#####Create Photobiont Presence/Absence Matrix#####

photo_pa <- lichen_data %>%
  distinct(tree_id, site_id, WHIA, photobiont) %>%
  filter(!is.na(photobiont)) %>%
  mutate(presabs = 1) %>%
  complete(
    tree_id,
    photobiont,
    fill = list(presabs = 0)
  ) %>%
  left_join(
    lichen_data %>%
      distinct(tree_id, site_id, WHIA),
    by = "tree_id"
  )

photo_pa <- photo_pa %>%
  select(
    tree_id,
    site_id = site_id.y,
    WHIA = WHIA.y,
    photobiont,
    presabs
  )

#####Create Morphotype Presence/Absence Matrix#####

morph_pa <- lichen_data %>%
  distinct(tree_id, site_id, WHIA, morphotype) %>%
  filter(!is.na(morphotype)) %>%
  mutate(presabs = 1) %>%
  complete(
    tree_id,
    morphotype,
    fill = list(presabs = 0)
  ) %>%
  left_join(
    lichen_data %>%
      distinct(tree_id, site_id, WHIA),
    by = "tree_id"
  )

morph_pa <- morph_pa %>%
  select(
    tree_id,
    site_id = site_id.y,
    WHIA = WHIA.y,
    morphotype,
    presabs
  )

####Tree Size & Lichen Richness####

#####Create Tree Richness Dataset#####

tree_richness <- lichen_data %>%
  group_by(tree_id) %>%
  summarise(
    richness = n_distinct(lichen_species),
    site_id = first(site_id),
    WHIA = first(WHIA),
    tree_species = first(tree_species),
    dbh_scaled = first(dbh_scaled),
    bark_roughness_class = first(bark_roughness_class),
    canopy_scaled = first(canopy_scaled),
    .groups = "drop"
  ) %>%
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

#####Create Pretty Tree Species Names#####

pretty_tree_species <- function(x) {
  sapply(x, function(i) {
    parts <- strsplit(i, "_")[[1]]
    
    if(length(parts) >= 2){
      paste(
        tools::toTitleCase(parts[1]),
        tolower(parts[2])
      )
    } else {
      tools::toTitleCase(gsub("_", " ", i))
    }
  })
}

pretty_tree_species_labels <- function(x) {
  paste0(
    "italic('",
    pretty_tree_species(x),
    "')"
  )
}

#####Create Tree Species Colours#####

tree_species_cols <- scales::hue_pal()(
  length(unique(tree_richness$tree_species))
)

names(tree_species_cols) <- unique(tree_richness$tree_species)
