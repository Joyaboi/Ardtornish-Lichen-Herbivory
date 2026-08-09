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
