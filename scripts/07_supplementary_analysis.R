#07_supplementary_analysis.R


####Packages####


library(dplyr)
library(tidyr)
library(terra)
library(geodata)
library(tibble)
library(iNEXT)
library(betapart)
library(vegan)
library(ggplot2)


####Climate Data####


#Download worldclim data
bio <- worldclim_global(
  var = "bio",
  res = 2.5,
  path = "data"
)

#Convert site coordinates for worldclim
site_coordinates <- vect(
  site_coords,
  geom = c("xcoord", "ycoord"),
  crs = "EPSG:27700"
)

site_coordinates_wgs84 <- project(site_coordinates, crs(bio))

climate <- extract(
  bio[[c(1, 4, 12)]],
  site_coordinates
)

climate

climate_sites <- cbind(
  site_coords,
  climate[, c("wc2.1_2.5m_bio_1",
              "wc2.1_2.5m_bio_4",
              "wc2.1_2.5m_bio_12")]
)

names(climate_sites)[4:6] <- c(
  "bio1_temp",
  "bio4_seasonality",
  "bio12_precip"
)

head(climate_sites)

final_site_ids <- unique(lichen_data$site_id)

length(final_site_ids)
final_site_ids

final_climate <- climate_sites[
  climate_sites$site_id %in% final_site_ids,
]

final_climate

summary_climate <- data.frame(
  variable = c(
    "Annual mean temperature",
    "Temperature seasonality",
    "Annual precipitation"
  ),
  mean = c(
    mean(final_climate$bio1_temp),
    mean(final_climate$bio4_seasonality),
    mean(final_climate$bio12_precip)
  ),
  min = c(
    min(final_climate$bio1_temp),
    min(final_climate$bio4_seasonality),
    min(final_climate$bio12_precip)
  ),
  max = c(
    max(final_climate$bio1_temp),
    max(final_climate$bio4_seasonality),
    max(final_climate$bio12_precip)
  )
)

summary_climate


####Sampling Completeness####


#####Whole Landscape#####

# Species x site incidence matrix
site_incidence <- lichen_data %>%
  distinct(site_id, lichen_species) %>%
  mutate(presence = 1) %>%
  pivot_wider(
    names_from = site_id,
    values_from = presence,
    values_fill = 0
  ) %>%
  column_to_rownames("lichen_species") %>%
  as.matrix()

# iNEXT analysis: observed = 15 sites; extrapolate to 30
site_all_inext <- iNEXT(
  list(All_sites = site_incidence),
  q = 0,
  datatype = "incidence_raw",
  endpoint = 30
)

# Sampling coverage, observed richness, number of uniques, etc.
site_all_inext$DataInfo

# Richness estimates at observed and extrapolated sample sizes
site_all_inext$iNextEst$size_based

# Estimated asymptotic richness
site_all_inext$AsyEst


#####WHIA Categories#####

# One WHIA category per site
site_whia <- lichen_data %>%
  distinct(site_id, WHIA)

# Species x site matrices for each WHIA category
whia_site_mats <- list(
  Low = site_incidence[
    , site_whia$site_id[site_whia$WHIA == "low_impact"],
    drop = FALSE
  ],
  
  Medium = site_incidence[
    , site_whia$site_id[site_whia$WHIA == "medium_impact"],
    drop = FALSE
  ],
  
  High = site_incidence[
    , site_whia$site_id[site_whia$WHIA == "high_impact"],
    drop = FALSE
  ]
)

# Observed = 5 sites/category; extrapolate to 10
whia_site_inext <- iNEXT(
  whia_site_mats,
  q = 0,
  datatype = "incidence_raw",
  endpoint = 10
)

whia_site_inext$DataInfo
whia_site_inext$iNextEst$size_based
whia_site_inext$AsyEst

#####Individual sites#####

# Unique species occurrences on each tree
tree_occ <- lichen_data %>%
  distinct(site_id, tree_id, lichen_species)

site_ids <- sort(unique(tree_occ$site_id))

# Build a species x tree incidence matrix for each site
site_inext_raw <- setNames(
  lapply(site_ids, function(s) {
    
    tree_occ %>%
      filter(site_id == s) %>%
      mutate(presence = 1) %>%
      select(lichen_species, tree_id, presence) %>%
      pivot_wider(
        names_from = tree_id,
        values_from = presence,
        values_fill = 0
      ) %>%
      column_to_rownames("lichen_species") %>%
      as.matrix()
    
  }),
  site_ids
)

site_inext <- iNEXT(
  site_inext_raw,
  q = 0,
  datatype = "incidence_raw",
  endpoint = 8
)

# Observed sample coverage and richness for each site
site_inext$DataInfo

# Asymptotic richness for each site
site_richness_est <- site_inext$AsyEst %>%
  filter(Diversity == "Species richness")

# Richness expected when doubling effort from 4 to 8 trees
site_2x <- site_inext$iNextEst$size_based %>%
  filter(t %in% c(4, 8)) %>%
  select(Assemblage, t, qD) %>%
  pivot_wider(
    names_from = t,
    values_from = qD,
    names_prefix = "t"
  ) %>%
  mutate(gain_4_to_8 = t8 - t4)

# Summary values used in the sampling-completeness table
summary(site_inext$DataInfo$SC)
summary(site_inext$DataInfo$S.obs)
summary(site_2x$t8)
summary(site_2x$gain_4_to_8)
summary(site_richness_est$Estimator)


####Site-Level Beta Diversity: Turnover vs. Nestedness####

# site_incidence is species x sites, so transpose it
# to obtain sites x species
site_comm <- t(site_incidence)

# Multiple-site beta-diversity partition
site_beta <- beta.multi(
  site_comm,
  index.family = "sorensen"
)

site_beta


#####Beta: Distance Decay Among Sites#####


# Coordinates only for sites represented in the lichen analysis
lichen_xy15 <- site_coords %>%
  filter(site_id %in% rownames(site_comm))

# Put coordinates in exactly the same order as site_comm
xy_ordered <- lichen_xy15[
  match(rownames(site_comm), lichen_xy15$site_id),
]

stopifnot(
  all(xy_ordered$site_id == rownames(site_comm))
)

# Geographic distance matrix
geo_dist <- dist(
  xy_ordered[, c("xcoord", "ycoord")]
)

# Pairwise beta-diversity components
site_beta_pair <- beta.pair(
  site_comm,
  index.family = "sorensen"
)

# Total compositional distance
lichen_dist <- site_beta_pair$beta.sor

set.seed(123)

# Total Sorensen distance decay
mantel(
  lichen_dist,
  geo_dist,
  method = "pearson",
  permutations = 9999
)

# Turnover component
mantel(
  site_beta_pair$beta.sim,
  geo_dist,
  method = "pearson",
  permutations = 9999
)

# Nestedness component
mantel(
  site_beta_pair$beta.sne,
  geo_dist,
  method = "pearson",
  permutations = 9999
)

#####Dataframe#####

site_sor_mat <- as.matrix(site_beta_pair$beta.sor)

# Unique site pairs
dist_decay <- as.data.frame(as.table(site_sor_mat)) %>%
  rename(
    site1 = Var1,
    site2 = Var2,
    dissimilarity = Freq
  ) %>%
  mutate(
    site1 = as.character(site1),
    site2 = as.character(site2)
  ) %>%
  filter(site1 < site2) %>%
  
  left_join(
    lichen_xy15 %>% select(site_id, xcoord, ycoord),
    by = c("site1" = "site_id")
  ) %>%
  rename(x1 = xcoord, y1 = ycoord) %>%
  
  left_join(
    lichen_xy15 %>% select(site_id, xcoord, ycoord),
    by = c("site2" = "site_id")
  ) %>%
  rename(x2 = xcoord, y2 = ycoord) %>%
  
  mutate(
    distance_km = sqrt((x2 - x1)^2 + (y2 - y1)^2) / 1000
  )

# Attach WHIA category to each member of the pair
dist_decay <- dist_decay %>%
  left_join(
    site_whia,
    by = c("site1" = "site_id")
  ) %>%
  rename(WHIA1 = WHIA) %>%
  
  left_join(
    site_whia,
    by = c("site2" = "site_id")
  ) %>%
  rename(WHIA2 = WHIA)

dist_decay <- dist_decay %>%
  mutate(
    
    outer = case_when(
      WHIA1 == WHIA2 & WHIA1 == "low_impact"    ~ "Low",
      WHIA1 == WHIA2 & WHIA1 == "medium_impact" ~ "Medium",
      WHIA1 == WHIA2 & WHIA1 == "high_impact"   ~ "High",
      
      WHIA1 %in% c("low_impact", "medium_impact") &
        WHIA2 %in% c("low_impact", "medium_impact") ~ "Low",
      
      WHIA1 %in% c("medium_impact", "high_impact") &
        WHIA2 %in% c("medium_impact", "high_impact") ~ "High",
      
      TRUE ~ "Low"
    ),
    
    inner = case_when(
      WHIA1 == WHIA2 & WHIA1 == "low_impact"    ~ "Low",
      WHIA1 == WHIA2 & WHIA1 == "medium_impact" ~ "Medium",
      WHIA1 == WHIA2 & WHIA1 == "high_impact"   ~ "High",
      
      WHIA1 %in% c("low_impact", "medium_impact") &
        WHIA2 %in% c("low_impact", "medium_impact") ~ "Medium",
      
      WHIA1 %in% c("medium_impact", "high_impact") &
        WHIA2 %in% c("medium_impact", "high_impact") ~ "Medium",
      
      TRUE ~ "High"
    )
  )

dist_model <- lm(
  dissimilarity ~ distance_km,
  data = dist_decay
)

summary(dist_model)


#####Tree-level Composition and Site Structure#####


# Tree x species presence/absence matrix
tree_comm_dist <- tree_occ %>%
  mutate(
    tree_uid = paste(site_id, tree_id, sep = "_"),
    presence = 1
  ) %>%
  select(tree_uid, lichen_species, presence) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presence,
    values_fill = 0
  ) %>%
  column_to_rownames("tree_uid") %>%
  as.matrix()

# Tree metadata in exactly the same order as tree_comm_dist
tree_meta <- tree_occ %>%
  distinct(site_id, tree_id) %>%
  mutate(tree_uid = paste(site_id, tree_id, sep = "_")) %>%
  slice(match(rownames(tree_comm_dist), tree_uid))

stopifnot(all(tree_meta$tree_uid == rownames(tree_comm_dist)))

# Sorensen dissimilarity among trees
tree_sor <- vegdist(
  tree_comm_dist,
  method = "bray",
  binary = TRUE
)

# Formal test of site-level compositional structure
site_perm <- adonis2(
  tree_sor ~ site_id,
  data = tree_meta,
  permutations = 9999
)

site_perm

#####Within-site#####

# Convert tree dissimilarities into unique tree pairs
tree_pairs <- as.data.frame(as.matrix(tree_sor)) %>%
  mutate(tree1 = rownames(.)) %>%
  pivot_longer(
    -tree1,
    names_to = "tree2",
    values_to = "dissimilarity"
  ) %>%
  filter(tree1 < tree2) %>%
  
  left_join(
    tree_meta %>% select(tree_uid, site_id),
    by = c("tree1" = "tree_uid")
  ) %>%
  rename(site1 = site_id) %>%
  
  left_join(
    tree_meta %>% select(tree_uid, site_id),
    by = c("tree2" = "tree_uid")
  ) %>%
  rename(site2 = site_id) %>%
  
  mutate(
    comparison = ifelse(
      site1 == site2,
      "Within site",
      "Between sites"
    )
  )

# Descriptive summary
tree_pairs %>%
  group_by(comparison) %>%
  summarise(
    n = n(),
    mean = mean(dissimilarity),
    median = median(dissimilarity),
    sd = sd(dissimilarity),
    .groups = "drop"
  )