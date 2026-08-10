#08_figures.R

# Just to note, some of the figures in the Dissertation are bespoke graphics
# and thus not included in R code
# you can find these figures in:
# Diss_Repo/figures


####Packages####


library(patchwork)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggeffects)


####Save Figures: On or OFF####


save_figures <- TRUE

#True = Figures will save

#False = Figures will not save


####Helper Functions####


#Format species names
pretty_species <- function(x) {
  sapply(x, function(i) {
    parts <- strsplit(i, "_")[[1]]
    
    if(length(parts) >= 2){
      species <- ifelse(
        parts[2] == "sp",
        paste0(parts[1], " sp."),
        paste(
          tools::toTitleCase(parts[1]),
          tolower(parts[2])
        )
      )
      species
    } else{
      tools::toTitleCase(gsub("_", " ", i))
    }
  })
}

#Format tree species names
pretty_tree_species <- function(x) {
  sapply(x, function(i) {
    parts <- strsplit(i, "_")[[1]]
    
    if(length(parts) >= 2){
      if(parts[2] == "sp"){
        paste0(
          tools::toTitleCase(parts[1]),
          " sp."
        )
      } else {
        paste(
          tools::toTitleCase(parts[1]),
          tolower(parts[2])
        )
      }
    } else {
      tools::toTitleCase(gsub("_", " ", i))
    }
  })
}


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


####Figures 7a & 7b: Lichen species obs frequency####


#Create WHIA ordered factor
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

#####7a: Rank Abundance Curve#####

#Calculate obs per species
species_obs <- lichen_data %>%
  count(lichen_species, name = "observations") %>%
  arrange(desc(observations)) %>%
  mutate(
    species_rank = row_number()
  )

head(species_obs)

#Identify top species
top_species <- lichen_data %>%
  count(lichen_species, sort = TRUE) %>%
  slice_head(n = 11) %>%
  pull(lichen_species)

#Create grouped composition
species_comp <- lichen_data %>%
  mutate(
    species_group = ifelse(
      lichen_species %in% top_species,
      lichen_species,
      "Other"
    )
  ) %>%
  count(WHIA, species_group) %>%
  group_by(WHIA) %>%
  mutate(
    proportion = n / sum(n)
  )

species_comp <- species_comp %>%
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

species_comp <- species_comp %>%
  mutate(
    species_group = ifelse(
      species_group %in% top_species,
      species_group,
      "Other"
    ),
    species_group = factor(
      species_group,
      levels = c(top_species, "Other")
    )
  )

#Color code
species_levels <- levels(species_comp$species_group)

species_cols <- c(
  "#D95F5F",  # muted red
  "#5B8DB8",  # muted blue
  "#6BAF6B",  # muted green
  "#9B7BB8",  # muted purple
  "#E6A15C",  # muted orange
  "#D8C95A",  # muted yellow
  "#A67C52",  # muted brown
  "#D98FB3",  # muted pink
  "#7FA6C9",  # slate blue
  "#B0B888",  # green-grey
  "#E07B39",  # burnt orange
  "#4F9D9D",  # teal
  "#8DBF55",  # olive green
  "#5FA8A8",  # muted turquoise
  "#C76D9B",  # raspberry
  "#8064A2",  # violet
  "#666666"   # dark grey
)

names(species_cols) <- species_levels

species_cols["Other"] <- "grey70"

#Rank abundance curve
fig7a <- ggplot(
  species_obs,
  aes(
    x = species_rank,
    y = observations,
    colour = "Lichen species"
  )
) +
  geom_line(
    colour = "grey70"
  ) +
  geom_point(
    aes(
      colour = ifelse(
        lichen_species %in% top_species,
        lichen_species,
        "Other"
      )
    ),
    size = 2
  ) +
  scale_colour_manual(
    values = species_cols,
    limits = c(top_species, "Other"),
    guide = "none"
  ) +
  labs(
    x = "Species rank",
    y = "Number of observations",
    colour = "Lichen"
  ) +
  theme_classic(base_size = 14)

#Label the most common species

#####7b: Species composition by WHIA#####

#Plot
fig7b <- ggplot(
  species_comp,
  aes(
    x = WHIA,
    y = proportion,
    fill = species_group
  )
) +
  geom_col(
    colour = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = species_cols,
    labels = function(x) {
      sapply(x, function(i) {
        if(i == "Other") {
          "Other"
        } else {
          parse(text = paste0(
            "italic('",
            pretty_species(i),
            "')"
          ))
        }
      })
    }
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  labs(
    x = NULL,
    y = "Proportion of observations",
    fill = "Lichen species"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 9)
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

#Combine Figures
figure7 <- (fig7a | fig7b) +
  plot_layout(guides = "collect") +
  guides(
    fill = guide_legend(
      nrow = 4,
      byrow = TRUE
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 14)
  )

figure7 <- figure7 +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

figure7

#Save Plot
if (save_figures) {
ggsave(
  filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_07.png",
  plot = figure7 + plot_annotation(tag_levels = "a"),
  width = 24.6,
  height = 14.5,
  units = "cm",
  dpi = 600
)
}


####Figures 8a & 8b: Richness####


#Create tree richness dataset
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

#Tree colours

tree_species_cols <- c(
  "#76C000",  # lime green
  "#AA00FF",  # purple
  "#FF6A00",  # orange
  "#FF1744",  # red
  "#00C853",  # green
  "#FFD600",  # gold
  "#008CFF",   # blue
  "#E6007A"  # magenta
)

names(tree_species_cols) <- unique(tree_richness$tree_species)

#####8a: Richness by WHIA#####

fig8a <- ggplot(
  tree_richness,
  aes(
    x = WHIA,
    y = richness,
    fill = tree_species
  )
) +
  geom_boxplot(
    aes(group = WHIA),
    width = 0.55,
    fill = "grey95",
    colour = "black",
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(width = 0.15),
    size = 2.5,
    alpha = 0.9,
    shape = 21,
    colour = "grey20",
    stroke = 0.75
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  labs(
    x = "WHIA category",
    y = "Lichen species richness",
    fill = "Host tree species"
  ) +
  scale_fill_manual(
    values = tree_species_cols,
    labels = function(x) {
      parse(text = pretty_tree_species_labels(x))
    }
  ) +
  theme_classic(base_size = 14)

#####8b: Predicted species occurrence probability#####

#Set WHIA factor order
species_pa <- species_pa %>%
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

func_pa <- func_pa %>%
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

m_rich <- glmer(
  presabs ~ WHIA +
    (1|lichen_species) +
    (1|site_id),
  data = species_pa,
  family = binomial
)

m_func_rich <- glmer(
  presabs ~ WHIA +
    (1|functional_group) +
    (1|site_id),
  data = func_pa,
  family = binomial
)

#Generate model predictions
species_rich_pred <- ggpredict(
  m_rich,
  terms = "WHIA"
)

species_rich_pred

fig8b <- ggplot(
  species_rich_pred,
  aes(
    x = x,
    y = predicted
  )
) +
  geom_point(
    size = 4
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.1,
    linewidth = 0.8
  ) +
  theme_classic(
    base_size = 14
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  labs(
    x = "WHIA category",
    y = "Predicted occurrence probability"
  )

#Combine into one figure
figure8 <- (fig8a | fig8b) +
  plot_layout(guides = "collect") +
  guides(
    fill = guide_legend(
      nrow = 4,
      byrow = TRUE
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 12)
  )

figure8 <- figure8 +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

figure8

#Save Plot
if (save_figures) {
ggsave(
  filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_08.png",
  plot = figure8 + plot_annotation(tag_levels = "a"),
  width = 24.6,
  height = 14.5,
  units = "cm",
  dpi = 600
)
}

####Figure 9: Species Level Response to WHIA####


# Model:

# presabs ~ WHIA +
# (1|lichen_species) +
# (1|site_id) +
# (1|lichen_species:WHIA) +
# (1|lichen_species:site_id)

# Create predictions for every species across WHIA categories

species_predictions <- ggpredict(
  m_turnover1,
  terms = c("WHIA", "lichen_species"),
  type = "random"
) %>%
  as.data.frame()

# Format WHIA categories

species_predictions$x <- factor(
  species_predictions$x,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  ),
  labels = c(
    "Low Impact",
    "Medium Impact",
    "High Impact"
  )
)

head(species_predictions)

# Calculate mean predicted occurrence across species

species_mean <- species_predictions %>%
  group_by(x) %>%
  summarise(
    mean_prediction = mean(predicted),
    .groups = "drop"
  )

species_mean$x <- factor(
  species_mean$x,
  levels = c(
    "Low Impact",
    "Medium Impact",
    "High Impact"
  )
)

# Plot

figure9 <- ggplot(
  species_predictions,
  aes(
    x = x,
    y = predicted,
    group = group
  )
) +
  geom_line(
    colour = "grey70",
    linewidth = 0.4,
    alpha = 1.0
  ) +
  geom_line(
    data = species_mean,
    aes(
      x = x,
      y = mean_prediction,
      group = 1
    ),
    colour = "#C0392B",
    linewidth = 1.5
  ) +
  scale_x_discrete(
    expand = c(0.05, 0)
  ) +
  labs(
    x = "WHIA category",
    y = "Predicted probability of species occurrence",
    title = NULL
  ) +
  theme_classic(
    base_size = 15
  ) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )

figure9

# Save Plot

if (save_figures) {
  ggsave(
    filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_09.png",
    plot = figure9,
    width = 24.6,
    height = 14.5,
    units = "cm",
    dpi = 600
  )
}


####Figures 10a & 10b: Species Community NMDS####


#####10a: Species Community NMDS#####

#Create tree x species matrix

species_matrix <- species_pa %>%
  select(
    tree_id,
    lichen_species,
    presabs
  ) %>%
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

#Create plotting dataframe

scores_species <- as.data.frame(
  scores(
    nmds_species,
    display = "sites"
  )
)

scores_species$tree_id <- rownames(scores_species)

scores_species <- scores_species %>%
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

#Order WHIA

scores_species$WHIA <- factor(
  scores_species$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

#Plot

fig10a <- ggplot(
  scores_species,
  aes(
    NMDS1,
    NMDS2,
    colour = WHIA
  )
) +
  geom_point(
    size = 2
  ) +
  stat_ellipse(
    level = 0.95,
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      low_impact = "#3350FF",
      medium_impact = "#D9B300",
      high_impact = "#FC3F3F"
    ),
    labels = c(
      low_impact = "Low Impact",
      medium_impact = "Medium Impact",
      high_impact = "High Impact"
    ),
    name = "WHIA"
  ) +
  theme_classic(
    base_size = 14
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "WHIA"
  ) +
  guides(
    colour = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 13),
    legend.margin = margin(t = -5, r = 2, b = 0, l = 0)
  )


#####10b: NMDS by Site#####

scores_species <- scores_species %>%
  mutate(
    site_group = sub(
      "[0-9]+$",
      "",
      site_id
    )
  )

fig10b <- ggplot(
  scores_species,
  aes(
    NMDS1,
    NMDS2,
    colour = site_group
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  stat_ellipse(
    aes(
      group = site_group
    ),
    type = "t",
    level = 0.95,
    linewidth = 1
  ) +
  scale_colour_brewer(
    palette = "Dark2"
  ) +
  theme_classic(
    base_size = 14
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "Site group"
  ) +
  guides(
    colour = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 13),
    legend.margin = margin(t = -5, r = 2, b = 0, l = 0)
  )


#####Combine Figures#####

figure10 <- (fig10a | fig10b) +
  plot_annotation(
    tag_levels = "a"
  ) &
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

figure10

#Save Plot
if (save_figures) {
  ggsave(
    filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_10.png",
    plot = figure10 + plot_annotation(tag_levels = "a"),
    width = 24.6,
    height = 14.5,
    units = "cm",
    dpi = 600
  )
}


####Figures 11a, 11b, and 11c: Dissimilarity####


#####11a: Site Dissimilarity#####

#Create site-level community matrix

site_species <- lichen_data %>%
  distinct(site_id, lichen_species) %>%
  mutate(presabs = 1) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#Convert to matrix

site_species_matrix <- site_species %>%
  column_to_rownames("site_id") %>%
  as.matrix()

#Save row order

site_order <- rownames(site_species_matrix)

#Reorder coordinates to match matrix

site_coords_ordered <- site_coords %>%
  filter(site_id %in% site_order) %>%
  arrange(match(site_id, site_order))

#Check ordering

identical(
  site_order,
  site_coords_ordered$site_id
)

#Ecological distance matrix

ecological_dist <- vegdist(
  site_species_matrix,
  method = "jaccard",
  binary = TRUE
)

#Geographic distance matrix

geo_dist <- dist(
  site_coords_ordered %>%
    select(xcoord, ycoord)
)

#Combine distances into a dataframe

distance_df <- data.frame(
  ecological_distance = as.vector(ecological_dist),
  geographic_distance = as.vector(geo_dist)
)

#Check

head(distance_df)

#Distance-decay plot

fig11a <- ggplot(
  distance_df,
  aes(
    x = geographic_distance,
    y = ecological_distance
  )
) +
  geom_point(
    size = 2,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "black"
  ) +
  labs(
    x = "Distance between sites (m)",
    y = "Pairwise Jaccard dissimilarity",
    title = NULL
  ) +
  theme_classic(
    base_size = 14
  ) +
  theme(
    axis.title.y = element_text(size = 18)
  )

#####11b: Tree Dissimilarity#####

#Create tree x species matrix

tree_species_matrix <- species_pa %>%
  select(
    tree_id,
    lichen_species,
    presabs
  ) %>%
  pivot_wider(
    names_from = lichen_species,
    values_from = presabs,
    values_fill = 0
  )

#Convert to community matrix

tree_comm <- tree_species_matrix %>%
  column_to_rownames("tree_id")

#Ecological distance matrix

ecological_dist_tree <- vegdist(
  tree_comm,
  method = "jaccard"
)

#Save row order

tree_order <- rownames(tree_comm)

#Reorder coordinates to match matrix

tree_coords_ordered <- tree_coords %>%
  filter(tree_id %in% tree_order) %>%
  arrange(match(tree_id, tree_order))

#Geographic distance matrix

geo_dist_tree <- dist(
  tree_coords_ordered %>%
    select(xcoord, ycoord)
)

#Combine distances into a dataframe

tree_decay <- data.frame(
  Geographic = as.vector(geo_dist_tree),
  Ecological = as.vector(ecological_dist_tree)
)

#Distance-decay plot

fig11b <- ggplot(
  tree_decay,
  aes(
    Geographic,
    Ecological
  )
) +
  geom_point(
    size = 2,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "black"
  ) +
  theme_classic(
    base_size = 14
  ) +
  labs(
    x = "Distance between trees (m)",
    y = NULL,
    title = NULL
  )

#Mantel test

mantel(
  ecological_dist_tree,
  geo_dist_tree,
  permutations = 999
)

#####11c: Tree Pairwise Dissimilarity by WHIA Pairing#####

#Convert ecological distance matrix to dataframe

eco_df <- as.data.frame(
  as.matrix(ecological_dist_tree)
)

eco_df$tree1 <- rownames(eco_df)

#Convert to pairwise format

eco_df <- eco_df %>%
  pivot_longer(
    cols = -tree1,
    names_to = "tree2",
    values_to = "dissimilarity"
  ) %>%
  filter(tree1 < tree2)

#Add WHIA information

tree_meta <- species_pa %>%
  distinct(
    tree_id,
    WHIA
  )

eco_df <- eco_df %>%
  left_join(
    tree_meta,
    by = c("tree1" = "tree_id")
  ) %>%
  rename(
    WHIA1 = WHIA
  ) %>%
  left_join(
    tree_meta,
    by = c("tree2" = "tree_id")
  ) %>%
  rename(
    WHIA2 = WHIA
  )

#Create WHIA pairing categories

eco_df <- eco_df %>%
  mutate(
    Pair = case_when(
      WHIA1 == "low_impact" &
        WHIA2 == "low_impact" ~ "L-L",
      
      WHIA1 == "medium_impact" &
        WHIA2 == "medium_impact" ~ "M-M",
      
      WHIA1 == "high_impact" &
        WHIA2 == "high_impact" ~ "H-H",
      
      WHIA1 != WHIA2 ~ "Between",
      
      TRUE ~ NA_character_
    )
  )

eco_df$Pair <- factor(
  eco_df$Pair,
  levels = c(
    "L-L",
    "M-M",
    "H-H",
    "Between"
  )
)

#Plot

fig11c <- ggplot(
  eco_df,
  aes(
    Pair,
    dissimilarity,
    fill = Pair
  )
) +
  geom_boxplot(
    colour = "black",
    outlier.shape = NA
  ) +
  scale_fill_manual(
    values = c(
      "L-L" = "#3350FF",
      "M-M" = "#D9B300",
      "H-H" = "#FC3F3F",
      "Between" = "grey70"
    ),
    guide = "none"
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.35,
    size = 0.6
  ) +
  theme_classic(
    base_size = 14
  ) +
  labs(
    x = "WHIA comparison",
    y = NULL,
    title = NULL
  )

#####Combine Figure 11#####

figure11 <- (
  fig11a |
    fig11b |
    fig11c
) +
  plot_annotation(
    tag_levels = "a"
  )

figure11

#Save Plot

if (save_figures) {
  ggsave(
    filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_11.png",
    plot = figure11 + plot_annotation(tag_levels = "a"),
    width = 24.6,
    height = 12,
    units = "cm",
    dpi = 600
  )
}


####Figures 12a, 12b, and 12c: Functional Composition####


#####12a: Photobiont Groups#####

func_comp <- lichen_data %>%
  mutate(
    photobiont_group = case_when(
      grepl("cyanolichen", photobiont) ~ "Cyanolichen",
      grepl("chlorolichen", photobiont) ~ "Chlorolichen",
      TRUE ~ "Unknown"
    ),
    photobiont_group = factor(
      photobiont_group,
      levels = c(
        "Cyanolichen",
        "Chlorolichen",
        "Unknown"
      )
    )
  ) %>%
  count(WHIA, photobiont_group) %>%
  group_by(WHIA) %>%
  mutate(
    proportion = n / sum(n)
  )

functional_cols <- c(
  "Cyanolichen" = "#1B9E77",
  "Chlorolichen" = "#FFFF6B",
  "Unknown" = "grey70"
)

fig12a <- ggplot(
  func_comp,
  aes(
    x = WHIA,
    y = proportion,
    fill = photobiont_group
  )
) +
  geom_col(
    colour = "black",
    linewidth = 0.2
  ) +
  labs(
    x = NULL,
    y = "Proportion of observations",
    fill = "Photobiont"
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  guides(
    fill = guide_legend(
      direction = "vertical",
      ncol = 1
    )
  ) +
  scale_fill_manual(
    values = functional_cols
  ) +
  theme_classic(
    base_size = 14
  )

#####12b: Morphotype Composition#####

morph_comp <- lichen_data %>%
  mutate(
    morphotype_simple = case_when(
      grepl("^foliose", morphotype) ~ "Foliose",
      grepl("^crustose", morphotype) ~ "Crustose",
      grepl("^fruticose", morphotype) ~ "Fruticose",
      grepl("^leprose", morphotype) ~ "Leprose",
      grepl("^squamulose", morphotype) ~ "Squamulose",
      TRUE ~ "Unknown"
    ),
    morphotype_simple = factor(
      morphotype_simple,
      levels = c(
        "Crustose",
        "Foliose",
        "Fruticose",
        "Leprose",
        "Squamulose",
        "Unknown"
      )
    )
  ) %>%
  count(WHIA, morphotype_simple) %>%
  group_by(WHIA) %>%
  mutate(
    proportion = n / sum(n)
  )

morphotype_cols <- c(
  "Crustose" = "#54FFF7",
  "Foliose" = "#FF6161",
  "Fruticose" = "#FCCC6D",
  "Leprose" = "#6D6DFC",
  "Squamulose" = "#FF8CFC",
  "Unknown" = "grey70"
)

fig12b <- ggplot(
  morph_comp,
  aes(
    x = WHIA,
    y = proportion,
    fill = morphotype_simple
  )
) +
  geom_col(
    colour = "black",
    linewidth = 0.2
  ) +
  labs(
    x = "WHIA Category",
    y = NULL,
    fill = "Morphotype"
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  guides(
    fill = guide_legend(
      direction = "vertical",
      nrow = 3
    )
  ) +
  scale_fill_manual(
    values = morphotype_cols
  ) +
  theme_classic(
    base_size = 14
  ) +
  theme(
    axis.title.x = element_text(size = 18)
  )

#####12c: Predicted Functional Group Occurrence Probability#####

func_rich_pred <- ggpredict(
  m_func_rich,
  terms = "WHIA"
)

func_rich_pred

fig12c <- ggplot(
  func_rich_pred,
  aes(
    x = x,
    y = predicted
  )
) +
  geom_point(
    size = 4
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.1,
    linewidth = 0.8
  ) +
  theme_classic(
    base_size = 14
  ) +
  scale_x_discrete(
    labels = c(
      low_impact = "Low",
      medium_impact = "Medium",
      high_impact = "High"
    )
  ) +
  labs(
    x = NULL,
    y = "Predicted functional group\noccurrence probability"
  )

fig12c_note <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.5,
    label = stringr::str_wrap(
      "Functional groups defined by lichen morphotype x photobiont type",
      width = 35
    ),
    size = 4,
    hjust = 0.5
  ) +
  theme_void()

fig12a_with_legend <- fig12a +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 13),
    legend.key.size = unit(0.5, "cm"),
    legend.key.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = -10, r = 2, b = 0, l = 0),
    legend.box.margin = margin(t = 5, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0.5, "cm")
  )

fig12b_with_legend <- fig12b +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 13),
    legend.key.spacing.y = unit(0.1, "cm"),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = -10, r = 2, b = 0, l = 0),
    legend.box.margin = margin(t = 5, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0.5, "cm")
  )

fig12c_with_caption <- fig12c +
  labs(
    caption = stringr::str_wrap(
      "*Functional groups are defined by lichen morphotype x photobiont type",
      width = 33
    )
  ) +
  theme(
    plot.caption = element_text(
      size = 13,
      hjust = 0,
      margin = margin(t = -55, l = 22)
    ),
    plot.caption.position = "plot"
  )

figure12 <- (
  fig12a_with_legend |
    fig12b_with_legend |
    fig12c_with_caption
) +
  plot_layout(
    widths = c(1, 1, 1)
  ) +
  plot_annotation(
    tag_levels = "a"
  )

figure12 <- figure12 +
  plot_annotation(
    tag_levels = "a"
  ) &
  theme(
    plot.margin = margin(0, 0, 0.5, 0)
  )

figure12

#Save Plot
if (save_figures) {
ggsave(
  filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_12.png",
  plot = figure12 + plot_annotation(tag_levels = "a"),
  width = 24.6,
  height = 14.5,
  units = "cm",
  dpi = 600
)
}


####Figure 13: Functional NMDS####


set.seed(123)

#Tree × functional group presence/absence matrix

morph_matrix <- func_pa %>%
  select(
    tree_id,
    functional_group,
    presabs
  ) %>%
  pivot_wider(
    names_from = functional_group,
    values_from = presabs,
    values_fill = 0
  )

#Community matrix for vegan

morph_comm <- morph_matrix %>%
  column_to_rownames("tree_id")

#Run NMDS

nmds_morph <- metaMDS(
  morph_comm,
  distance = "jaccard",
  k = 2,
  trymax = 100
)

nmds_morph$stress

#Extract NMDS scores

scores_morph <- as.data.frame(
  scores(
    nmds_morph,
    display = "sites"
  )
)

scores_morph$tree_id <- rownames(scores_morph)

#Add metadata

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

#Order WHIA

scores_morph$WHIA <- factor(
  scores_morph$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

#Plot

fig13 <- ggplot(
  scores_morph,
  aes(
    NMDS1,
    NMDS2,
    colour = WHIA
  )
) +
  geom_point(
    size = 3
  ) +
  stat_ellipse(
    level = 0.95,
    linewidth = 1
  ) +
  scale_colour_manual(
    values = c(
      low_impact = "#3350FF",
      medium_impact = "#D9B300",
      high_impact = "#FC3F3F"
    ),
    labels = c(
      low_impact = "Low Impact",
      medium_impact = "Medium Impact",
      high_impact = "High Impact"
    ),
    name = "WHIA"
  ) +
  theme_classic(
    base_size = 14
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "WHIA"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 15),
    legend.margin = margin(
      t = -5,
      r = 2,
      b = 0,
      l = 0
    )
  )

#PERMANOVA

adonis2(
  morph_comm ~ WHIA,
  data = scores_morph,
  method = "jaccard"
)

figure13 <- fig13

figure13

#Save Plot

if (save_figures) {
ggsave(
  filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_13.png",
  plot = figure13,
  width = 24.6,
  height = 14.5,
  units = "cm",
  dpi = 600
)
}

####Figure A1: Tree Size & Lichen Richness####

figure_A1 <- ggplot(
  tree_richness,
  aes(
    x = dbh_scaled,
    y = richness,
    colour = tree_species
  )
) +
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  geom_smooth(
    aes(group = 1),
    method = "loess",
    se = TRUE,
    colour = "black",
    linewidth = 0.8
  ) +
  scale_colour_manual(
    values = tree_species_cols,
    labels = function(x) {
      parse(text = pretty_tree_species_labels(x))
    }
  ) +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Lichen species richness",
    colour = "Host tree species"
  ) +
  theme_classic(base_size = 14)

figure_A1

if (save_figures) {
  ggsave(
    filename = "D:/Desktop/UoE/Dissertation/Diss_Repo/figures/figure_A1.png",
    plot = figure_A1,
    width = 24.6,
    height = 14.5,
    units = "cm",
    dpi = 600
  )
}