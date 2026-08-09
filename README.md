# Do deer population control measures affect epiphytic lichen biodiversity in Scottish rainforests?

This repository contains the data-processing and statistical analysis workflows
associated with an MSc dissertation investigating relationships between woodland
herbivore impact and epiphytic lichen biodiversity at Ardtornish Estate, Scotland.

## Abstract
Atlantic temperate rainforests are ecosystems of high conservation priority in the United Kingdom, supporting diverse epiphytic lichen communities, including internationally important oceanic rainforest lichens. Herbivore management is widely used to promote woodland regeneration across Scotland, yet it remains unclear whether variation in herbivore impact influences the diversity and composition of these lichen communities. This study investigated whether woodland herbivore impact was associated with epiphytic lichen species richness, community composition, and functional trait composition.
Epiphytic lichen surveys were conducted across Atlantic oak woodland sites within the Ardtornish Estate on the Morvern Peninsula, western Scotland. Woodland Herbivore Impact Assessment Lite (WHIA Lite) classifications were used to categorise sites according to browsing intensity. Species richness, community composition, and functional trait composition were analysed using hierarchical generalised linear mixed-effects models alongside multivariate community analyses.
No evidence was found that woodland herbivore impact influenced lichen species richness, species turnover, community composition, or functional trait composition. Instead, variation in lichen communities was primarily associated with differences among woodland sites and geographic separation rather than the woodland herbivore impact gradient. Together, these findings suggest that spatial and local environmental factors exerted a stronger influence on epiphytic lichen communities than recent differences in browsing pressure represented by WHIA categories. If herbivore management affects epiphytic lichen communities, its effects are likely to occur indirectly through longer-term changes in woodland structure rather than as immediate responses to reduced browsing pressure. These findings improve understanding of the responses of Atlantic rainforest lichens to woodland regeneration and provide evidence for evaluating the biodiversity outcomes of herbivore management in temperate rainforest restoration.

## Repository structure

Diss_Repo/
├── README.md
│
├── data/
│   ├── raw/
│   │   ├── 15_sites.csv
│   │   ├── 15_trees.csv
│   │   ├── 15_quadrats.csv
│   │   ├── 15_lichens.csv
│   │   ├── site_coordinates.csv
│   │   └── tree_coordinates.csv
│   │
│   └── processed/
│       └── lichen_data.csv
│
├── scripts/
│   ├── 01_data_preparation.R
│   ├── 02_species_richness.R
│   ├── 03_species_turnover.R
│   ├── 04_functional_traits.R
│   ├── 05_nmds.R
│   ├── 06_figures.R
│   └── 07_tables.R
│
├── figures/
│   ├── Figure_1.png
│   ├── Figure_2.png
│   ├── Figure_3.png
│   ├── Figure_4.png
│   ├── Figure_5.png
│   ├── Figure_6.png
│   ├── Figure_7.png
│   └── Figure_8.png
│
└── .gitignore

## Software

R
QGIS
QField

Key R packages include:
- vegan
- lme4
- ggplot2
- dplyr
- tidyr
- patchwork

## Reproducibility

Scripts are numbered according to the order in which major processing and
analytical steps are performed.
