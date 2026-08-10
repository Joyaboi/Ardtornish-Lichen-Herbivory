# Do deer population control measures affect epiphytic lichen biodiversity in Scottish rainforests?

This repository contains the data-processing and statistical analysis workflows
associated with an MSc dissertation investigating relationships between woodland
herbivore impact and epiphytic lichen biodiversity at Ardtornish Estate, Scotland.

## Abstract
Atlantic temperate rainforests are ecosystems of high conservation priority in the United Kingdom, supporting diverse epiphytic lichen communities, including internationally important oceanic rainforest lichens. Herbivore management is widely used to promote woodland regeneration across Scotland, yet it remains unclear whether variation in herbivore impact influences the diversity and composition of these lichen communities. This study investigated whether woodland herbivore impact was associated with epiphytic lichen species richness, community composition, and functional trait composition.
Epiphytic lichen surveys were conducted across Atlantic oak woodland sites within the Ardtornish Estate on the Morvern Peninsula, western Scotland. Woodland Herbivore Impact Assessment Lite (WHIA Lite) classifications were used to categorise sites according to browsing intensity. Species richness, community composition, and functional trait composition were analysed using hierarchical generalised linear mixed-effects models alongside multivariate community analyses.
No evidence was found that woodland herbivore impact influenced lichen species richness, species turnover, community composition, or functional trait composition. Instead, variation in lichen communities was primarily associated with differences among woodland sites and geographic separation rather than the woodland herbivore impact gradient. Together, these findings suggest that spatial and local environmental factors exerted a stronger influence on epiphytic lichen communities than recent differences in browsing pressure represented by WHIA categories. If herbivore management affects epiphytic lichen communities, its effects are likely to occur indirectly through longer-term changes in woodland structure rather than as immediate responses to reduced browsing pressure. These findings improve understanding of the responses of Atlantic rainforest lichens to woodland regeneration and provide evidence for evaluating the biodiversity outcomes of herbivore management in temperate rainforest restoration.

## Repository structure

```text
Ardtornish-Lichen-Herbivory/
├── data/
│   ├── raw/
│   │   ├── 15_lichens.csv
│   │   ├── 15_quadrats.csv
│   │   ├── 15_sites.csv
│   │   ├── 15_trees.csv
│   │   ├── site_coordinates.csv
│   │   └── tree_coordinates.csv
│   │
│   └── processed/
│       └── lichen_data.csv
│
├── figures/
│   ├── figure_01.png
│   ├── figure_02.jpeg
│   ├── figure_03.jpeg
│   ├── figure_04.png
│   ├── figure_05.png
│   ├── figure_06.png
│   ├── figure_07.png
│   ├── figure_08.png
│   ├── figure_09.png
│   ├── figure_10.png
│   ├── figure_11.png
│   ├── figure_12.png
│   ├── figure_13.png
│   └── figure_A1.png
│
├── scripts/
│   ├── 01_data_prep.R
│   ├── 02_species_richness.R
│   ├── 03_species_turnover.R
│   ├── 04_functional_traits.R
│   ├── 05_community_composition.R
│   ├── 06_spatial_dissimilarity.R
│   ├── 07_exploratory_analysis.R
│   ├── 08_figures.R
│   └── 09_tables.R
│
├── .gitignore
├── Diss_Repo.Rproj
└── README.md
```

### Directories

- data/raw/ — Raw CSV datasets exported from the field data and spatial data collection workflow.
- data/processed/ — Processed datasets prepared for statistical analysis.
- figures/ — Figures generated for the dissertation and supplementary material.
- scripts/ — Sequential R scripts covering data preparation, statistical analyses, community analyses, figure generation, and table generation.

### Data files

The raw data are separated into datasets describing the 15 analysed woodland sites, sampled trees, quadrats, lichen observations, and associated spatial coordinates. The processed dataset combines the relevant information into a format suitable for subsequent statistical analyses.

### Analysis workflow

The numbered R scripts are intended to document the analytical workflow sequentially:

01_data_prep.R — Data preparation and transformation.

02_species_richness.R — Analysis of lichen species richness and occurrence.

03_species_turnover.R — Analysis of species turnover and species-specific responses to WHIA.

04_functional_traits.R — Analysis of lichen functional traits and functional group turnover.

05_community_composition.R — Analysis and visualisation of community composition.

06_spatial_dissimilarity.R — Analysis of spatial patterns in community dissimilarity.

07_exploratory_analysis.R — Additional exploratory analyses.

08_figures.R — Generation of dissertation figures.

09_tables.R — Generation of dissertation tables.

### Other files

- .gitignore — Specifies files excluded from version control.
- Diss_Repo.Rproj — RStudio project file.
- README.md — Documentation of the project, analytical workflow, repository structure, and reproducibility information.

## Software

- R version 4.5.1
- QGIS
- QField

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

##AI Transparency

ChatGPT 5.6 – Luna was used in the creation of this GitHub repository and the code contained within it. Specifically, this AI model assisted with the development of the repository structure and README, as well as portions of the code in scripts/01_data_prep.R, scripts/08_figures.R, and scripts/09_tables.R. AI was used primarily for debugging and troubleshooting, and no AI-generated code was incorporated without manual review and verification.
