#09_tables.R


####Packages####


library(dplyr)
library(tidyr)


####Table T1: Summary of Sampling Effort, Analytical Sampling Structure, and Lichen Dataset Composition####


#Calculate analytical sampling metrics

n_sites_analysis <- lichen_data %>%
  distinct(site_id) %>%
  nrow()

n_trees <- lichen_data %>%
  distinct(tree_id) %>%
  nrow()

n_tree_species <- lichen_data %>%
  distinct(tree_species) %>%
  nrow()

n_quadrats_with_lichens <- lichen_data %>%
  distinct(quadrat_id) %>%
  nrow()

n_lichen_occurrences <- nrow(lichen_data)

n_lichen_species <- lichen_data %>%
  distinct(lichen_species) %>%
  nrow()

#Calculate WHIA structure

n_WHIA <- lichen_data %>%
  distinct(WHIA) %>%
  nrow()

trees_per_WHIA <- lichen_data %>%
  distinct(tree_id, WHIA) %>%
  count(WHIA) %>%
  summarise(
    value = first(n)
  ) %>%
  pull(value)

sites_per_WHIA <- lichen_data %>%
  distinct(site_id, WHIA) %>%
  count(WHIA) %>%
  summarise(
    value = first(n)
  ) %>%
  pull(value)

trees_per_site <- lichen_data %>%
  distinct(site_id, tree_id) %>%
  count(site_id) %>%
  summarise(
    value = first(n)
  ) %>%
  pull(value)

quadrats_per_tree <- lichen_data %>%
  distinct(tree_id, quadrat_id) %>%
  count(tree_id) %>%
  summarise(
    value = first(n)
  ) %>%
  pull(value)

#Calculate total quadrats surveyed

n_quadrats_surveyed <- n_trees * quadrats_per_tree

#Create Table T1

table_T1 <- tibble(
  Metric = c(
    "WHIA categories",
    "Sites sampled",
    "Sites included in analysis",
    "Sites per WHIA category",
    "Trees sampled",
    "Trees per WHIA category",
    "Trees per site",
    "Tree species recorded",
    "Quadrats surveyed",
    "Quadrats containing lichens",
    "Quadrats per tree",
    "Lichen occurrences",
    "Lichen species recorded"
  ),
  Value = c(
    3,
    36,
    n_sites_analysis,
    sites_per_WHIA,
    n_trees,
    trees_per_WHIA,
    trees_per_site,
    n_tree_species,
    n_quadrats_surveyed,
    n_quadrats_with_lichens,
    quadrats_per_tree,
    n_lichen_occurrences,
    n_lichen_species
  )
)

table_T1


####Table T3: Fixed-Effect Estimates from the Functional Group Occurrence Model####


#Fix factor
func_pa$WHIA <- factor(
  func_pa$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

#Extract fixed effects

table_T3 <- summary(m_func_rich)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Term") %>%
  filter(
    Term %in% c(
      "WHIAmedium_impact",
      "WHIAhigh_impact"
    )
  ) %>%
  mutate(
    `WHIA Comparison` = c(
      "Medium-impact vs. low-impact",
      "High-impact vs. low-impact"
    ),
    `Estimate (β ± SE)` = paste0(
      sprintf("%.3f", Estimate),
      " ± ",
      sprintf("%.3f", `Std. Error`)
    ),
    `p-value` = sprintf(
      "%.3f",
      `Pr(>|z|)`
    )
  ) %>%
  select(
    `WHIA Comparison`,
    `Estimate (β ± SE)`,
    `p-value`
  )

table_T3


####Table T4: Fixed-Effect Estimates from the Hierarchical Species Occurrence Model####


#Fix Factor
species_pa$WHIA <- factor(
  species_pa$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

#Extract fixed effects

table_T4 <- summary(m_rich)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Term") %>%
  filter(
    Term %in% c(
      "WHIAmedium_impact",
      "WHIAhigh_impact"
    )
  ) %>%
  mutate(
    `WHIA Comparison` = c(
      "Medium-impact vs. low-impact",
      "High-impact vs. low-impact"
    ),
    `Estimate (β ± SE)` = paste0(
      sprintf("%.3f", Estimate),
      " ± ",
      sprintf("%.3f", `Std. Error`)
    ),
    `p-value` = sprintf(
      "%.3f",
      `Pr(>|z|)`
    )
  ) %>%
  select(
    `WHIA Comparison`,
    `Estimate (β ± SE)`,
    `p-value`
  )

table_T4


####Table T5: Hierarchical Model Comparisons####


#Compare occurrence model with reduced turnover model

comparison_1 <- anova(
  m_rich,
  m_turnover2,
  test = "Chisq"
)

#Compare reduced turnover model with species x WHIA variation model

comparison_2 <- anova(
  m_turnover2,
  m_turnover1,
  test = "Chisq"
)

#Create Table T5

table_T5 <- tibble(
  `Model comparison` = c(
    "Occurrence model vs. turnover model",
    "Site turnover vs. species x WHIA variation model"
  ),
  `ΔAIC` = c(
    AIC(m_turnover2) - AIC(m_rich),
    AIC(m_turnover1) - AIC(m_turnover2)
  ),
  `ꭕ²` = c(
    comparison_1$Chisq[2],
    comparison_2$Chisq[2]
  ),
  `df` = c(
    comparison_1$Df[2],
    comparison_2$Df[2]
  ),
  `p` = c(
    comparison_1$`Pr(>Chisq)`[2],
    comparison_2$`Pr(>Chisq)`[2]
  ),
  Interpretation = c(
    "Evidence of species turnover among trees/sites",
    "No evidence of WHIA-associated species turnover"
  )
) %>%
  mutate(
    `ΔAIC` = sprintf("%.2f", `ΔAIC`),
    `ꭕ²` = sprintf("%.2f", `ꭕ²`),
    `df` = as.integer(.data$df),
    `p` = ifelse(
      p < 0.001,
      "<0.001",
      sprintf("%.3f", p)
    )
  )

table_T5


####Table T6: Hierarchical Model Comparisons####


#Compare occurrence model with reduced turnover model

comparison_1 <- anova(
  m_func_rich,
  m_func_turnover2,
  test = "Chisq"
)

#Compare reduced turnover model with functional group x WHIA variation model

comparison_2 <- anova(
  m_func_turnover2,
  m_func_turnover1,
  test = "Chisq"
)

#Create Table T6

table_T6 <- tibble(
  `Model comparison` = c(
    "Occurrence model vs. site turnover model",
    "Site turnover vs. functional group x WHIA variation"
  ),
  `ΔAIC` = c(
    AIC(m_func_turnover2) - AIC(m_func_rich),
    AIC(m_func_turnover1) - AIC(m_func_turnover2)
  ),
  `χ²` = c(
    comparison_1$Chisq[2],
    comparison_2$Chisq[2]
  ),
  `df` = c(
    comparison_1$Df[2],
    comparison_2$Df[2]
  ),
  `p` = c(
    comparison_1$`Pr(>Chisq)`[2],
    comparison_2$`Pr(>Chisq)`[2]
  ),
  Interpretation = c(
    "Evidence of functional group turnover among trees/sites",
    "No evidence of WHIA-driven functional group turnover"
  )
) %>%
  mutate(
    `ΔAIC` = sprintf("%.2f", `ΔAIC`),
    `χ²` = sprintf("%.2f", `χ²`),
    `df` = as.integer(.data$df),
    `p` = ifelse(
      p < 0.001,
      "<0.001",
      sprintf("%.3f", p)
    )
  )

table_T6


####Table A1: Species Richness Model Estimates####


#Random-effect variance components
richness_random <- as.data.frame(
  VarCorr(m_rich)
) %>%
  transmute(
    `Model component` = grp,
    Variance = sprintf("%.3f", vcov),
    SD = sprintf("%.3f", sdcor)
  )

richness_random

#Model fit statistics
richness_fit <- tibble(
  `Model statistic` = c(
    "Observations",
    "AIC",
    "BIC",
    "Log-likelihood"
  ),
  Value = c(
    nobs(m_rich),
    sprintf("%.2f", AIC(m_rich)),
    sprintf("%.2f", BIC(m_rich)),
    sprintf("%.2f", logLik(m_rich))
  )
)

richness_fit


####Table A2: Species Turnover Model Estimates####


#Random-effect variance components
turnover_random <- as.data.frame(
  VarCorr(m_turnover1)
) %>%
  transmute(
    `Model component` = grp,
    Variance = sprintf("%.3f", vcov),
    SD = sprintf("%.3f", sdcor)
  )

turnover_random

#Model fit statistics
turnover_fit <- tibble(
  `Model statistic` = c(
    "Observations",
    "AIC",
    "BIC",
    "Log-likelihood"
  ),
  Value = c(
    nobs(m_turnover1),
    sprintf("%.2f", AIC(m_turnover1)),
    sprintf("%.2f", BIC(m_turnover1)),
    sprintf("%.2f", logLik(m_turnover1))
  )
)

turnover_fit


####Table A3: Functional-Group Model Estimates####


#Random-effect variance components
func_turnover_random <- as.data.frame(
  VarCorr(m_func_turnover1)
) %>%
  transmute(
    `Model component` = grp,
    Variance = sprintf("%.3f", vcov),
    SD = sprintf("%.3f", sdcor)
  )

func_turnover_random

#Model fit statistics
func_turnover_fit <- tibble(
  `Model statistic` = c(
    "Observations",
    "AIC",
    "BIC",
    "Log-likelihood"
  ),
  Value = c(
    nobs(m_func_turnover1),
    sprintf("%.2f", AIC(m_func_turnover1)),
    sprintf("%.2f", BIC(m_func_turnover1)),
    sprintf("%.2f", logLik(m_func_turnover1))
  )
)

func_turnover_fit


####Table A4. Alternative Models Incorporating Tree-Level Covariates ####


#Compare species richness models
richness_cov_test <- anova(
  m_rich,
  m_rich_cov,
  test = "Chisq"
)

#Compare species turnover models
turnover_cov_test <- anova(
  m_turnover1,
  m_turnover_cov,
  test = "Chisq"
)

#Compare functional group turnover models
func_turnover_cov_test <- anova(
  m_func_turnover1,
  m_func_turnover_cov,
  test = "Chisq"
)

#Extract values

A4_delta_AIC <- c(
  AIC(m_rich_cov) - AIC(m_rich),
  AIC(m_turnover_cov) - AIC(m_turnover1),
  AIC(m_func_turnover_cov) - AIC(m_func_turnover1)
)

A4_chisq <- c(
  richness_cov_test$Chisq[2],
  turnover_cov_test$Chisq[2],
  func_turnover_cov_test$Chisq[2]
)

A4_df <- c(
  richness_cov_test$Df[2],
  turnover_cov_test$Df[2],
  func_turnover_cov_test$Df[2]
)

A4_p <- c(
  richness_cov_test$`Pr(>Chisq)`[2],
  turnover_cov_test$`Pr(>Chisq)`[2],
  func_turnover_cov_test$`Pr(>Chisq)`[2]
)

#Create Table A4

table_A4 <- tibble(
  `Model comparison` = c(
    "Species richness + covariates vs. species richness",
    "Species turnover + covariates vs. species turnover",
    "Functional group turnover + covariates vs. functional group turnover"
  ),
  `ΔAIC` = sprintf("%.2f", A4_delta_AIC),
  `χ²` = sprintf("%.2f", A4_chisq),
  df = as.integer(A4_df),
  p = ifelse(
    A4_p < 0.001,
    "<0.001",
    sprintf("%.3f", A4_p)
  )
)

table_A4