#04_functional_traits.R


####Packages####


library(dplyr)
library(tidyr)
library(lme4)
library(ggeffects)


####Hadfield Functional Turnover####


#####Make a tree x Functional matrix#####

func_pa <- lichen_data %>%
  distinct(tree_id, functional_group) %>%
  mutate(presabs = 1)

func_pa <- expand_grid(
  tree_id = unique(lichen_data$tree_id),
  functional_group = unique(lichen_data$functional_group)
) %>%
  left_join(
    func_pa,
    by = c("tree_id", "functional_group")
  ) %>%
  mutate(
    presabs = replace_na(presabs, 0)
  )

#####Add metadata#####

func_pa <- func_pa %>%
  left_join(
    lichen_data %>%
      distinct(
        tree_id,
        site_id,
        WHIA,
        tree_species,
        dbh_scaled,
        bark_roughness_class,
        canopy_scaled
      ),
    by = "tree_id"
  )

#####Fix Factor#####

func_pa$WHIA <- factor(
  func_pa$WHIA,
  levels = c(
    "low_impact",
    "medium_impact",
    "high_impact"
  )
)

#####Functional Richness#####

m_func_rich <- glmer(
  presabs ~ WHIA +
    (1|functional_group) +
    (1|site_id),
  data = func_pa,
  family = binomial
)

summary(m_func_rich)

#####Functional Richness + Confounders#####

m_func_rich_cov <- glmer(
  presabs ~ WHIA +
    tree_species +
    dbh_scaled +
    bark_roughness_class +
    canopy_scaled +
    (1 | functional_group) +
    (1 | site_id),
  data = func_pa,
  family = binomial
)

#####Compare Functional Richness Models#####

AIC(m_func_rich, m_func_rich_cov)

anova(
  m_func_rich,
  m_func_rich_cov,
  test = "Chisq"
)

#####Functional Turnover#####

m_func_turnover1 <- glmer(
  presabs ~ WHIA +
    (1|functional_group) +
    (1|site_id) +
    (1|functional_group:WHIA) +
    (1|functional_group:site_id),
  data = func_pa,
  family = binomial
)

summary(m_func_turnover1)

m_func_turnover2 <- glmer(
  presabs ~ WHIA +
    (1|functional_group) +
    (1|site_id) +
    (1|functional_group:site_id),
  data = func_pa,
  family = binomial
)

summary(m_func_turnover2)

anova(
  m_func_turnover1,
  m_func_turnover2
)

#####Functional Turnover + Confounders#####

m_func_turnover_cov <- glmer(
  presabs ~ WHIA +
    tree_species +
    dbh_scaled +
    bark_roughness_class +
    canopy_scaled +
    (1 | functional_group) +
    (1 | site_id) +
    (1 | functional_group:WHIA) +
    (1 | functional_group:site_id),
  data = func_pa,
  family = binomial
)

#####Compare Functional Turnover Confounding Models#####

AIC(m_func_turnover1, m_func_turnover_cov)

anova(
  m_func_turnover1,
  m_func_turnover_cov,
  test = "Chisq"
)

#####Test Functional Turnover#####

anova(
  m_func_rich,
  m_func_turnover1,
  test = "Chisq"
)

#####Plot Functional Responses#####

plot_model <- ggpredict(
  m_func_rich,
  terms = "WHIA"
)

plot(plot_model)

ranef(m_func_turnover1)$`functional_group:WHIA`