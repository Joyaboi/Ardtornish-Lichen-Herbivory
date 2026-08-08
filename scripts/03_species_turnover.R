#03_species_turnover.R

####Species Turnover Models####

#Species x WHIA variation model
m_turnover1 <- glmer(
  presabs ~ WHIA +
    (1|lichen_species) +
    (1|site_id) +
    (1|lichen_species:WHIA) +
    (1|lichen_species:site_id),
  data = species_pa,
  family = binomial
)

summary(m_turnover1)

#Reduced model (no species x WHIA)
m_turnover2 <- glmer(
  presabs ~ WHIA +
    (1|lichen_species) +
    (1|site_id) +
    (1|lichen_species:site_id),
  data = species_pa,
  family = binomial
)

summary(m_turnover2)

#####Species Turnover Model + Confounders#####

m_turnover_cov <- glmer(
  presabs ~ WHIA +
    tree_species +
    dbh_scaled +
    bark_roughness_class +
    canopy_scaled +
    (1 | lichen_species) +
    (1 | site_id) +
    (1 | lichen_species:WHIA) +
    (1 | lichen_species:site_id),
  data = species_pa,
  family = binomial
)

summary(m_turnover_cov)

#####Compare Turnover Covariate Models#####

AIC(m_turnover1, m_turnover_cov)

anova(
  m_turnover1,
  m_turnover_cov,
  test = "Chisq"
)

#####Compare Turnover Models#####

anova(
  m_turnover1,
  m_turnover2
)

#High p-value suggests that adding species-specific WHIA responses did not significantly improve model fit