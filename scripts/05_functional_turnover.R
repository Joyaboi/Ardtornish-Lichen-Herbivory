#04_functional_turnover.R


####Packages####


library(dplyr)
library(tidyr)
library(lme4)
library(ggeffects)


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