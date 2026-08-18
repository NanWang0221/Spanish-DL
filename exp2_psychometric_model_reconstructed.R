# ============================================================================
# Experiment 2: Bayesian mixed-effects PSYCHOMETRIC model  (RECONSTRUCTED)
# ----------------------------------------------------------------------------
# NOTE: The real brm() call is NOT in section-3-experiment2.Rmd (that file is a
# draft). This script reconstructs the model from (a) the published paper's
# spec, (b) the working Exp 1 model in section-2-experiment1.Rmd, and (c) the
# variable names left in the commented plotting code (VOT_gs, Block,
# Condition.Exposure == "Shift40", VOT.mean_test, post_sample_block1).
# Every reconstructed assumption is marked with  ## VERIFY.
# ============================================================================

library(tidyverse)
library(magrittr)
library(brms)
library(tidybayes)
library(MASS)        # contr.sdif() = backward difference coding

# ---------------------------------------------------------------------------
# 1. Data
# ---------------------------------------------------------------------------
## VERIFY: filename / column names for the Exp 2 exposure-test data
d.exp2 <- read_csv("../data/d.test.Exp2.csv", show_col_types = FALSE)

# Exp 2 has interleaved EXPOSURE and TEST blocks. The paper fits TWO separate
# models (one per phase). VOT is centred on the TEST-block mean in BOTH models
# so that all other effects are evaluated at the same VOT across models.
## VERIFY: the column that marks phase (here assumed "Phase" %in% c("exposure","test"))
VOT.mean_test <- mean(d.exp2$Item.VOT[d.exp2$Phase == "test"])
VOT.sd_test   <- sd(  d.exp2$Item.VOT[d.exp2$Phase == "test"])

# ---------------------------------------------------------------------------
# 2. Predictor coding
# ---------------------------------------------------------------------------
d.exp2 %<>%
  mutate(
    # Gelman scaling: centre, divide by TWICE the SD  (Gelman, 2008)
    VOT_gs = (Item.VOT - VOT.mean_test) / (2 * VOT.sd_test),
    # exposure condition (between-participants): +0 / +10 / +40 ms shift
    Condition.Exposure = factor(Condition.Exposure,
                                levels = c("Shift0", "Shift10", "Shift40")),
    # discrete test blocks (this replaces Exp 1's continuous Trial)
    Block = factor(Block)
  )

# Backward difference coding: each level vs the previous one
contrasts(d.exp2$Condition.Exposure) <- MASS::contr.sdif(nlevels(d.exp2$Condition.Exposure))
contrasts(d.exp2$Block)              <- MASS::contr.sdif(nlevels(d.exp2$Block))

d.test     <- filter(d.exp2, Phase == "test")
d.exposure <- filter(d.exp2, Phase == "exposure")   # unlabeled trials only ## VERIFY

# ---------------------------------------------------------------------------
# 3. Weakly-regularizing priors
# ---------------------------------------------------------------------------
# The model is a 2-component mixture:
#   component 2 = mu2  -> PERCEPTUAL model (stimulus-driven)
#   component 1 = mu1  -> LAPSING model   (stimulus-independent response bias)
#   theta1             -> LAPSE RATE      (mixing weight on component 1)
# p("t") = (1 - lapse) * inv_logit(mu2) + lapse * inv_logit(mu1)
priors <- c(
  # perceptual fixed effects: Student-t(df=3, 0, 2.5)
  set_prior("student_t(3, 0, 2.5)", class = "b",         dpar = "mu2"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "mu2"),
  # WIDER prior on the VOT slope (scale 15) — needed for convergence in Exp 2
  set_prior("student_t(3, 0, 15)",  class = "b", coef = "VOT_gs", dpar = "mu2"),
  # lapsing bias intercept
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", dpar = "mu1"),
  # lapse rate: brms default logistic(0, 1)  (uniform-ish over 0..1, extremes down-weighted)
  set_prior("logistic(0, 1)",       class = "Intercept", dpar = "theta1"),
  # random-effect SDs and correlations
  set_prior("cauchy(0, 2)",         class = "sd"),
  set_prior("lkj(1)",               class = "cor")
)

# ---------------------------------------------------------------------------
# 4a. MAIN model  (full factorial -> use for hypothesis tests / effects)
# ---------------------------------------------------------------------------
# Maximal random effects:
#   by-participant: intercept + slopes for the WITHIN-participant factors
#                   (Block, VOT, and their interaction).  Condition is
#                   between-participants, so it is NOT a by-participant slope.
#   by-item (the 3 minimal pairs): intercept + slopes for the FULL factorial
#                   (Condition x Block x VOT).
# Shared grouping tags ( | p | , | i | ) estimate the covariance among random
# effects across the three linear predictors (mu1, mu2, theta1) / within item.
bf_test <- bf(
  Response.Voicing == "voiceless" ~ 1,                       # dummy; dpars below

  mu2 ~ Condition.Exposure * Block * VOT_gs +
        (1 + Block * VOT_gs | p | ParticipantID) +
        (1 + Condition.Exposure * Block * VOT_gs | i | Item.MinimalPair),

  mu1    ~ 1 + (1 | p | ParticipantID),                      # response bias
  theta1 ~ 1 + (1 | p | ParticipantID)                       # lapse rate
)

fit_test <- brm(
  formula = bf_test,
  data    = d.test,
  family  = mixture(bernoulli("logit"), bernoulli("logit")),
  prior   = priors,
  chains  = 4, cores = 4,
  iter    = 4000, warmup = 2000,          # -> 2000 post-warmup draws/chain = 8000 total
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  file    = "../models/Exp2-test-psychometric"
)

# The exposure-phase model is identical in structure, fit to d.exposure:
# fit_exposure <- update(fit_test, newdata = d.exposure,
#                        file = "../models/Exp2-exposure-psychometric")

# ---------------------------------------------------------------------------
# 4b. CELL-MEANS variant  (separate intercept + VOT slope per Condition x Block)
#     Prediction-equivalent to 4a; convenient for reading off PSEs directly.
#     ( response ~ 0 + (condition * block) / VOT ; see paper SI 5.1 )
# ---------------------------------------------------------------------------
bf_cells <- bf(
  Response.Voicing == "voiceless" ~ 1,
  mu2 ~ 0 + (Condition.Exposure * Block) / VOT_gs +
        (1 + Block * VOT_gs | p | ParticipantID) +
        (1 + Condition.Exposure * Block * VOT_gs | i | Item.MinimalPair),
  mu1    ~ 1 + (1 | p | ParticipantID),
  theta1 ~ 1 + (1 | p | ParticipantID)
)
# fit_cells <- brm(bf_cells, d.test, family = mixture(bernoulli("logit"), bernoulli("logit")),
#                  prior = priors, chains = 4, cores = 4, iter = 4000, warmup = 2000,
#                  control = list(adapt_delta = 0.99), file = "../models/Exp2-test-cellmeans")

# ---------------------------------------------------------------------------
# 5. Derive model-predicted PSE per Condition x Block  (lapse-corrected)
# ---------------------------------------------------------------------------
# PSE = the VOT where the PERCEPTUAL log-odds (mu2) = 0.  Because mu2 is linear
# in VOT_gs, PSE = -intercept/slope.  We get intercept & slope robustly (works
# under any contrast coding) by predicting the mu2 linear predictor at two VOT
# points per cell, marginalizing over random effects (re_formula = NA).
descale <- function(z, m, s) z * (2 * s) + m     # inverse of the /(2*SD) scaling

grid <- expand_grid(
  VOT_gs             = c(0, 1),                   # 0 -> intercept, slope = f(1) - f(0)
  Condition.Exposure = levels(d.test$Condition.Exposure),
  Block              = levels(d.test$Block)
)

pse_by_cell <- grid %>%
  add_linpred_draws(fit_test, dpar = "mu2", re_formula = NA) %>%
  ungroup() %>%
  select(.draw, Condition.Exposure, Block, VOT_gs, .linpred) %>%
  pivot_wider(names_from = VOT_gs, values_from = .linpred,
              names_prefix = "lp") %>%
  mutate(
    intercept = lp0,
    slope     = lp1 - lp0,
    PSE_ms    = descale(-intercept / slope, VOT.mean_test, VOT.sd_test)
  ) %>%
  group_by(Condition.Exposure, Block) %>%
  median_qi(PSE_ms, .width = 0.95)

print(pse_by_cell)

# ---------------------------------------------------------------------------
# 6. Example hypothesis tests (Bayes factor + posterior probability)
# ---------------------------------------------------------------------------
# e.g. did the +40 shift move the boundary more than the +10 shift by block 9?
# hypothesis(fit_test, "mu2_...", class = "b")     ## fill in coef names via
# fixef(fit_test) / get_variables(fit_test)
#
# Compare the PSE draws (pse_by_cell, before median_qi) against the idealized
# pre-exposure listener / idealized learner PSEs to test predictions 1-4.
# ============================================================================
