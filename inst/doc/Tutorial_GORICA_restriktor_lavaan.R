## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  tidy = FALSE,
  # figuren: leesbaar + responsive
  fig.width  = 8,
  fig.height = 5,
  dpi        = 150,
  fig.retina = 2,
  fig.align  = "center",
  out.width  = "100%",
  out.extra  = 'style="height:auto;"'
)

options(width = 200) 

## ----results='hide', message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
# To install restriktor in R:
#if (!require("restriktor")) install.packages("restriktor")
library(restriktor)

# print docs in the help-tab to view arguments and explanations for the function
#?goric

# To install lavaan in R:
# if (!require("lavaan")) install.packages("lavaan")
library(lavaan)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model, using own labeling
HS.model <- ' 
  visual  =~ lambda_v1*x1 + lambda_v2*x2 + lambda_v3*x3      
  textual =~ lambda_t1*x4 + lambda_t2*x5 + lambda_t3*x6
  speed   =~ lambda_s1*x7 + lambda_s2*x8 + lambda_s3*x9 
'

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Hypotheses
H1_cfa <- '
  abs(lambda_v1) > .6; abs(lambda_v2) > .6; abs(lambda_v3) > .6; 
  abs(lambda_t1) > .6; abs(lambda_t2) > .6; abs(lambda_t3) > .6; 
  abs(lambda_s1) > .6; abs(lambda_s2) > .6; abs(lambda_s3) > .6; 
'
# vs its complement (default)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit the model
# fixing the variances of all the latent variables in a CFA model to unity (std.lv = TRUE; https://www.lavaan.ugent.be/tutorial/syntax2.html)
fit_cfa <- cfa(HS.model, data = HolzingerSwineford1939,
           std.lv = TRUE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate GORICA values and weights for H1_cfa and its complement.

set.seed(100) # Needed for reproducibility & sensitivity check
results_cfa <- goric(fit_cfa, 
                  hypotheses = list(H1_cfa = H1_cfa),
                  standardized = TRUE) 

#summary(results_cfa)
results_cfa

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model
HS.model_mgcfa <- ' 
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9 
'

# configural invariance
fit_ci <- cfa(HS.model_mgcfa, 
              data = HolzingerSwineford1939, 
              group = "school")

# weak invariance
fit_wi <- cfa(HS.model_mgcfa, 
              data = HolzingerSwineford1939, 
              group = "school",
              group.equal = "loadings")

# strong invariance
fit_si <- cfa(HS.model_mgcfa, 
              data = HolzingerSwineford1939, 
              group = "school",
              group.equal = c("intercepts", "loadings"))

# strict invariance
fit_strict <- cfa(HS.model_mgcfa, 
                  data = HolzingerSwineford1939, 
                  group = "school",
                  group.equal = c("intercepts", "loadings", "residuals"))

# model comparison with AIC
AIC_meas.invar <- lavTestLRT(fit_ci, fit_wi, fit_si, fit_strict)$AIC
hypo.names <- c("configural", "weak", "strong", "strict")
AIC_weights <- calc_ICweights(AIC_meas.invar, hypo.names)
#print(AIC_weights, use_scientific = FALSE, digits = 3)
AIC_weights$ratio_IC_weights
AIC_weights$ratio_IC_weights["weak",]

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model, using own labeling (per group!)
# Specify model, such that we obtain the latent means (and not differences w.r.t. a reference category).
HS.model_mgcfa <- '
              visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9

# latent variable intercepts; here: 
# the latent means
   visual  ~ c(mean_v_P, mean_v_GW) * 1
   textual ~ c(mean_t_P, mean_t_GW) * 1
   speed   ~ c(mean_s_P, mean_s_GW) * 1
'
# where below we ensure:
# intercepts equal across groups $
# loadings equal across groups: 
# group.equal = c("intercepts", "loadings")
# and
# first intercept equal to 0:
# marker.int.zero = TRUE


# Hypotheses
H1_mgcfa <- "
abs(mean_v_P) > abs(mean_v_GW); 
abs(mean_t_P) > abs(mean_t_GW); 
abs(mean_s_P) > abs(mean_s_GW)
"
# vs its complement (default)

# fit the model
fit_mgcfa <- cfa(HS.model_mgcfa, 
                 data = HolzingerSwineford1939, 
                 group = "school", 
                 group.equal = c("intercepts", "loadings"), 
                 marker.int.zero = TRUE)

# Calculate GORICA values and weights
set.seed(100) # Needed for reproducibility & sensitivity check
results_mgcfa <- goric(fit_mgcfa, 
                       hypotheses = list(H1_mgcfa = H1_mgcfa))
results_mgcfa
#
# # Alternative:
# # In the case you want to extract the estimates and their covariance matrix yourself and use that as input for the goric function:
# #
# # Extract estimates and their covariance matrix
# names_param <- c("mean_v_P", "mean_v_GW", "mean_t_P", "mean_t_GW", "mean_s_P", "mean_s_GW")
# est <- coef(fit_mgcfa)[names_param]
# VCOV <- vcov(fit_mgcfa)[names_param, names_param]
# #
# # Calculate GORICA values and weights
# set.seed(100) # Needed for reproducibility & sensitivity check
# results_mgcfa <- goric(est, VCOV = VCOV,
#                        hypotheses = list(H1_mgcfa = H1_mgcfa)) 
# 
# # GORICA values and weights
# #summary(results_mgcfa)
# results_mgcfa

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model, using own labeling
model_sem <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ beta_dem65_ind60*ind60 + beta_dem65_dem60*dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

# Hypotheses
H1_sem <- "
abs(beta_dem65_dem60) > abs(beta_dem65_ind60) 
"
# vs its complement (default)

# fit the model
fit_sem <- sem(model_sem, data = PoliticalDemocracy)

# Calculate GORICA values and weights
set.seed(100) # Needed for reproducibility & sensitivity check
results_sem <- goric(fit_sem,
                     hypotheses = list(H1_sem = H1_sem),
                     standardized = TRUE) 

#summary(results_sem)
results_sem

# Unrestricted & Order-restricted parameter estimates of interest
# Note: Hypotheses should be specified before seeing the data.
# Looking afterwards at the standardized estimates can give you more insight.
#
# Unrestricted parameter estimates of interest
subset(standardizedSolution(fit_sem), op == "~" & lhs == "dem65")
#
# Order-restricted parameter estimates of interest;
# that is, the estimates of interest under the restrictions in the hypotheses:
results_sem$ormle

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model, using own labeling
model_msem <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ beta_x1*x1 + beta_x2*x2 + beta_x3*x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'

# Hypotheses
H1_msem <- "
abs(beta_x1) > abs(beta_x2) > abs(beta_x3) 
"
# vs its complement (default)

# fit the model
fit_msem <- sem(model = model_msem, data = Demo.twolevel, cluster = "cluster")

# Calculate GORICA values and weights
set.seed(100) # Needed for reproducibility & sensitivity check
results_msem <- goric(fit_msem, 
                  hypotheses = list(H1_msem = H1_msem),
                  standardized = TRUE) 

#summary(results_msem)
results_msem

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the model, using own labeling
# a linear growth model with a time-varying covariate
model_growth <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  # regressions
    i ~ x1 + x2
    s ~ s_1*x1 + s_2*x2
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
  ## Extra: Label intercepts
  #i ~ intercept_i * 1
  #s ~ intercept_s * 1
'

# Hypotheses
H1_growth <- "
abs(s_2) > abs(s_1) 
"
# vs its complement (default)

# fit the model
fit_growth <- growth(model_growth, data = Demo.growth)

# Calculate GORICA values and weights
set.seed(100) # Needed for reproducibility & sensitivity check
results_growth <- goric(fit_growth,
                        hypotheses = list(H1_growth = H1_growth),
                        standardized = TRUE) 

#summary(results_growth)
results_growth

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create data
set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)

# specify the model, using own labeling
model_med <- ' 
           # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             indirect := a*b
           # direct effect (c) # Optional
             direct := c       # Could leave this out and use c
         '

# Hypotheses
H_part <- "abs(indirect) > 0.1; abs(direct) > 0.1"
H_full <- "abs(indirect) > 0.1; -0.1 < direct < 0.1"
# and unconstrained as failsafe (default)

# fit the model
fit_med <- sem(model_med, data = Data)
#summary(fit_med, standardized = TRUE)

# Calculate GORICA values and weights:
#
# 1. based on the object
set.seed(100) # Needed for reproducibility & sensitivity check
results_med <- goric(fit_med,
                     hypotheses = list(H_part = H_part, H_full = H_full),
                     standardized = TRUE)

## ----echo = TRUE, eval = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#summary(results_med)
results_med
#
# # Alternative:
# # 2. based on extracted estimates
# # Extract standardized estimates of the defined parameters and their covariance matrix
# label_names <- c("direct", "indirect")
# est <- as.vector(standardizedSolution(fit_med)['est.std'])$est.std
# names(est) <- standardizedSolution(fit_med)$label
# est <- est[label_names]
# #est
# VCOV <- lavInspect(fit_med, "vcov.def.std.all")[label_names, label_names] 
# # VCOV # covariance matrix of parameter estimates
# #
# # GORICA
# set.seed(123) # for reproducibility & possibly sensitivity check
# results_med <- goric(est, VCOV = VCOV,
#                    hypotheses = list(H_part = H_part, H_full = H_full))
# #summary(results_med)
# results_med

