## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  tidy = FALSE,
  fig.width  = 8,
  fig.height = 5,
  dpi        = 150,
  fig.retina = 2,
  fig.align  = "center",
  out.width  = "100%",
  out.extra  = 'style="height:auto;"'
)

options(width = 200) 

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(Kuiper2012estimates)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
## First, install the package, if you have not done this already:
if (!require("restriktor")) install.packages("restriktor")

## Then, load the packages:
library(restriktor) # for the goric function

# If you want to use restriktor from github:
#if (!require("devtools")) install.packages("devtools")
#library(devtools) 
#install_github("LeonardV/restriktor")
#library(restriktor) # for goric function

# print docs in the help-tab to view arguments and explanations for the function
#?evSyn

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# set estimates as named vectors and store in list.
estimates <- lapply(Kuiper2012estimates$estimate, setNames, "beta_past")

# store variances (i.e., the square of the standard errors) 
# in list of matrices.
covmats <- lapply(Kuiper2012estimates$se^2, as.matrix)

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
Hpos <- 'beta_past > 0'

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
evSyn_trust <- evSyn(estimates, VCOV = covmats, 
                     hypotheses = list(Hpos = Hpos),
                     # type = 'added', # default
                     comparison = 'complement')

evSyn_trust
#summary(evSyn_trust)
plot(evSyn_trust)
#round(evSyn_trust$Final_ratio_GORICA_weights, 2)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# To install the packages in R:
#if (!require("restriktor")) install.packages("restriktor")

# To install restriktor from github:
# if (!require("devtools")) install.packages("devtools")
# library(devtools) 
# install_github("LeonardV/restriktor")
library(restriktor)
#
# print docs in the help-tab to view arguments and explanations for the function
#?evSyn

# Data
library(metadat)
dat <- dat.berkey1998
#
# print docs in the help-tab to view explanations for the data set
#?dat.berkey1998

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# View(dat)

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Store estimates in list of named vectors
estimates <- lapply(seq(1, nrow(dat), 2), function(i)
  setNames(c(dat$yi[i:(i+1)]), c('PD', 'AL')))
# Note that these names will be used in specifying the hypotheses.

# Store (co-)variances in list of (unnamed) variance-covariance matrices.
covmats <- lapply(seq(1, nrow(dat), 2), function(i) 
  as.matrix(dat[i:(i+1), c('v1i', 'v2i')]))
# Note that it does not matter what the names/labels are;
# for the estimates it does matter.

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set 1
H1.1 <- "AL < 0; PD > 0"
H1.2 <- "AL < 0; PD < 0"
#
# Set 2
H2 <- "abs(AL) > 0.2; abs(PD) > 0.2"
#
# Set 3
H3 <- "abs(AL) < abs(PD)"

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set1 <- evSyn(object = estimates, VCOV = covmats,
                      hypotheses = list(H1.1 = H1.1, H1.2 = H1.2)
                      #comparison = "unconstrained" # default
                      #type = "added" # default
                      )

results_Set1
# summary(results_Set1)
plot(results_Set1)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(results_Set1$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set1_H1c <- evSyn(estimates, VCOV = covmats,
                     hypotheses = list(H1.1 = H1.1),
                     comparison = 'complement')

results_Set1_H1c
# summary(results_Set1_H1c)
plot(results_Set1_H1c)
#round(results_Set1_H1c$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set2 <- evSyn(estimates, VCOV = covmats,
                    hypotheses = list(H1 = H2),
                    comparison = 'complement')

results_Set2
# summary(results_Set2)
plot(results_Set2)
#round(results_Set2$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set3 <- evSyn(estimates, VCOV = covmats,
                    hypotheses = list(H1 = H3),
                    comparison = 'complement')

results_Set3
# summary(results_Set3)
plot(results_Set3)
#round(results_Set3$Final_ratio_GORICA_weights, 2)

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set1 <- evSyn(object = estimates, VCOV = covmats,
                      hypotheses = list(H1.1 = H1.1, H1.2 = H1.2)
                      #comparison = "unconstrained" # default
                      #type = "added" # default
                      )

results_Set1
# summary(results_Set1)
plot(results_Set1)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# goric objects needed to obtain the GORICA values from
gorica_Study1 <- goric(estimates[[1]], VCOV = covmats[[1]],
                       hypotheses = list(H1.1=H1.1, H1.2=H1.2))
gorica_Study2 <- goric(estimates[[2]], VCOV = covmats[[2]],
                       hypotheses = list(H1.1=H1.1, H1.2=H1.2))
gorica_Study3 <- goric(estimates[[3]], VCOV = covmats[[3]],
                       hypotheses = list(H1.1=H1.1, H1.2=H1.2))
gorica_Study4 <- goric(estimates[[4]], VCOV = covmats[[4]],
                       hypotheses = list(H1.1=H1.1, H1.2=H1.2))
gorica_Study5 <- goric(estimates[[5]], VCOV = covmats[[5]],
                       hypotheses = list(H1.1=H1.1, H1.2=H1.2))

# list of GORICA values for each of the five studies
GORICA <- list(gorica_Study1$result[,4], gorica_Study2$result[,4],
               gorica_Study3$result[,4], gorica_Study4$result[,4],
               gorica_Study5$result[,4])
# GORICA

# GORICA evidence synthesis
results_Set1_gorica <- evSyn(GORICA)
#
results_Set1_gorica
# summary(results_Set1_gorica)
#plot(results_Set1_gorica)
#round(results_Set1_gorica$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
results_Set1_gorica <- evSyn(GORICA, 
                             hypo_names = c("H1.1", "H1.2", "Hu"))
#
results_Set1_gorica
# summary(results_Set1_gorica)
#plot(results_Set1_gorica)
#round(results_Set1_gorica$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# list of GORICA weights for each of the five studies
GORICAweights <- list(gorica_Study1$result[,7], gorica_Study2$result[,7],
               gorica_Study3$result[,7], gorica_Study4$result[,7],
               gorica_Study5$result[,7])
# GORICAweights

# GORICA evidence synthesis
results_Set1_gw <- evSyn(GORICAweights, 
                         hypo_names = c("H1.1", "H1.2", "Hu"))
#
results_Set1_gw
#summary(results_Set1_gw)
#plot(results_Set1_gw)
#round(results_Set1_gw$Final_ratio_GORICA_weights, 2)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Log-likelihood values
LL_Set1 <- list(gorica_Study1$result[,2], gorica_Study2$result[,2],
                gorica_Study3$result[,2], gorica_Study4$result[,2],
                gorica_Study5$result[,2])
# Penalty values
PT_Set1  <- list(gorica_Study1$result[,3], gorica_Study2$result[,3],
                 gorica_Study3$result[,3], gorica_Study4$result[,3],
                 gorica_Study5$result[,3])

# GORICA evidence synthesis
results_Set1_LLandPT <- evSyn(LL_Set1, PT = PT_Set1,
                            #type = "added", # Default
                            hypo_names = c("H1.1", "H1.2", "Hu"))
#
results_Set1_LLandPT
# summary(results_Set1_LLandPT)
#plot(results_Set1_LLandPT)
#round(results_Set1_LLandPT$Final_ratio_GORICA_weights, 2)

