# library(testthat)
# 
# 
# test_that("conTest p-value works correctly", {
#   DATA <- subset(ZelazoKolb1972, Group != "Control")
#   fit.lm <- lm(Age ~ -1 + Group, data = DATA)
#   myConstraints <- 'GroupActive < GroupPassive < GroupNo'
#   
#   result <- iht(fit.lm, constraints = myConstraints)
#   
#   expect_s3_class(result, "conTest")
#   expect_equal(round(result$global$pvalue, 3)[1], 0.028)
#   expect_equal(round(result$A$pvalue, 3)[1], 0.028)
#   expect_equal(round(result$B$pvalue, 3)[1], 1)
#   expect_equal(round(result$C$pvalue, 3)[1], 0.153)
# })
# 
# 
# test_that("LPs lm object are computed correctly", {
#   DATA <- subset(ZelazoKolb1972, Group != "Control")
#   fit.lm <- lm(Age ~ -1 + Group, data = DATA)
#   fit.glm <- glm(Age ~ -1 + Group, data = DATA)
#   myConstraints <- 'GroupActive < GroupPassive < GroupNo'
#   
#   result.lm <- goric(fit.lm, hypotheses = list(H1a = myConstraints))
#   result.glm <- goric(fit.glm, hypotheses = list(H1a = myConstraints))
#   result.est <- goric(coef(fit.lm), VCOV = vcov(fit.lm), hypotheses = list(H1a = myConstraints))
#   
#   expect_s3_class(result.lm, "con_goric")
#   expect_s3_class(result.glm, "con_goric")
#   expect_s3_class(result.est, "con_goric")
#   
#   actual.lm <- round(c(result.lm$objectList$H1a$wt.bar), 3)
#   actual.glm <- round(c(result.glm$objectList$H1a$wt.bar), 3)
#   actual.est <- round(c(result.est$objectList$H1a$wt.bar), 3)
#   
#   names(actual.lm) <- NULL
#   names(actual.glm) <- NULL
#   names(actual.est) <- NULL
#   
#   expect_equal(actual.lm, c(0.329, 0.500, 0.171))
#   expect_equal(actual.glm, c(0.329, 0.500, 0.171))
#   expect_equal(actual.est, c(0.329, 0.500, 0.171))
# })
# 
# 
# test_that("goric, PT and LL (i.e., weights) are computed correctly", {
#   DATA <- subset(ZelazoKolb1972, Group != "Control")
#   fit.lm <- lm(Age ~ -1 + Group, data = DATA)
#   myConstraints <- 'GroupActive < GroupPassive < GroupNo'
#   
#   result.restr <- restriktor(fit.lm, constraints = myConstraints, se = "none")
#   s.restr <- summary(result.restr)
#   
#   result.goric <- goric(fit.lm, hypotheses = list(H1a = myConstraints))
#   result.est <- goric(coef(fit.lm), VCOV = vcov(fit.lm), hypotheses = list(H1a = myConstraints))
#   
#   expect_s3_class(result.restr, "restriktor")
#   expect_s3_class(result.goric, "con_goric")
#   expect_s3_class(result.est, "con_goric")
#   
#   #actual.restr.goric <- round(c(s.restr$goric), 3)
#   #actual.restr.PT <- round(c(attr(s.restr$goric, "penalty")), 3)
#   #actual.restr.LL <- round(c(attr(s.restr$goric, "loglik")), 3)
#   
#   #actual.goric <- round(c(result.goric$result[1, "goric"]), 3)
#   #actual.goric.PT <- round(c(result.goric$result[1, "penalty"]), 3)
#   #actual.goric.LL <- round(c(result.goric$result[1, "loglik"]), 3)
#   actual.goric.gw <- round(c(result.goric$result[1, "loglik"]), 3)
#   
#   #actual.goric.est <- round(c(result.est$result[1, "gorica"]), 3)
#   actual.goric.est.PT <- round(c(result.est$result[1, "penalty"]), 3)
#   actual.goric.est.LL <- round(c(result.est$result[1, "loglik"]), 3)
#   
#   #expect_equal(actual.restr.goric, actual.goric)
#   expect_equal(actual.restr.PT, actual.goric.PT)
#   expect_equal(actual.restr.PT, actual.goric.est.PT)
#   expect_equal(actual.restr.LL, actual.goric.LL)
#   expect_equal(actual.restr.LL, actual.goric.est.LL)
# })
# 
# 
# # -------------------------------------------------------------------------
# fit.lm <- lm(Age ~ 1 + Group, data = ZelazoKolb1972)
# 
# myConstraints <- '
# #  GroupNo > 0 & GroupNo < 1 # range restrictie
# #  GroupNo < -2 & GroupNo = 1 # constraints are inconsistent, no solution (restriktor() and goric())
# # GroupNo < -2 & GroupNo < 1 & GroupNo < 0
# GroupNo < 1 & GroupNo > 2
# # GroupPassive < 1 & GroupPassive < 0;
# # (GroupNo - GroupPassive) < -1;
# # new := (GroupNo - GroupPassive); 
# # new < 2
#  '
# 
# fit.restr <- restriktor(fit.lm, constraints = myConstraints)
# fit.restr
# summary(fit.restr)
# fit.restr$constraints
# fit.restr$rhs
# 
# fit.goric <- goric(fit.lm, hypotheses = list(myConstraints))
# summary(fit.goric)
# 
# 
# h1 <- 'GroupNo < 1'
# h2 <- 'GroupNo < 2'
# h3 <- 'GroupNo < 3'
# 
# goric(fit.lm, hypotheses = list(h1), comparison = "complement")
# restriktor:::print.con_goric(goric(fit.lm, hypotheses = list(h1, h2, h3), comparison = "unconstrained"))
# restriktor:::print.con_goric(goric(fit.lm, hypotheses = list(h1), comparison = "unconstrained"))
# restriktor:::print.con_goric(goric(fit.lm, hypotheses = list(h1, h2), comparison = "unconstrained"))
# 
# 
# # no range restrictions specified
# Amat <- matrix(c(1, 0, 0, 1, -1, 0), nrow = 3)
# restriktor:::detect_range_restrictions(Amat)
# restriktor:::PT_Amat_meq(Amat, meq = 1)
# 
# 
# 
# #devtools::test()
# ## PT check
# n <- 100
# p <- 4
# betas <- seq(0, by = 0.2, length.out = p)
# set.seed(3013073)
# X <- cbind(mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = diag(p)))
# colnames(X) <- c("x1","x2","x3","x4")
# z <- X %*% betas        
# y <- z + rnorm(n)
# DATA <- data.frame(y, X)
# model1 <- y ~  1 + x1 + x2 + x3 + x4
# 
# linmod1 <- glm(model1, data = DATA, family = "gaussian")
# 
# # PT = 5.5
# H1 <- 'x1 < x2'
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# #---
# H1 <- list(constraints = rbind(c(0,-1,1,0,0)), rhs = 0, neq = 0)
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# #---
# H1 <- 'x1 < x2'
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement",
#       mix_weights = "boot")
# 
# 
# # PT = 5.5
# H1 <- 'x1 < 0'
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# #---
# H1 <- list(constraints = rbind(c(0,-1,0,0,0)), rhs = 0, neq = 0)
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# 
# 
# # PT = 5.5
# H1 <- 'abs(x1) < 0.01'
# goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# 
# # PT = 5 (residual variance +1)
# H1 <- '0 < x1 < 0.01'
# out <- goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement")
# out
# out$objectList$H1$wt.bar
# # PT = 4 
# out <- goric(coef(linmod1), VCOV = vcov(linmod1), hypotheses = list(H1 = H1), comparison = "complement")
# out
# out$objectList$H1$wt.bar
# 
# H1 <- 'x1 < 0.01'
# out <- goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement", type = "goric")
# out
# out$objectList$H1$wt.bar
# 
# 
# H1 <- 'x1 < 0'
# out <- goric(linmod1, hypotheses = list(H1 = H1), comparison = "complement", 
#              mix_weights = "pmvnorm",
#              lower = rep(-1, rep(length(coef(linmod1)))),
#              upper = rep( 1, rep(length(coef(linmod1)))))
# out$objectList$H1$wt.bar
# 
# # prepare data
# DATA <- subset(ZelazoKolb1972, Group != "Control")
# 
# # fit unrestrikted linear model
# fit1.lm <- lm(Age ~ Group, data = DATA)
# 
# # some artificial restrictions
# H1 <- "GroupPassive > 0"#; GroupPassive < GroupNo"
# H2 <- "GroupPassive > 0; GroupPassive > GroupNo"
# H3 <- "GroupPassive = 0; GroupPassive < GroupNo"
# 
# goric(est, VCOV = VCOV, hypotheses = list(H1 = H1, H2 = H2, H3 = H3), 
#       mix_weights = "pmvnorm")
# 
# 
# est <- coef(fit1.lm)
# VCOV <- vcov(fit1.lm)
# # object is of class lm
# out <- goric(est, VCOV = VCOV, hypotheses = list(H1 = H1, H2 = H2, H3 = H3), 
#              mix_weights = "pmvnorm", # switches to boot method (for now)
#              lower = rep(-1, rep(length(est))),
#              upper = rep( 1, rep(length(est))),
#              seed = 123)
# out$objectList$H1$wt.bar
# 
# # try to adjust ic.weights() function to deal with truncated mvnorm
# 
# 
# fit.restr <- restriktor(linmod1, constraints = H1, control = list(mix_weights_bootstrap_limit = 999))
# fit.restr$wt.bar
# 
# 
# 
# 
# 
# 
# 
# #undebug(lavaan:::lav_func_jacobian_complex)
# #undebug(lavaan:::lav_constraints_parse)
# H1a <- 'x1 < 0; abs(x2) < 0.01'
# H1b <- 'abs(x2) < 0.01'
# 
# out1a <- goric(linmod1, hypotheses = list(H1a = H1a), comparison = "complement")
# out1b <- goric(linmod1, hypotheses = list(H1b = H1b), comparison = "complement")
# out1a$constraints
# out1b$constraints
# 
# # library(lavaan)
# # model1 <- y ~  1 + a*x1 + b*x2 + x3 + x4
# # constraints <- 'a < 0; abs(b) < .01'
# # fit1a.lav <- sem(model1, data = DATA, constraints = constraints)
# # fit1a.lav@Model@cin.JAC
# 
# 
# 
# DATA <- ZelazoKolb1972
# DATA <- subset(DATA, Group != "Control")
# 
# fit.lm <- lm(Age ~ -1 + Group, data = DATA)
# summary(fit.lm)
# 
# myConstraints <- ' GroupActive  < GroupPassive; 
#                    GroupPassive < GroupNo '
# 
# fit.con <- restriktor(fit.lm, constraints = myConstraints)
# summary(fit.con)
# 
# goric(fit.lm, hypotheses = list(H1 = myConstraints))
# 
# 
# DATA1 <- subset(ZelazoKolb1972, Group != "Control")
# fit1.lm <- lm(Age ~ -1 + Group, data = DATA1)
# myConstraints1 <- ' GroupActive < GroupPassive < GroupNo '
# 
# fit.rest <- restriktor(fit1.lm, myConstraints1)
# fit.rest
# summary(fit.rest)
# goric(fit.rest)
# 
# undebug(restriktor:::print.con_goric)
# 
# library(profvis)
# #Rprof("my_profile.out")
# 
# #profvis({
# goric(fit1.lm, hypotheses = list(myConstraints1, myConstraints1, myConstraints1))
# #})
# 
# h1 <- fit.rest$constraints
# rhs <- fit.rest$rhs
# 
# myConstraints1 <- list(constraints = h1, rhs = rhs, neq = 0)
# fit.restr <- restriktor(fit1.lm, constraints = h1, rhs = rhs)
# 
# goric(fit1.lm, hypotheses = list(H1 = list(constraints = h1, rhs = rhs, neq = 0),
#                                  H2 = list(constraints = h1, rhs = rhs, neq = 0),
#                                  H3 = list(constraints = h1, rhs = rhs, neq = 0)))
# 
# #Rprof(NULL)
# #summary <- summaryRprof("my_profile.out")
# #print(summary)
# 
# 
# 
# fit.rest <- restriktor(fit1.lm, myConstraints1, mix_weights = "boot",
#                        # argumenten die doorgevoerd worden naar de truncated
#                        # mvnorm functie, zie ?rtmvnorm (pacakge rtmvnorm)
#                        lower =rep(-1, length = length(coef(fit1.lm))), 
#                        upper =rep( 2, length = length(coef(fit1.lm))),
#                        # control argumenten als mix_weights = "boot", indien je
#                        # de default wilt aanpassen
#                        control = list(mix_weights_bootstrap_limit = 1e5,
#                                       convergence_crit = 1e-03,
#                                       chunk_size = 5000))
# fit.rest$wt.bar
# 
# DATA1 <- subset(ZelazoKolb1972, Group != "Control")
# fit1.lm <- lm(Age ~ -1 + Group, data = DATA1)
# myConstraints1 <- ' GroupActive < GroupPassive < GroupNo '
# 
# fit.goric <- goric(fit1.lm, hypotheses = list(myConstraints1), mix_weights = "boot",
#                    # argumenten die doorgevoerd worden naar de truncated
#                    # mvnorm functie, zie ?rtmvnorm (pacakge rtmvnorm)
#                    lower =rep(-1, length = length(coef(fit1.lm))), 
#                    upper =rep( 2, length = length(coef(fit1.lm))),
#                    # control argumenten als mix_weights = "boot", indien je
#                    # de default wilt aanpassen
#                    control = list(mix_weights_bootstrap_limit = 1e5,
#                                   convergence_crit = 1e-03,
#                                   chunk_size = 5000))
# fit.goric$objectList$H1$wt.bar
# 
# 
# # -------------------------------------------------------------------------
# 
# ## Example 1 - 4 studies
# est_1 <- c(beta1 = 0.09)
# est_2 <- c(beta1 = 0.14)
# est_3 <- c(beta1 = 1.09)
# est_4 <- c(beta1 = 1.781)
# Param_studies <- list(est_1, est_2, est_3, est_4)
# 
# # standard error of the beta's (from the primary studies)
# vcov_est_1 <- matrix(c(0.029^2), nrow = 1)
# vcov_est_2 <- matrix(c(0.054^2), nrow = 1)
# vcov_est_3 <- matrix(c(0.093^2), nrow = 1)
# vcov_est_4 <- matrix(c(0.179^2), nrow = 1)
# CovMx_studies <- list(vcov_est_1, vcov_est_2, vcov_est_3, vcov_est_4)
# 
# # Set of hypotheses for each study
# # Note: in this case the same for each study
# H0   <- "beta1 = 0"
# Hpos <- "beta1 > 0"
# Hneg <- "beta1 < 0"
# hypotheses <- list(H0 = H0, Hpos = Hpos, Hneg = Hneg)
# 
# # Since this covers the whole space / covers all theories, we do not need a safeguard-hypothesis:
# comparison <- "none"
# 
# evS4_added <- evSyn(object = Param_studies, VCOV = CovMx_studies, 
#                     hypotheses = hypotheses,
#                     type = "added", 
#                     comparison = "none",
#                     control = list(convergence_crit = 1e-01), 
#                     mix_weights = "boot", seed = 1)
# evS4_added
