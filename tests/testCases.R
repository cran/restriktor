## test constrained lm, rlm, glm
# mixing weights
# bootstrap standard errors
# models with and without intercept
# case weights
# constraints on factors and interactions
library(restriktor)
n <- 100
p <- 4
betas = c(.1,.2,.3,.4,.5)
set.seed(3013073)
X <- cbind(mvtnorm:::rmvnorm(n, mean=rep(0,p), sigma=diag(p)), rbinom(n,1,0.5))
colnames(X) <- c("x1","x2","x3","x4","f1")
z <- X %*% betas        
y <- z + rnorm(n)
DATA <- data.frame(y, X)

# intercept model
model1 <- y ~  1 + x1 + x2 + x3 + x4
# no intercept model
model2 <- y ~ -1 + x1 + x2 + x3 + x4
# intercept model with interaction
model3 <- y ~ 1 + x1*f1 + x2*f1 + x3*f1 + x4*f1
# no intercept model with interaction
model4 <- y ~ -1 + x1*f1 + x2*f1 + x3*f1 + x4*f1

############################ lm #################################
linmod1 <- lm(model1, data = DATA)
linmod2 <- lm(model2, data = DATA)
linmod3 <- lm(model3, data = DATA)
linmod1wt <- lm(model1, data = DATA, weights = abs(rnorm(n)))
linmod2wt <- lm(model2, data = DATA, weights = abs(rnorm(n)))
linmod1fac <- lm(model3, data = DATA)
linmod2fac <- lm(model4, data = DATA)

# check normal functionality mixing weights and compare with bootstrapped mixing weights
restr1a <- restriktor(linmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none")
restr1b <- restriktor(linmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none", 
                      mix.weights = "boot", mix.bootstrap = 9999, seed = 123,
                      parallel = "multicore", ncpus = 2)
if (!all(abs(restr1a$wt.bar - restr1b$wt.bar) < .01)) {
  stop("mixing weights are not approx. equal")
}

## check normal functionality robust standard errors
restr1 <- restriktor(linmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr1)
restr2 <- restriktor(linmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr2)
## check normal functionality standard bootstrap
restr3 <- restriktor(linmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr3)
restr4 <- restriktor(linmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr4)
restr5 <- restriktor(linmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr5)
restr6 <- restriktor(linmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2 )
summary(restr6)
restr7 <- restriktor(linmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr7)
restr8 <- restriktor(linmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr8)
## check normal functionality model-based bootstrap
restr9 <- restriktor(linmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr9)
restr10 <- restriktor(linmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr10)

restr11 <- restriktor(linmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr11)
restr12 <- restriktor(linmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr12)
restr13 <- restriktor(linmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr13)
restr14 <- restriktor(linmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr14)

tests <- c("F","LRT","score")
for (test in tests) {
  out1 <- iht(restr1, test = test)
  out1
  if (is.na(out1$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$B$pvalue[1])) stop("pvalue is NA!")
  
  out2 <- iht(restr2, test = test)
  out2
  if (is.na(out2$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$B$pvalue[1])) stop("pvalue is NA!")
  
  out3 <- iht(restr3, test = test)
  out3
  if (is.na(out3$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$B$pvalue[1])) stop("pvalue is NA!")
  
  out4 <- iht(restr4, test = test)
  out4
  if (is.na(out4$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$B$pvalue[1])) stop("pvalue is NA!")
  
  out5 <- iht(restr1, test = test)
  out5
  if (is.na(out5$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$B$pvalue[1])) stop("pvalue is NA!")
  
  out6 <- iht(restr2, test = test)
  out6
  if (is.na(out6$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$B$pvalue[1])) stop("pvalue is NA!")
  
  out7 <- iht(restr1, test = test)
  out7
  if (is.na(out7$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$B$pvalue[1])) stop("pvalue is NA!")
  
  out8 <- iht(restr2, test = test)
  out8
  if (is.na(out8$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$B$pvalue[1])) stop("pvalue is NA!")
  
  out9 <- iht(restr1, test = test)
  out9
  if (is.na(out9$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$B$pvalue[1])) stop("pvalue is NA!")
  
  out10 <- iht(restr2, test = test)
  out10
  if (is.na(out10$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$B$pvalue[1])) stop("pvalue is NA!")
  
  out11 <- iht(restr1, test = test)
  out11
  if (is.na(out11$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$B$pvalue[1])) stop("pvalue is NA!")
  
  out12 <- iht(restr2, test = test)
  out12
  if (is.na(out12$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$B$pvalue[1])) stop("pvalue is NA!")
  
  out13 <- iht(restr1, test = test)
  out13
  if (is.na(out13$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$B$pvalue[1])) stop("pvalue is NA!")
  
  out14 <- iht(restr2, test = test)
  out14
  if (is.na(out14$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$B$pvalue[1])) stop("pvalue is NA!")
}


# check functionality if equality constraints only
restr1 <- restriktor(linmod1, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod2, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod3, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod1wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod2wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod1fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(linmod2fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")

########################### rlm #################################
library(MASS)
rlinmod1 <- rlm(model1, data = DATA, method = "MM", maxit = 500)
rlinmod2 <- rlm(model2, data = DATA, method = "MM", maxit = 500)
rlinmod3 <- rlm(model3, data = DATA, method = "MM", maxit = 500)
rlinmod1wt <- rlm(model1, data = DATA, weights = abs(rnorm(n)), method = "MM", maxit = 500)
rlinmod2wt <- rlm(model2, data = DATA, weights = abs(rnorm(n)), method = "MM", maxit = 500)
rlinmod1fac <- rlm(model3, data = DATA, method = "MM", maxit = 500)
rlinmod2fac <- rlm(model4, data = DATA, method = "MM", maxit = 500)


# check normal functionality mixing weights and compare with bootstrapped mixing weights
restr1a <- restriktor(rlinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none")
restr1b <- restriktor(rlinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none", 
                      mix.weights = "boot", mix.bootstrap = 9999, seed = 123,
                      parallel = "multicore", ncpus = 2)
if (!all(abs(restr1a$wt.bar - restr1b$wt.bar) < .01)) {
  stop("mixing weights are not approx. equal")
}

## check normal functionality robust standard errors
restr1 <- restriktor(rlinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr1)
restr2 <- restriktor(rlinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr2)
## check normal functionality standard bootstrap
restr3 <- restriktor(rlinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr3)
restr4 <- restriktor(rlinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr4)
restr5 <- restriktor(rlinmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr5)
restr6 <- restriktor(rlinmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2 )
summary(restr6)
restr7 <- restriktor(rlinmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr7)
restr8 <- restriktor(rlinmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr8)
## check normal functionality model-based bootstrap
restr9 <- restriktor(rlinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr9)
restr10 <- restriktor(rlinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr10)

restr11 <- restriktor(rlinmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr11)
restr12 <- restriktor(rlinmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr12)
restr13 <- restriktor(rlinmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr13)
restr14 <- restriktor(rlinmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                      se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr14)

tests <- c("F","Wald","Wald2","score")
for (test in tests) {
  out1 <- iht(restr1, test = test)
  out1
  if (is.na(out1$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$B$pvalue[1])) stop("pvalue is NA!")
  
  out2 <- iht(restr2, test = test)
  out2
  if (is.na(out2$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$B$pvalue[1])) stop("pvalue is NA!")
  
  out3 <- iht(restr3, test = test)
  out3
  if (is.na(out3$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$B$pvalue[1])) stop("pvalue is NA!")
  
  out4 <- iht(restr4, test = test)
  out4
  if (is.na(out4$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$B$pvalue[1])) stop("pvalue is NA!")
  
  out5 <- iht(restr1, test = test)
  out5
  if (is.na(out5$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$B$pvalue[1])) stop("pvalue is NA!")
  
  out6 <- iht(restr2, test = test)
  out6
  if (is.na(out6$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$B$pvalue[1])) stop("pvalue is NA!")
  
  out7 <- iht(restr1, test = test)
  out7
  if (is.na(out7$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$B$pvalue[1])) stop("pvalue is NA!")
  
  out8 <- iht(restr2, test = test)
  out8
  if (is.na(out8$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$B$pvalue[1])) stop("pvalue is NA!")
  
  out9 <- iht(restr1, test = test)
  out9
  if (is.na(out9$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$B$pvalue[1])) stop("pvalue is NA!")
  
  out10 <- iht(restr2, test = test)
  out10
  if (is.na(out10$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$B$pvalue[1])) stop("pvalue is NA!")
  
  out11 <- iht(restr1, test = test)
  out11
  if (is.na(out11$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$B$pvalue[1])) stop("pvalue is NA!")
  
  out12 <- iht(restr2, test = test)
  out12
  if (is.na(out12$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$B$pvalue[1])) stop("pvalue is NA!")
  
  out13 <- iht(restr1, test = test)
  out13
  if (is.na(out13$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$B$pvalue[1])) stop("pvalue is NA!")
  
  out14 <- iht(restr2, test = test)
  out14
  if (is.na(out14$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$B$pvalue[1])) stop("pvalue is NA!")
}


# check functionality if equality constraints only
restr1 <- restriktor(rlinmod1, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod2, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod3, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod1wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod2wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod1fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(rlinmod2fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")

########################### glm #################################
glinmod1 <- glm(model1, data = DATA)
glinmod2 <- glm(model2, data = DATA)
glinmod3 <- glm(model3, data = DATA)
glinmod1wt <- glm(model1, data = DATA, weights = abs(rnorm(n)))
glinmod2wt <- glm(model2, data = DATA, weights = abs(rnorm(n)))
glinmod1fac <- glm(model3, data = DATA)
glinmod2fac <- glm(model4, data = DATA)


# check normal functionality mixing weights and compare with bootstrapped mixing weights
restr1a <- restriktor(glinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none")
restr1b <- restriktor(glinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", se = "none", 
                      mix.weights = "boot", mix.bootstrap = 9999, seed = 123,
                      parallel = "multicore", ncpus = 2)
if (!all(abs(restr1a$wt.bar - restr1b$wt.bar) < .01)) {
  stop("mixing weights are not approx. equal")
}

## check normal functionality robust standard errors
restr1 <- restriktor(glinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr1)
restr2 <- restriktor(glinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "HC0", mix.weights = "pmvnorm")
summary(restr2)
## check normal functionality standard bootstrap
restr3 <- restriktor(glinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr3)
restr4 <- restriktor(glinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr4)
restr5 <- restriktor(glinmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr5)
restr6 <- restriktor(glinmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2 )
summary(restr6)
restr7 <- restriktor(glinmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr7)
restr8 <- restriktor(glinmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                     se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr8)
## check normal functionality model-based bootstrap
restr9 <- restriktor(glinmod1, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                     se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr9)
restr10 <- restriktor(glinmod2, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr10)

restr11 <- restriktor(glinmod1wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr11)
restr12 <- restriktor(glinmod2wt, constraints = "x2 > 0; x3 > 0; x4 == 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr12)
restr13 <- restriktor(glinmod1fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                      se = "boot.model.based", B = 50, parallel = "multicore", ncpus = 2)
summary(restr13)
restr14 <- restriktor(glinmod2fac, constraints = "x2 > 0; x3 > 0; x4 == 0; f1.x3 < 0", 
                      se = "boot.standard", B = 50, parallel = "multicore", ncpus = 2)
summary(restr14)

tests <- c("F","LRT","score")
for (test in tests) {
  out1 <- iht(restr1, test = test)
  out1
  if (is.na(out1$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out1$B$pvalue[1])) stop("pvalue is NA!")
  
  out2 <- iht(restr2, test = test)
  out2
  if (is.na(out2$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out2$B$pvalue[1])) stop("pvalue is NA!")
  
  out3 <- iht(restr3, test = test)
  out3
  if (is.na(out3$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out3$B$pvalue[1])) stop("pvalue is NA!")
  
  out4 <- iht(restr4, test = test)
  out4
  if (is.na(out4$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out4$B$pvalue[1])) stop("pvalue is NA!")
  
  out5 <- iht(restr1, test = test)
  out5
  if (is.na(out5$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out5$B$pvalue[1])) stop("pvalue is NA!")
  
  out6 <- iht(restr2, test = test)
  out6
  if (is.na(out6$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out6$B$pvalue[1])) stop("pvalue is NA!")
  
  out7 <- iht(restr1, test = test)
  out7
  if (is.na(out7$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out7$B$pvalue[1])) stop("pvalue is NA!")
  
  out8 <- iht(restr2, test = test)
  out8
  if (is.na(out8$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out8$B$pvalue[1])) stop("pvalue is NA!")
  
  out9 <- iht(restr1, test = test)
  out9
  if (is.na(out9$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out9$B$pvalue[1])) stop("pvalue is NA!")
  
  out10 <- iht(restr2, test = test)
  out10
  if (is.na(out10$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out10$B$pvalue[1])) stop("pvalue is NA!")
  
  out11 <- iht(restr1, test = test)
  out11
  if (is.na(out11$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out11$B$pvalue[1])) stop("pvalue is NA!")
  
  out12 <- iht(restr2, test = test)
  out12
  if (is.na(out12$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out12$B$pvalue[1])) stop("pvalue is NA!")
  
  out13 <- iht(restr1, test = test)
  out13
  if (is.na(out13$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out13$B$pvalue[1])) stop("pvalue is NA!")
  
  out14 <- iht(restr2, test = test)
  out14
  if (is.na(out14$global$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$A$pvalue[1])) stop("pvalue is NA!")
  if (is.na(out14$B$pvalue[1])) stop("pvalue is NA!")
}


# check functionality if equality constraints only
restr1 <- restriktor(glinmod1, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod2, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod3, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod1wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod2wt, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod1fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
restr1 <- restriktor(glinmod2fac, constraints = "x2 == 0; x3 == 0; x4 == 0", se = "HC")
summary(restr1)
out1 <- iht(restr1)
out1
if (is.na(out1$pvalue[1])) stop("pvalue is NA!")
