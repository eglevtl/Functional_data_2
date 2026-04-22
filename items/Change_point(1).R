############################################################
#### Load packages                                     #####
############################################################

library(sde)
library(fda)
library(fda.usc)

############################################################
#### Generate errors                                   #####
############################################################

n <- 301 # discretization length
m <- 200 # sample size

### Brownian bridge
br.bridge <- replicate(m, BBridge(x=0, y=0, t0=0, T=1, N=100))
matplot(br.bridge, type="l")
bb <- create.fourier.basis(c(0,1), 25)
epsilonA <- smooth.basis((0:100)/100, br.bridge, bb)$fd
plot(epsilonA)

### bspline basis
q <- 11
bbasis <- create.bspline.basis(c(0,1), nbasis=q)
plot(bbasis)
coeffs <- replicate(m, rexp(q, 2))
epsilonB <- fd(coeffs, bbasis)
plot(epsilonB)  

############################################################
#### Mean functions                                    #####
############################################################

a <- 5
p <- 3
b <- 4
w <- 2
t.seq <- seq(0,1, length=n)

m1 <- fdata2fd(fdata(a*t.seq, t.seq))
m2 <- fdata2fd(fdata(sin(p * pi * t.seq), t.seq))
m3 <- fdata2fd(fdata(b * sqrt(t.seq), t.seq))
m4 <- fdata2fd(fdata(w * exp(t.seq), t.seq))

opar <- par(mfrow=c(2,2))
plot(m1)
plot(m2)
plot(m3)
plot(m4)
par(opar)

###########################################################
#### Processes with Brownian bridge noise              ####
###########################################################

X1a <- m1 + epsilonA
plot(X1a)
X2a <- m2 + epsilonA
plot(X2a)
X3a <- m3 + epsilonA
plot(X3a)
X4a <- m4 + epsilonA
plot(X4a)

### subsample size

n1 <- 30
n2 <- 60 
n3 <- 40
n4 <- 70

Xa.coef <- cbind(X1a$coefs[,1:n1], X2a$coefs[,(n1+1):(n1+n2)], 
                 X3a$coefs[,(n1+n2+1):(n1+n2+n3)], X4a$coefs[,(n1+n2+n3+1):(n1+n2+n3+n4)])
Xa.sample <- fd(Xa.coef, X1a$basis)
plot(Xa.sample)


###########################################################
#### Processes with basis functions                    ####
###########################################################

X1b <- m1 + epsilonB
plot(X1b)
X2b <- m2 + epsilonB
plot(X2b)
X3b <- m3 + epsilonB
plot(X3b)
X4b <- m4 + epsilonB
plot(X4b)

### subsample size

n1 <- 30
n2 <- 60 
n3 <- 40
n4 <- 70

Xb.coef <- cbind(X1b$coefs[,1:n1], X2b$coefs[,(n1+1):(n1+n2)], 
                 X3b$coefs[,(n1+n2+1):(n1+n2+n3)], X4b$coefs[,(n1+n2+n3+1):(n1+n2+n3+n4)])
Xb.sample <- fd(Xb.coef, X1b$basis)
plot(Xb.sample)

###########################################################
#### break points                                      ####
###########################################################

(break.points <- c(n1+1, n1+n2+1, n1+n2+n3+1))

###########################################################
#### fPCA                                              ####
###########################################################

pca.a <- pca.fd(Xa.sample, nharm = 1)
plot(pca.a)

pca.b <- pca.fd(Xb.sample, nharm = 1)
plot(pca.b)

### scores

xi.a <- pca.a$scores
plot(xi.a, type="l")
abline(v=break.points, col=2, lty=2)


xi.b <- pca.b$scores
plot(xi.b, type="l")
abline(v=break.points, col=2, lty=2)

###########################################################
#### non parametric change point                       ####
###########################################################

# https://cran.r-project.org/web/packages/ecp/vignettes/ecp.pdf

library(ecp)

output.a <- e.divisive(xi.a, R = 499, alpha = 2)
output.a$estimates
output.a$k.hat

plot(xi.a, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = output.a$estimates, col=4, lty=2)

output.b <- e.divisive(xi.b, R = 499, alpha = 2)
output.b$estimates
output.b$k.hat

plot(xi.b, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = output.b$estimates, col=4, lty=2)


output.a2 <- e.agglo(X = xi.a, alpha = 1)
output.a2$estimates

plot(xi.a, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = output.a$estimates, col=4, lty=2)
abline(v = output.a2$estimates, col=5, lty=2)


output.b2 <- e.agglo(X = xi.b, alpha = 1)
output.b2$estimates

plot(xi.b, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = output.b$estimates, col=4, lty=2)
abline(v = output.b2$estimates, col=5, lty=2)

###########################################################
#### envcpt package                                    ####
###########################################################

# https://cran.r-project.org/web/packages/EnvCpt/EnvCpt.pdf

library(EnvCpt)

###

models.a <- envcpt(xi.a, models=c("mean","meanar1","meanar2","meanar1cpt","meanar2cpt",
                      "trend","trendcpt","trendar1","trendar2","trendar1cpt","trendar2cpt"))

plot(models.a,type='fit') # plots the fits
plot(models.a,type="aic") # plots the aic values
plot(models.a,type="bic") # plots the bic values

which.min(AIC(models.a))
which.min(BIC(models.a))

models.a$trendcpt

plot(xi.a, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = c(30, 90, 130), col=5, lty=2)

###

models.b <- envcpt(xi.b, models=c("mean","meanar1","meanar2","meanar1cpt","meanar2cpt",
                                  "trend","trendcpt","trendar1","trendar2","trendar1cpt","trendar2cpt"))

plot(models.b,type='fit') # plots the fits
plot(models.b,type="aic") # plots the aic values
plot(models.b,type="bic") # plots the bic values

which.min(AIC(models.b))
which.min(BIC(models.b))

models.b$trendcpt

plot(xi.b, type="l")
abline(v=break.points, col=2, lty=2)
abline(v = c(30, 90, 129), col=5, lty=2)


###########################################################
#### change point package                              ####
###########################################################

# https://cran.r-project.org/web/packages/changepoint/changepoint.pdf

library(changepoint)

###

cpt.mean(as.vector(xi.a), method="AMOC") # at most one change point

cpt.mean(as.vector(xi.a), method="PELT") # multiple change points

cpt.mean(as.vector(xi.a), penalty = "BIC", method="SegNeigh") # multiple change points

cpt.mean(as.vector(xi.a), method="BinSeg") # multiple change points

###

cpt.mean(as.vector(xi.b), method="AMOC") # at most one change point

cpt.mean(as.vector(xi.b), method="PELT") # multiple change points

cpt.mean(as.vector(xi.b), penalty = "BIC", method="SegNeigh") # multiple change points

cpt.mean(as.vector(xi.b), method="BinSeg") # multiple change points

