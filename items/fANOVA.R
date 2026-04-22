###########################################################
#### Packages                                          ####
###########################################################

library(dplyr)
library(fda)
library(fdANOVA)

###########################################################
#### Pointwise fANOVA function                         ####
###########################################################


fANOVA.pointwise <- function(data, groups, t.seq, alpha=0.05) {
  # data is matrix with time in rows and variables in columns
  # group is a list names separating columns into different groups, a factor
  # time scale for measures
  n <- nrow(data)
  pvals <- numeric(n)
  lv <- levels(groups)
  k <- length(lv)
  mean.p <- matrix(NA, ncol=k, nrow=n)
  perm <- factorial(k)/(factorial(2)*(factorial(k-2)))
  Tukey.posthoc <- matrix(NA, ncol=perm, nrow=n)
  for(i in 1:n) {
    dt <- data.frame((data[i,]), groups)
    names(dt) <- c("values", "groups")
    av <- aov(values~groups, data = dt)
    pvals[i] <- summary(av)[[1]]["Pr(>F)"][1,1]
    mean.p[i,]  <- as.matrix((dt %>% group_by(groups) %>% summarise(mean(values)))[,2])
    colnames(Tukey.posthoc) <- rownames(TukeyHSD(av)$groups)
    Tukey.posthoc[i,] <- TukeyHSD(av)$groups[,4]
  }
  
  overall_mean <- apply(data, 1, mean)
  
  opar1 <- par(mfrow=c(2,1))
  
  plot(t.seq, pvals, type="l", main = "Pointwise ANOVA p-values",
       xlab = "Time", ylab="p-value", ylim=c(0,1))
  lines(t.seq, rep(0.05, n), col="blue", lty=2)
  
  mn <- min(mean.p, overall_mean)
  mx <- max(mean.p, overall_mean)
  
  plot(t.seq, overall_mean, type = "l", main = "Group means",
       xlab = "Time", ylab = "Mean", ylim = c(mn-0.05, mx+0.05))
  for(i in 1:k) {
    lines(t.seq, mean.p[,i], col=i+1, lty=i+1)
  }
  
  legend("topright", legend=c("Overall", lv), lty=1:(k+1), col=1:(k+1), title="Group")
  
  par(opar1)
  
  
  opar2 <- par(mfrow=c(1,1), ask = TRUE)
  
  for(i in 1:perm) {
    plot(t.seq, Tukey.posthoc[,i], type="l", main = paste("Tukey HSD p-values", rownames(TukeyHSD(av)$groups)[i]),
         xlab = "Time", ylab = "p-value", ylim = c(0,1))
    lines(t.seq, rep(0.05, n), col="blue", lty=2)
  }
  
  par(opar2)
  
  return(list(p.values=pvals, TukeyHSD=Tukey.posthoc, gr.means = mean.p, overal.mean=overall_mean))
} 

###########################################################
#### Generated data                                    ####
###########################################################

epsilon <- cbind(matrix(rnorm(5000), ncol=20), 
                 matrix(rnorm(3000, 2, 1), ncol=12),
                 matrix(rnorm(4000, 1, 1), ncol=16))
names(epsilon) <- paste("col", 1:48, sep="")

tt <- seq(0,1, length=250)
cof <- replicate(250, rexp(1, 2))
mydata <- cos(pi*tt*cof) + epsilon

gr <- factor(c(rep('A', 20), rep("B", 12), rep("C", 16)))

dev.new()
matplot(mydata, type="l")

dev.new()
fANOVA.pointwise(data=mydata, groups=gr, t.seq=tt, alpha=0.05) 

###########################################################
#### Gait data                                         ####
###########################################################

?gait 

dev.new()
matplot(gait[,,1], type="l") # Hip angle
dev.new()
matplot(gait[,,2], type="l") # Knee angle

group.label.gait <- factor(rep(1:3, each = 13))
dev.new()
fANOVA.pointwise(data=gait[,,1], groups=group.label.gait, 
                 t.seq=as.numeric(rownames(gait[,,1])), alpha=0.05)

dev.new()
fANOVA.pointwise(data=gait[,,2], groups=group.label.gait, 
                 t.seq=as.numeric(rownames(gait[,,1])), alpha=0.05) 


## Functional data

gaittime  <- (1:20)/21
gaitrange <- c(0,1)
gaitbasis <- create.fourier.basis(gaitrange,21)
lambda    <- 10^(-11.5)
harmaccelLfd <- vec2Lfd(c(0, 0, (2*pi)^2, 0))
gaitfdPar <- fdPar(gaitbasis, harmaccelLfd, lambda)
gaitSmooth <- smooth.basis(gaittime, gait[,,1], gaitfdPar)
gaitfd <- gaitSmooth$fd

dev.new()
plot(gaitfd)
dev.new()
plotfit.fd(gait[,,1], gaittime, gaitfd)

t.new <- seq(0,1, length=1001)
gait.eval <- eval.fd(t.new, gaitfd)

dev.new()
fANOVA.pointwise(data=gait.eval, groups=group.label.gait, 
                 t.seq=t.new, alpha=0.05) 

###########################################################
#### Canadian weather data                             ####
###########################################################

group.label <- as.factor(CanadianWeather$region)
tempdata <- CanadianWeather$dailyAv[,,1]

dev.new()
matplot(tempdata, type="l")

dev.new()
fANOVA.pointwise(data=tempdata, groups=group.label, 
                 t.seq=1:365, alpha=0.05) 


## Functional data

daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65)

#  -----------  set up the harmonic acceleration operator  ----------

harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))

#  ---------  create fd objects for temp. and prec. ---------------

daytempfd <- with(CanadianWeather, smooth.basis(day.5,
                                                dailyAv[,,"Temperature.C"],
                                                daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd )
dev.new()
plot(daytempfd, axes=FALSE)
axisIntervals(1)
axis(2)

t.new2 <- seq(0, 365, length=1001)
temp.eval <- eval.fd(t.new2, daytempfd)

dev.new()
fANOVA.pointwise(data=temp.eval, groups=group.label, 
                 t.seq=t.new2, alpha=0.05) 




##########################################################
#### Functional ANOVA and MANOVA                      ####
##########################################################

##########################################################
#### Graphs                                           ####
##########################################################

### generated data

dev.new()
plotFANOVA(x = mydata, int = c(0, 1))
plotFANOVA(x = mydata, group.label = as.character(gr),
           int = c(0, 1))
plotFANOVA(x = mydata, group.label = as.character(gr),
           int = c(0, 1), separately = TRUE)
plotFANOVA(x = mydata, group.label = as.character(gr),
           int = c(0, 1), means = TRUE)

### gait data
dev.new()
plotFANOVA(x = gait[,,1], int = c(0.025, 0.975))
plotFANOVA(x = gait[,,1], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975))
plotFANOVA(x = gait[,,1], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975), separately = TRUE)
plotFANOVA(x = gait[,,1], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975), means = TRUE)

dev.new()
plotFANOVA(x = gait[,,2], int = c(0.025, 0.975))
plotFANOVA(x = gait[,,2], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975))
plotFANOVA(x = gait[,,2], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975), separately = TRUE)
plotFANOVA(x = gait[,,2], group.label = as.character(group.label.gait),
           int = c(0.025, 0.975), means = TRUE)

### weather data
dev.new()
plotFANOVA(x = tempdata, int = c(1, 365))
plotFANOVA(x = tempdata, group.label = as.character(group.label),
           int = c(1, 365))
plotFANOVA(x = tempdata, group.label = as.character(group.label),
           int = c(1, 365), separately = TRUE)
plotFANOVA(x = tempdata, group.label = as.character(group.label),
           int = c(1, 365), means = TRUE)

##########################################################
#### fANOVA                                           ####
##########################################################
set.seed(123)
(fanova1 <- fanova.tests(x = mydata, group.label = gr))
(fanova2 <- fanova.tests(x = gait[,,1], group.label = group.label.gait))
#(fanova2.fd <- fanova.tests(x = gait.eval, group.label = group.label.gait, parallel = TRUE))
#(fanova3 <- fanova.tests(x = gait[,,2], group.label = group.label.gait))
#(fanova4 <- fanova.tests(x = tempdata, group.label = group.label))
(fanova4.fd <- fanova.tests(x = temp.eval, group.label = group.label, 
                            parallel = TRUE))

### Functional basis for FP type test

## Gait data

own.basis <- gaitfd$coefs
own.cross.prod.mat <- inprod(gaitbasis, gaitbasis)

set.seed(123)
fanova.tests(gait[,,1], group.label.gait,
             test = c("FP", "GPF", "Fmaxb", "TRP"),
             params = list(paramFP = list(B.FP = 1000, basis = "own",
                                          own.basis = own.basis,
                                          own.cross.prod.mat =
                                            own.cross.prod.mat),
                           paramFmaxb = 1000,
                           paramTRP = list(k = c(10, 15),
                                           projection = "BM",
                                           permutation = TRUE,
                                           B.TRP = 1000)))
## Temperature data

own.basis.t <- daytempfd$coefs
own.cross.prod.mat.t <- inprod(daybasis65, daybasis65)

set.seed(123)

fanova.tests(temp.eval, group.label,
             test = c("FP", "GPF", "Fmaxb", "TRP"),
             params = list(paramFP = list(B.FP = 1000, basis = "own",
                                          own.basis = own.basis.t,
                                          own.cross.prod.mat =
                                            own.cross.prod.mat.t),
                           paramFmaxb = 1000,
                           paramTRP = list(k = c(10, 15),
                                           projection = "BM",
                                           permutation = TRUE,
                                           B.TRP = 1000)))

##########################################################
#### fMANOVA                                          ####
##########################################################

## gait data

set.seed(123)

x.gait <- list()
x.gait[[1]] <- gait[,,1]
x.gait[[2]] <- gait[,,2]

fmanova <- fmanova.ptbfr(x.gait, group.label.gait, int = c(0.025, 0.975),
                         B = 5000, basis = "Fourier", criterion = "eBIC",
                         commonK = "mean", minK = 5, maxK = 19, gamma.eBIC = 0.7)
summary(fmanova)

## Smoothing to functional data and applying fMANOVA
gaitSmooth.m <- smooth.basis(gaittime, gait, gaitfdPar)
gaitfd.m <- gaitSmooth.m$fd

dev.new()
plot(gaitfd.m)
plotfit.fd(gait, gaittime, gaitfd.m)

own.basis <- list()
own.basis[[1]] <- gaitfd.m$coefs[,,1]
own.basis[[2]] <- gaitfd.m$coefs[,,2]
own.cross.prod.mat <- inprod(gaitbasis, gaitbasis)

fmanova <- fmanova.ptbfr(x.gait, group.label.gait, int = c(0.025, 0.975),
                         B = 5000, basis = "own", 
                         own.basis = own.basis,
                         own.cross.prod.mat = own.cross.prod.mat,
                         criterion = "eBIC", commonK = "mean")
summary(fmanova)

## Canadian weather data

x.weather <- list()
x.weather[[1]] <- CanadianWeather$dailyAv[,,1] # temperature
x.weather[[2]] <- CanadianWeather$dailyAv[,,2] # precipitation

fmanova.w <- fmanova.ptbfr(x.weather, group.label, int = c(0.5, 364.5),
                           B = 5000, basis = "Fourier", criterion = "eBIC",
                           commonK = "mean", minK = 5, maxK = 19, gamma.eBIC = 0.7)
summary(fmanova.w)

## Smoothing to functional data and applying fMANOVA


weather1 <- smooth.basis(day.5, CanadianWeather$dailyAv[,,1], daybasis65)$fd
weather2 <- smooth.basis(day.5, CanadianWeather$dailyAv[,,2], daybasis65)$fd

dev.new()
plot(weather1)
plot(weather2)

own.basis.t <- list()
own.basis.t[[1]] <- weather1$coefs
own.basis.t[[2]] <- weather2$coefs
own.cross.prod.mat.t <- inprod(daybasis65, daybasis65)

fmanova.w <- fmanova.ptbfr(x.weather, group.label, int = c(0.5, 364.5),
                           B = 5000, basis = "own", 
                           own.basis = own.basis.t,
                           own.cross.prod.mat = own.cross.prod.mat.t,
                           criterion = "eBIC", commonK = "mean")
summary(fmanova.w)



####Random projection based tests - independent way

set.seed(123)
fmanova1w <- fmanova.trp(x.weather, group.label, k = c(1, 5, 10, 15, 20))
fmanova2w <- fmanova.trp(x.weather, group.label, k = c(1, 5, 10, 15, 20),
                         permutation = TRUE)

dev.new()
plot(x = fmanova1w, y = fmanova2w)
dev.new()
plot(x = fmanova1w, y = fmanova2w, withoutRoy = TRUE)


####Random projection based tests - dependent way

set.seed(123)
fmanova3 <- fmanova.trp(x.gait, group.label.gait, k = c(1, 5, 10, 15, 20),
                        independent.projection.tests = FALSE)
fmanova4 <- fmanova.trp(x.gait, group.label.gait, k = c(1, 5, 10, 15, 20),
                        permutation = TRUE,
                        independent.projection.tests = FALSE)

dev.new()
plot(x = fmanova3, y = fmanova4)
