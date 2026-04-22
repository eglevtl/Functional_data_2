# One sample pointwise bootstrap t-test
Z.boot <- function(x, t.seq, mu, replication=100, alpha=0.05) {
  if(class(x) != "fd") stop("X must be fd object")
  mu.x <- mean.fd(x)
  n <- dim(x$coefs)[2]
  k <- length(t.seq)
  Z.star <- matrix(NA, ncol=replication, nrow = k)
  
  for(i in 1:replication) {
    rep <- sample(1:n, n, replace = TRUE)
    xstar.coefs <- x$coefs[,rep]
    xstar.names <- x$fdnames
    xstar.names$reps <- rep
    xstar.fd <- fd(xstar.coefs, x$basis, xstar.names)
    mu.star <- mean.fd(xstar.fd)
    sd.star <- sd.fd(xstar.fd)
    delta <- mu.star - mu.x
    delta.t <- eval.fd(t.seq, delta)
    sd.star.t <- eval.fd(t.seq, sd.star)
    Z.star[,i] <- sqrt(n)*delta.t/sd.star.t
  }
  
  crit.val <- numeric(k)
  for(i in 1:k) {
    Z.sort <- sort(Z.star[i,])
    crit.val[i] <- quantile(Z.sort, 1-alpha/2)
  }
  
  delta.stat <- mu.x - mu
  gamma.stat <- sd.fd(x)
  gamma.stat.t <- eval.fd(t.seq, gamma.stat)
  delta.stat.t <- eval.fd(t.seq, delta.stat)
  Z <- sqrt(n)*delta.stat.t/gamma.stat.t
  
  mu.x.t <- eval.fd(t.seq, mu.x)
  
  confintlower <- mu.x.t - crit.val*gamma.stat.t*sqrt(1/n)
  confintupper <- mu.x.t + crit.val*gamma.stat.t*sqrt(1/n)
  
  opar <- par(mfrow=c(2,1))
  
  mx <- max(cbind(Z, crit.val))
  mn <- min(cbind(Z, -crit.val))
  
  plot(t.seq, Z, type="l", xlab = 'Time', ylab = "Z statistics",
       main = "One sample t-test", ylim=c(mn-0.5, mx+0.5))
  lines(t.seq, crit.val, lty=2, lwd=2, col="blue")
  lines(t.seq, -crit.val, lty=2, lwd=2, col="blue")
  
  maxx <- max(cbind(mu.x.t, confintupper))
  minn <- min(cbind(mu.x.t, confintlower))
  
  plot(mu.x, xlab = "Time", ylab = "Mean value", ylim=c(minn-0.5, maxx+0.5))
  lines(t.seq, confintlower, col=2, lty=2)
  lines(t.seq, confintupper, col=2, lty=2)
  par(opar)
  
  return(list(statistics = Z, critical.value = crit.val,
              mean.est = mu.x, sd.est = gamma, z.star = Z.star,
              lower.int = confintlower, upper.int = confintupper))
}
