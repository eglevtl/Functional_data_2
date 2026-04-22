# One sample pointwise t-test

Z <- function(x, t.seq, mu, alpha=0.05) {
  if(class(x) != "fd") stop("X must be fd object")
  k <- length(t.seq)
  
  mu.x <- mean.fd(x)
  n <- dim(x$coef)[2]
  delta <- mu.x - mu
  gamma <- sd.fd(x)
  gamma.t <- eval.fd(t.seq, gamma)
  delta.t <- eval.fd(t.seq, delta)
  Zpoint <- sqrt(n)*delta.t/gamma.t
  
  crit.values <- qt(1-alpha/2, n-1)
  
  confintlower <- mu.x - crit.values*gamma*sqrt(1/n)
  confintupper <- mu.x + crit.values*gamma*sqrt(1/n)
  
  opar <- par(mfrow=c(2,1))
  plot(t.seq, Zpoint, type="l", xlab = 'Time', ylab = "Z statistics",
       main = "One sample t-test")
  lines(t.seq, rep(crit.values, k), lty=2, lwd=2, col="blue")
  lines(t.seq, rep(-crit.values, k), lty=2, lwd=2, col="blue")
  
  maxx <- max(cbind(eval.fd(t.seq, mu.x), eval.fd(t.seq, confintupper)))
  minn <- min(cbind(eval.fd(t.seq, mu.x), eval.fd(t.seq, confintlower)))
  
  plot(mu.x, xlab = "Time", ylab = "Mean value", ylim=c(minn-0.5, maxx+0.5))
  lines(confintlower, col=2, lty=2)
  lines(confintupper, col=2, lty=2)
  par(opar)
  
  return(list(statistics.pointwise = Zpoint,
              critical.value = crit.values,
              mean.est = mu.x, sd.est = gamma,
              lower.int = confintlower, upper.int = confintupper))
}
