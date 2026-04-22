# Two samples pointwise t-test

Ztwosample <- function(x, y, t.seq, alpha=0.05) {
  if(class(x) != "fd") stop("X must be fd object")
  if(class(y) != "fd") stop("Y must be fd object")
  k <- length(t.seq)
  
  mu.x <- mean.fd(x)
  mu.y <- mean.fd(y)
  
  n <- dim(x$coef)[2]
  m <- dim(x$coef)[2]
  
  delta <- (mu.x - mu.y)
  delta.t <- eval.fd(t.seq, delta)
  
  z.x <- center.fd(x)
  z.y <- center.fd(y)
  
  z.x.t <- eval.fd(t.seq, z.x)
  z.y.t <- eval.fd(t.seq, z.y)
  z.t <- cbind(z.x.t, z.y.t)
  
  if(n > k) {
    Sigma <- (t(z.t) %*% z.t)/(n-2)
  } else {
    Sigma <- (z.t %*% t(z.t))/(n-2)
  }
  
  gamma.t <- diag(Sigma)
  Zpointwise <- sqrt((n*m)/(n+m)) * delta.t/sqrt(gamma.t)

    crit <- qt(1-alpha/2, n-2)
    crit.val <- rep(crit, k)
    params <- list(critical.value = crit)

  
  mx <- max(cbind(Zpointwise, crit.val))
  mn <- min(cbind(Zpointwise, -crit.val))
  
  plot(t.seq, Zpointwise, type="l", xlab = 'Time', ylab = "Z statistics",
       main = "Two samples t-test", ylim=c(mn-0.5, mx+0.5))
  lines(t.seq, crit.val, lty=2, lwd=2, col="blue")
  lines(t.seq, -crit.val, lty=2, lwd=2, col="blue")
  
  
  return(list(statistics.pointwise = Zpointwise,
              params = params))
}
