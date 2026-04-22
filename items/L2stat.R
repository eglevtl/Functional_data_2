# # One sample L2 norma based bootstrap test

L2.stat <- function(x, t.seq, mu0, replication=100, method = 1:2) {
  # method = 1: T stat
  # method = 2: Bootsrap
  if(class(x) != "fd") stop("X must be fd object")
  mu.x <- mean.fd(x)
  n <- dim(x$coefs)[2]
  k <- length(t.seq)
  
  mu.x.t <- eval.fd(t.seq, mu.x)
  mu0.t <- eval.fd(t.seq, mu0)
  
  z <- center.fd(x)
  z.t <- eval.fd(t.seq, z)
  
  F.s <- n * t(mu.x.t-mu0.t) %*% (mu.x.t-mu0.t)
  F.s <- F.s[1]
  
  if(n > k) {
    Sigma <- (t(z.t) %*% z.t)/(n-1)
  } else {
    Sigma <- (z.t %*% t(z.t))/(n-1)
  }
  
  A <- trace(Sigma)
  B <- trace(Sigma^2)
  
  btFstat <-  numeric(replication)
  
  if(method == 1) { #naive method
    A2 <- A^2
    B2 <- B
    alp <- B2/A
    df <- A2/B2
    pvalue <- 1-pchisq(F.s/alp, df)
    params <- list(alpha = alp, df = df)
  } 
  if(method == 2) {  #bootstrapping method
    for(i in 1:replication) {
      btfflag <- sample(1:n,size = n, replace = TRUE)
      xstar.coefs <- x$coefs[,btfflag]
      xstar.names <- x$fdnames
      xstar.names$reps <- btfflag
      xstar.fd <- fd(xstar.coefs, x$basis, xstar.names)
      btx <- eval.fd(t.seq, xstar.fd)
      btmu <- apply(btx,1,mean) - mu.x.t
      btFstat[i] <- n * t(btmu) %*% btmu
    }
    pvalue <- mean(btFstat>=F.s)
    params <- list(btFstat)
  }
  return(list(statistics = F.s, pvalue = pvalue, params=params))
}
