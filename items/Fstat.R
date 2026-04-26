# # One sample F type bootstrap test

F.stat <- function(x, t.seq, mu0, replication=100, method = 1:2) {
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

  if(n > k) {
    Sigma <- (t(z.t) %*% z.t)/(n-1)
  } else {
    Sigma <- (z.t %*% t(z.t))/(n-1)
  }
  
  A <- trace(Sigma)
  B <- trace(Sigma^2)
  
  F.s <- (n * t(mu.x.t-mu0.t) %*% (mu.x.t-mu0.t))/A
  F.s <- F.s[1]
  
  btFstat <-  numeric(replication)
  
  if(method == 1) { #naive method
    A2 <- A^2
    B2 <- B
    df <- A2/B2
    pvalue <- 1-pchisq(F.s, df, (n-1)*df)
    params <- list(df1=df, df2=(n-1)*df)
  } 
  if(method == 2) {  #bootstrapping method
    for(i in 1:replication) {
      btfflag <- sample(1:n,size = n, replace = TRUE)
      xstar.coefs <- x$coefs[,btfflag]
      xstar.names <- x$fdnames
      xstar.names$reps <- btfflag
      xstar.fd <- fd(xstar.coefs, x$basis, xstar.names)
      xstar.fd.center <- center.fd(xstar.fd)
      btx <- eval.fd(t.seq, xstar.fd)
      btz <- eval.fd(t.seq, xstar.fd.center)
      if(n > k) {
        btSigma <- (t(btz) %*% btz)/(n-1)
      } else {
        btSigma <- (btz %*% t(btz))/(n-1)
      }
      btmu <- apply(btx,1,mean) - mu.x.t
      btFstat[i] <- (n * t(btmu) %*% btmu)/trace(btSigma)
    }
    pvalue <- mean(btFstat>=F.s)
    params <- list(btFstat)
  }
  return(list(statistics = F.s, pvalue = pvalue, params=params))
}
