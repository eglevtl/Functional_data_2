# # Two sample F type bootstrap test

F.stat.twosample <- function(x, y, t.seq, alpha=0.05, method=1:2, replications=100) {
  # method = 1: T stat
  # method = 2: Bootsrap
  if(class(x) != "fd") stop("X must be fd object")
  if(class(y) != "fd") stop("Y must be fd object")
  
  mu.x <- mean.fd(x)
  mu.y <- mean.fd(y)
  
  n <- dim(x$coefs)[2]
  m <- dim(y$coefs)[2]
  
  k <- length(t.seq)
  
  cn <- (n*m)/(n+m)
  delta <- (mu.x - mu.y)
  delta.t <- eval.fd(t.seq, delta)
  
  z.x <- center.fd(x)
  z.y <- center.fd(y)
  
  z.x.t <- eval.fd(t.seq, z.x)
  z.y.t <- eval.fd(t.seq, z.y)
  z.t <- cbind(z.x.t, z.y.t)
  
  if(n > k | m > k) {
    Sigma <- (t(z.t) %*% z.t)/(n-2)
  } else {
    Sigma <- (z.t %*% t(z.t))/(n-2)
  }
  
  A <- trace(Sigma)
  B <- trace(Sigma^2)
  
  Fstat <- (cn * t(delta.t) %*% delta.t)/A
  Fstat <- Fstat[1]
  
  btFstat <-  numeric(replications)
  
  if(method == 1) { #naive method
    kappa <- A^2/B
    pvalue <- 1-pf(Fstat, kappa, (n-2)*kappa)
    params <- list(df1 = kappa, df2 = (n-2)*kappa)
  } 
  if(method == 2) {  #bootstrapping method
    for(i in 1:replications) {
      rep1 <- sample(1:n, n, replace = TRUE)
      xstar.coefs <- x$coefs[,rep1]
      xstar.names <- x$fdnames
      xstar.names$reps <- rep1
      xstar.fd <- fd(xstar.coefs, x$basis, xstar.names)
      
      rep2 <- sample(1:m, m, replace = TRUE)
      ystar.coefs <- y$coefs[,rep2]
      ystar.names <- y$fdnames
      ystar.names$reps <- rep2
      ystar.fd <- fd(ystar.coefs, y$basis, ystar.names)
      
      mu.x.star <- mean.fd(xstar.fd)
      mu.y.star <- mean.fd(ystar.fd)
      delta.star <- (mu.x.star - mu.y.star)
      delta.star.t <- eval.fd(t.seq, delta.star)
      
      bt.z.x <- center.fd(xstar.fd)
      bt.z.y <- center.fd(ystar.fd)
      
      bt.z.x.t <- eval.fd(t.seq, bt.z.x)
      bt.z.y.t <- eval.fd(t.seq, bt.z.y)
      z.star.t <- cbind(bt.z.x.t, bt.z.y.t)
      
      if(n > k | m > k) {
        btSigma <- (t(z.star.t) %*% z.star.t)/(n-2)
      } else {
        btSigma <- (z.star.t %*% t(z.star.t))/(n-2)
      }
      
      btmu <- apply(delta.star.t,1,mean) - delta.t
      btFstat[i] <- (cn * t(btmu) %*% btmu)/trace(btSigma)
    }
    pvalue <- mean(btFstat>=Fstat)
    params <- list(btFstat)
  }
  return(list(statistics = Fstat, pvalue = pvalue, params=params))
}
