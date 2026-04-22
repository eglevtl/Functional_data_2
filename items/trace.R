#Trace function
trace <- function(data) {
  tr <- sum(diag(data))
  return(tr)
}