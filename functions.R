##### Collection of custom functions

# Create serial-dilution titration curve
titr <- function(top,fold,n){
  out <- c(top)
  for(i in 2:n){
    out <- c(out, out[i-1]/fold)
  }
  return(out)
}