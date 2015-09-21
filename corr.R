corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  f<-list.files(directory, full.names=TRUE)
  d=complete(directory)
  x=numeric()
  
  for (i in 1:nrow(d)) {
    if (d[i,2]>threshold){
      r<-read.csv(f[i]) ## read in monitor file
      x<-c(x,cor(r[["nitrate"]],r[["sulfate"]],use="complete.obs")) ## add correlation for complete nitrate and sulfate obs
      
     }
  }
  x
}
  
