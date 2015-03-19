library(metafor)
library(compute.es)
library(MAd)

import.google <- function(df_url, NASTRINGS = c("-", "NA", "", "--", "---")) {
  library(RCurl)
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  #IMPORT DATASET FROM GOOGLE SPREADSHEETS
  if(!capabilities()["http/ftp"]) 
    stop("No internet capabilities.")    
  if(.Platform$OS.type == "unix" && is.null(nsl("cran.r-project.org")))
    stop("Internet appears to be down.")
  require(RCurl) #because google uses HTTPS        
  file <- getURL(url=df_url) # Get the file
  con <- textConnection(file) # Establish text connection
  df <- read.csv(con, na.strings=NASTRINGS, header=TRUE, stringsAsFactors = FALSE) #Read it as a df 
  close(con) #Cleanup
  return(df)
}


fC = function(x, digits=2){
  x = formatC(x=x, digits=digits, format="f")
  x = gsub("0([.])", "\\1", x)
  if(as.numeric(x) >= 0)
    x = paste("~", x, sep="")    
  return(x)
}

ores = function(or, lb, ub){  
  x = or_to_d(or)[2] 
  out = NA
  for(i in seq(10, 10000, 10)) {
    es = des(x, n.1=i, n.2=i)
    l = es$l.or
    u = es$u.or
    if(l > lb & u < ub){
      out = des(x, n.1=i-5, n.2=i-5)
      break
    }      
  }
  return(out)
}
r_est = function(r, lb, ub){    
  out = NA
  for(i in seq(10, 10000, 10)) {    
    es = res(r, n=i)    
    l = es$l.r
    u = es$u.r
    if(l > lb & u < ub){
      out = res(r, n=i-5)      
      break
    }      
  }
  return(out)
}

# View(df)

