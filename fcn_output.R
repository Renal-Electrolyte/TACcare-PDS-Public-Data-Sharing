# ****** R FUNCTIONS FOR OUTPUT FORMAT
require(kableExtra); require(pander);
## Set defaults for kable -- tool for rmarkdown tables
mykable <- function(x,n.left=1,full_width=F,...){
  kable(x,row.names=FALSE,align=c(rep("l",n.left),rep("r",ncol(x)-n.left)),...) %>% 
    kable_styling(bootstrap_options = c("striped", "hover","condensed")
                  ,full_width = full_width
                  )
}
## Set defaults for pander -- tool for rmarkdown tables
mypander <- function(x,width=Inf){
  pander(format(x, quote=FALSE, justify="right"),split.tables=width)
}

## Function to round off p-values
pvalf <- function(x,decimals=3){
  thresh <- 10^(-decimals)
  f <- paste("%0.",decimals,"f",sep="")
  if(!is.numeric(x)){
    pf <- rep("NA",length(x))
    warning("x not numeric")
  } else{
    pf <- sprintf(f,x)
    pf[x<thresh] <- sprintf(paste("<",f,sep=""),thresh)
  }
  pf
}