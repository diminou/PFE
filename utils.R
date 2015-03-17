PFE<-Sys.getenv("PFE")
setwd(PFE)

install.packages("RCurl")
install.packages("httr")
install.packages("devtools")
library(RCurl)
library(httr)
library(devtools)
devtools::install_github("nicolewhite/RNeo4j")

install.packages("RNeo4j")
install.packages("stringr")
install.packages("R.oo")
install.packages("tm")
install.packages("SnowballC")
install.packages("RTextTools")

concatenerArticles <- function(df){
  table = table(df)
  ncol(table)
  rownames(table)
  count <- data.frame(count=rep(0,nrow(table)))
  for(i in 1:ncol(abis)){
    count <- count + table[,i]*as.numeric(colnames(table)[i])
  }
  return(data.frame(title=rownames(table),count=count))
}

