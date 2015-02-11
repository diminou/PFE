PFE<-Sys.getenv("PFE")
setwd(PFE)

install.packages("RCurl")
install.packages("httr")
install.packages("devtools")
library(RCurl)
library(httr)
library(devtools)
devtools::install_github("nicolewhite/RNeo4j")


