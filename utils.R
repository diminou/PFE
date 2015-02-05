PFE<-Sys.getenv("PFE")
setwd(PFE)

install.packages("RCurl")
install.packages("httr")
install.packages("devtools")
devtools::install_github("nicolewhite/RNeo4j")
