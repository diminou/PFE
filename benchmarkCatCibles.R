PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")


source(paste(PFE, "ESA.R", sep = "/"))
source(paste(PFE, "bonjour.R", sep = "/"))
source(paste(PFE, "rechercheCatCible.R", sep = "/"))

library(utils)
library(RCurl)
library(rjson)

log <- paste(HOME, ".pfe/benchmarkCatCibles1.log", sep = "/")

dim(categoriesCibles)[1]

categoriesCibles[[2]][1]


sink(file = log, append = T, type = c("output", "message"))

benchmarkCategoriesCiblesFinal <- function(){
  
  counter <- 0
  
  for (i in 1:dim(categoriesCibles)[1]){
    print(date())
    print(paste("i : ", i, sep = ""))
    nameCat <- categoriesCibles[i, 2]
    resBestCat <- codeToLabel(getBestCatCode(nameCat))
    if(!is.null(resBestCat)){
      if(nameCat==resBestCat){
        print(paste("nameCat :", nameCat))
        print(paste("resBestCat :", resBestCat))
        counter=counter+1
        print("GAGNE MDR")
      }
    }
    
  }
  return (paste(counter,"/",dim(categoriesCibles),sep=""))
}
benchmarkCategoriesCiblesFinal()

sink(file = NULL, append = T, type = c("output", "message"))
