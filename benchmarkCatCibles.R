PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")


# source(paste(PFE, "ESA.R", sep = "/"))
# source(paste(PFE, "bonjour.R", sep = "/"))
source(paste(PFE, "rechercheCatCible.R", sep = "/"))

library(utils)
library(RCurl)
library(rjson)


dim(categoriesCibles)[1]

categoriesCibles[[2]][1]




benchmarkCategoriesCiblesFinal <- function(){
  
  counter <- 0
  
  for (i in 1:dim(categoriesCibles)[1]){
    nameCat <- categoriesCibles[[2]][i]
    resBestCat <- codeToLabel(getBestCat2(nameCat))
    print(paste("nameCat :", nameCat))
    print(paste("resBestCat :", resBestCat))
    if(!is.null(resBestCat)){
      if(nameCat==resBestCat){
        counter=counter+1
        print("GAGNE MDR")
      }
    }
    
  }
  return (paste(counter,"/",dim(categoriesCibles)),sep="")
}
benchmarkCategoriesCiblesFinal()

