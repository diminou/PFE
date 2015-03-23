PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")


source(paste(PFE, "ESA.R", sep = "/"))
source(paste(PFE, "bonjour.R", sep = "/"))
source(paste(PFE, "rechercheCatCible.R", sep = "/"))






