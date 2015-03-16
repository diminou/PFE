
PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)


getCategoriesCibles <- function(){
  filePath <- paste(HOME,".pfe/categoriesCibles.csv",sep="/")
  data <- read.csv(filePath)
  Code_rubrique_AN9 <- data$Code_rubrique_AN9
  Label_Categorie_cible <- paste(paste(data$Lib_rubrique_AN8,data$LibSegment30,sep=" "), "",sep=" ")#data$LibSegment5
  return(categoriesCibles <- data.frame(Code_rubrique_AN9,Label_Categorie_cible))
}
categoriesCibles <- getCategoriesCibles()
head(categoriesCibles)

print(cos_sim_req_doc(as.character(categoriesCibles[1, 2])))
