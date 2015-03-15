

PFE<-Sys.getenv("PFE")
setwd(PFE)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)

library(RNeo4j)
library(compiler)
#install.packages("devtools")
#devtools::install_github("nicolewhite/RNeo4j")

db = startGraph("127.0.0.1:7474/db/data/")

# Fonction qui permet d'ignorer l'apostrophe
escapeApostrophes <- cmpfun(function(string) {
  return(gsub("'", "\\'", string, fixed =T ))
})



fixEncoding <- function(string){
  return(enc2utf8(iconv(string, from = "UTF-8", to = "ISO-8859-1")))
}

getCategoriesAround <- function(category_label,buffer){
  queryC <- paste(paste(paste("MATCH(c:category {label:'",category_label,sep=""),"'})-[:relates *1..",buffer,sep=""),"{type:'broader'}]-(c2:category) RETURN c2",sep="")
  resultC <- getNodes(db,queryC)
  label <- sapply(resultC, function(p) fixEncoding(p$label))
  result=data.frame(label)
  print(paste(paste("Il y a",nrow(result)),"catégories reliées"))
  return (result)
}

writeCategoriesCsv <- function(fileName,categories){
  filePath <- paste(paste(paste(HOME,"/.pfe/",sep=""),fileName,sep=""),".csv",sep="")
  write.table(categories, file = filePath,quote=FALSE,row.names = FALSE,col.names=FALSE)
}

# Catégorie Type_de_commerce :
Type_de_commerce <- getCategoriesAround("Type_de_commerce",5)
writeCategoriesCsv("Type_de_commerce",Type_de_commerce)


# Catégorie Métier_du_bâtiment :
Metier_du_batiment <- getCategoriesAround("Métier_du_bâtiment",3)
writeCategoriesCsv("Metier_du_batiment",Metier_du_batiment)


# Catégorie Service_public :
Service_public <- getCategoriesAround("Service_public",2)
writeCategoriesCsv("Service_public",Service_public)
