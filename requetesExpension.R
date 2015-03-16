

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



fixEncodingRE <- function(string){
  return(enc2utf8(iconv(string, from = "UTF-8", to = "ISO-8859-1")))
}

getCategoriesAround <- function(category_label,buffer){
  queryC <- paste(paste(paste("MATCH(c:category {label:'",category_label,sep=""),"'})-[:relates *1..",buffer,sep=""),"{type:'broader'}]-(c2:category) RETURN c2",sep="")
  resultC <- getNodes(db,queryC)
  label <- sapply(resultC, function(p) fixEncodingRE(p$label))
  result=data.frame(label)
  print(paste(paste("Il y a",nrow(result)),"catégories reliées"))
  return (result)
}

getArticlesFromCategory <- function(category_label) {
  query <- paste("match (c:category {label: '", escapeApostrophes(category_label), "'}) <-- (a:article) return a", sep = "")
  result <- sapply(getNodes(db, query), function(x) fixEncodingRE(x$title))
  return(result)
}

getWordsFromArticle <- function(article_title) {
  query <- paste("match (a:article {title: '", escapeApostrophes(article_title), "'}) -[r]-> (w:word) return a.title as title, r.count as count, w.stem as stem",
                 sep = "")
  result <- cypher(db, query)
  result$w.stem <- sapply(result$w.stem, fixEncodingRE)
  return(result)
}

getWordsFromCats <- function(category_label, depth) {
  query <- paste ("match (c:category {label:'",
                  escapeApostrophes(category_label),
                  "'})<-[r1 *0..",
                  depth,
                  "]-(cat:category)<--(a:article)-[r]->(w:word) return cat.label, a.title as title , r.count as count, w.stem as stem",
                      sep = "")
  result <- cypher(db, query)
  result$w.stem <- sapply(result$w.stem, fixEncodingRE)
  result$a.title <- sapply(result$a.title, fixEncodingRE)
  result$cat.label <- sapply(result$cat.label, fixEncodingRE)
  return(result)
}

getWordsFromCats25K <- function(category_label, depth, offset_over_25K) {
  offset <- as.integer(25000 * offset_over_25K)
  query <- paste ("match (c:category {label:'",
                  escapeApostrophes(category_label),
                  "'})<-[r1 *0..",
                  depth,
                  "]-(cat:category)<--(a:article)-[r]->(w:word) return cat.label as label, a.title as title, r.count as count, w.stem as stem ",
                  "order by cat.label desc skip ",
                  offset,
                  " limit 25000",
                  sep = "")
  result <- cypher(db, query)
  result$stem <- sapply(result$stem, fixEncodingRE)
  result$title <- sapply(result$title, fixEncodingRE)
  result$label <- sapply(result$label, fixEncodingRE)
  return(result)
}

writeResultsToCsv <- function(data, path) {
  write.csv(data, file = path, row.names = F, fileEncoding = "UTF-8")
}

writeAllFromCat25K <- function(category_label, depth) {
  dir = paste(HOME,".pfe", category_label, sep="/")
  dir.create(dir)
  offset_count <- 0
  flag = T
  while(flag) {
    path <- paste(dir, "/", category_label, offset_count, sep = "")
    path <- paste(path, "csv", sep = ".")
    data <- getWordsFromCats25K(category_label, depth, offset_count)
    if(is.null(dim(data))){
      flag = F
    } else {
      writeResultsToCsv(data, path)
      offset_count <- offset_count + 1
    }
  }  
}

writeAllFromCat25K('Service_public', 2)
writeAllFromCat25K("Métier_du_bâtiment", 3)
writeAllFromCat25K('Type_de_commerce', 5)
# 
# writeCategoriesCsv <- function(fileName,categories){
#   filePath <- paste(paste(paste(HOME,"/.pfe/",sep=""),fileName,sep=""),".csv",sep="")
#   write.table(categories, file = filePath,quote=FALSE,row.names = FALSE,col.names=FALSE)
# }
# 
# # Catégorie Type_de_commerce :
# Type_de_commerce <- getCategoriesAround("Type_de_commerce",5)
# writeCategoriesCsv("Type_de_commerce",Type_de_commerce)
# 
# 
# # Catégorie Métier_du_bâtiment :
# Metier_du_batiment <- getCategoriesAround("Métier_du_bâtiment",3)
# writeCategoriesCsv("Metier_du_batiment",Metier_du_batiment)
# 
# 
# # Catégorie Service_public :
# Service_public <- getCategoriesAround("Service_public",2)
# writeCategoriesCsv("Service_public",Service_public)
