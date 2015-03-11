# Code permettant d'effectuer des requetes à la base Neo4j

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

# Fonction retournant les titres des articles liés à un mot clé stemmatisé
#   ainsi que la valeur count du lien
# input : un mot stemmatisé
getArticlesFromWord <- function(stem){
  queryA <- paste(paste("MATCH(a:article)--(w:word {stem:'",stem,sep=""),"'}) RETURN a",sep="")
  queryR <- paste(paste("MATCH(a:article)-[rel]-(w:word {stem:'",stem,sep=""),"'}) RETURN rel",sep="")
  resultA <- getNodes(db,queryA)
  resultR <- getRels(db,queryR)
  title <- sapply(resultA, function(p) fixEncoding(p$title))
  count <- sapply(resultR, function(p) p$count)
  result=data.frame(title,count)
  return (result)
}
result=getArticlesFromWord("commerc")
result

# Fonction retournant le lien entre un article et un mot en fonction
# du titre de l'article et
# de la racine du mot
getLinkFromArticleWord <- function(title, stem) {
  q <- paste("match(:article {title:\"", title, "\"})-[r]-(:word {stem:\"", stem, "\"}) return r", sep = "")
  result <- getSingleRel(db, q)
  if(is.null(result)){
    return(NULL)
  }
  return(as.numeric(result$count))
}

# Fonction retournant les mots stemmatisés présents dans un article
#   ainsi que la valeur count du lien
# input : le titre d'un article
getWordsFromArticle <- function(ttr){
  titre = escapeApostrophes(ttr)
  print(titre)
  queryW <- paste(paste("MATCH(a:article {title:\"",titre,sep=""),"\"})--(w:word) RETURN w",sep="")
  queryR <- paste(paste("MATCH(a:article {title:\"",titre,sep=""),"\"})-[rel]-(w:word) RETURN rel",sep="")
  print(queryW)
  print(queryR)
  resultW <- getNodes(db,queryW)
  resultR <- getRels(db,queryR)
  stem <- sapply(resultW, function(p) p$stem)
  count <- sapply(resultR, function(p) p$count)
  result=data.frame(stem,count)
  return (result)
}
result=getWordsFromArticle("Boulanger")
result

# Fonction retournant les labels des catégories liées à un article
# input : titre de l'article
getCategoriesFromArticle <- function(titre){
  queryC <- paste(paste("MATCH(c:category)--(a:article {title:'",titre,sep=""),"'}) RETURN c",sep="")
  resultC <- getNodes(db,queryC)
  label <- sapply(resultC, function(p) p$label)
  result=data.frame(label)
  return (result)
}
result=getCategoriesFromArticle("Baker")
result

# Fonction retournant les titre des articles liés à une catégorie
# input : label de la catégorie
label="Homonymie_de_toponyme"
getArticlesFromCategory <- function(label){
  queryA <- paste(paste("MATCH(c:category {label:'",label,sep=""),"'})--(a:article) RETURN a",sep="")
  resultA <- getNodes(db,queryA)
  titre <- sapply(resultA, function(p) p$title)
  result=data.frame(titre)
  return (result)
}
result=getArticlesFromCategory("Homonymie_de_toponyme")
result


# Fonction retournant les labels des catégories liés à un mot clé stemmatisé
# input : un mot stemmatisé
getCategoriesFromWord <- function(stem){
  queryC <- paste(paste("MATCH(c:category)--(a:article)--(w:word {stem:'",stem,sep=""),"'}) RETURN c",sep="")
  resultC <- getNodes(db,queryC)
  label <- sapply(resultC, function(p) p$label)
  result=data.frame(label)
  return (result)
}
result=getCategoriesFromWord("commerc")
result

# Fonction retournant les mots stemmatisés liés à une catégorie
# input : le label d'une catégorie
getWordsFromCategory <- function(label){
  queryW <- paste(paste("MATCH(c:category {label:'",label,sep=""),"'})--(a:article)--(w:word) RETURN w",sep="")
  resultW <- getNodes(db,queryW)
  stem <- sapply(resultW, function(p) p$stem)
  result=data.frame(stem)
  return (result)
}
result = getWordsFromCategory("Homonymie_de_toponyme")
result