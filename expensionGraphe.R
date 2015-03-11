# Code permettant de stabiliser/propager un graphe par marche "aléatoire"

PFE<-Sys.getenv("PFE")
source(paste(PFE, "requetesNeo4j.R", sep = "/"))
# Entrée : des concepts et catégories cibles pour une certaine "intention"
# Sortie : un graphe stable par marche markovienne contenant les catégories et concepts liés à l'"intention" de départ

library(RNeo4j)
library(compiler)
library(tm)
library(SnowballC)
#install.packages("devtools")
#devtools::install_github("nicolewhite/RNeo4j")

db = startGraph("127.0.0.1:7474/db/data/")

getWordsRequete <- function(req){
  req <- gsub("_", replacement=" ", x=req,fixed = TRUE)
  req <- tolower(req)
  req <- removeWords(req, stopwords("french"))
  req <- stripWhitespace(req)
  words <- unlist(strsplit(req, "\\s"))
  words <- wordStem(words, language = "french")
  return(words)
}

# Obtention des articles liés au mots de la requete :
getArticles <- function(words){
  a <- data.frame()
  for(w in words){
    a = rbind(a,getArticlesFromWord(w))
  }
  
  # Gérer les articles présents plusieurs fois :
  table = table(a)
  ncol(table)
  rownames(table)
  count <- data.frame(count=rep(0,nrow(table)))
  for(i in 1:ncol(table)){
    count <- count + table[,i]*as.numeric(colnames(table)[i])
  }
  articles <- data.frame(title=rownames(table),count=count)
  return(articles)
}

# Obtention des catégories liées aux articles
getCategories <- function(articles){
  c <- data.frame()
  for(art in articles$title){
    print(art)
    c <- rbind(c, getCategoriesFromArticle(art))
  }
  categories <- unique(c)
  return(categories)
}



requete <- "Boulanger pain"
words <- getWordsRequete(requete)
articles <- getArticles(words)
categories <- getCategories(articles)
