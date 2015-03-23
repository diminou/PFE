### Recuperation des variables environnement ###
PFE<-Sys.getenv("PFE")
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
HOME<-Sys.getenv("HOME")
print(HOME)


### Importation des packages ###
library(RNeo4j)
library(compiler)
library(R.oo)
library(Rcpp)
library(tm)
library(SnowballC)
library(RTextTools)
source(paste(PFE, "ESA.R", sep = "/"))
source(paste(PFE, "requetesNeo4j.R", sep = "/"))

### Nombre de meilleures paires à retenir de l´ESA
globalArtNum <- 10

getCategoriesCibles <- function(){
  filePath <- paste(HOME,".pfe/categoriesCibles.csv",sep="/")
  data <- read.csv(filePath)
  Code_rubrique_AN9 <- data$Code_rubrique_AN9
  Label_Categorie_cible <- data$Lib_rubrique_AN8
  return(categoriesCibles <- data.frame(Code_rubrique_AN9,Label_Categorie_cible))
}
categoriesCibles <- getCategoriesCibles()

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

adaptEsa <- function(esaList) {
  result <- data.frame(cbind(esaList[[1]], esaList[[2]]), stringsAsFactors = F)
  if(is.null(esaList[[1]])|is.null(esaList[[2]])) {
    return(NULL)
  }
  if(is.null(dim(result))) {
    return(NULL)
  }
  result[, 2] <- as.numeric(result[, 2])
  result[, 1] <- as.character(result[, 1])
  return(result)
}

retrieveShortestPath <- function(title1, title2) {
  query <- paste("match (a:article {title:'", escapeApostrophes(title1),
                 "'}), (b:article {title:'",
                 escapeApostrophes(title2),
                 "'}) match p = allShortestPaths((a)-[:linked *]-(b))
                  unwind nodes(p)  as cd
                  with collect(cd.code) as res
                  unwind res as r
                  return r", sep = "")
  return(cypher(db, query))
}


retrieveMostPertinentPath <- function(title1, title2, pertinenceProduct) {
  query <- paste("match (a:article {title:'", escapeApostrophes(title1),
                 "'}), (b:article {title:'",
                 escapeApostrophes(title2),
                 "'})
                 match p = allShortestPaths((a)-[:linked *]-(b))
                 with  p as shortestPath, reduce(accum = 1.0, r in relationships(p) | accum * tofloat(r.pertinence)) as weight
                 order by weight desc
                 unwind nodes(shortestPath) as pp
                 return distinct(pp.code), weight", sep = "")
  result <- cypher(db,query)
  if(is.null(result)) {
    return(NULL)
  }
  result[, 2] <- result[, 2]*pertinenceProduct
  return(na.omit(result))
}

makeAllCouples <- function(vec1) {
  grid <- expand.grid(unique(vec1),unique(vec1))
  grid <- grid[grid[, 1] > grid[, 2], ]
  return(grid)
}

makeAllPairsfromESA <- function(dataframe) {
  ids <- unique(dataframe[,1])
  grid <- expand.grid(ids, ids)
  grid[, 1] <- as.character(grid [, 1])
  grid[, 2] <- as.character(grid [, 2])
  grid <- grid[grid[, 1] > grid[, 2], ]
  grid[, 3] <- rep(0, dim(grid)[1])
  for(i in 1:dim(grid)[1]){
    grid[i, 3] <- dataframe[grid[i, 1], 2] * dataframe[grid[i, 2], 2]
  }
  return(grid)
}

pushBack <- function(lst, elt) {
  result <- lst
  result[[length(lst) + 1]] <- elt
  return(result)
}

getAllCats <- function(grid) {
  catFrames <- list()
  for(i in 1:dim(grid)[1]) {
    catFrames <- pushBack(catFrames, retrieveMostPertinentPath(grid[i, 1], grid[i, 2], grid[i, 3]))
  }
  return(catFrames)
}

getSortedCats <- function(catsList) {
  datamap <- Reduce(function(x, y) rbind(x, y), catsList )
  result <- tryCatch({return(aggregate(datamap[, 2], by = list(datamap[, 1]), FUN = sum))},
                     error = function(e) { return(NULL)})
  if(is.null(result)){
    return(NULL)
  }
  return(result[order(-result[, 2]), ])
}

getBestCatCode <- function(query) {
  result <- tryCatch({return(getSortedCats(getAllCats(na.omit(makeAllPairsfromESA(adaptEsa(cos_sim_req_doc(query)))[1:globalArtNum,])))[1, 1])},
                     error = function(e){return (NULL)})
  return(result)
}

codeToLabel <- function(code) {
  print(code)
  if(is.null(code)) {
    return(NULL)
  }
  if(is.null(code)) {
    return (null)
  }
  return(tryCatch({return(categoriesCibles$Label_Categorie_cible[categoriesCibles$Code_rubrique_AN9==code])},
                  error = function(e) {
    return(NULL)
  }))
}

####################################################
### Sans la recherche de chemins les plus courts ###
####################################################

getCatsArticle2 <- function(artTitle, pertinence) {
  query = paste("match (a:article {title:'",
                escapeApostrophes(artTitle),
                "'})-[r]-(t:target) return a.title as title, r.pertinence as pertinence, t.code as code", sep = "")
  result <- cypher(db, query)
  if(is.null(dim(result))) {
    return(NULL)
  }
  if(dim(result)[1] == 0) {
    return (NULL)
  }
  result$pertinence <- as.numeric(result$pertinence)*pertinence
  return(result)
}

getCatsQuery2 <- function(query){
  articles <- adaptEsa(cos_sim_req_doc(query))
  result <- list()
  if(is.null(dim(articles))) {
    return(NULL)
  }
  for(i in 1:dim(articles)[1]) {
    result <- pushBack(result, getCatsArticle2(articles[i, 1], articles[i, 2]))
  }
  mergeAll <- Reduce(function(x, y) rbind(x, y), result)
  finRes <- tryCatch({return(aggregate(mergeAll[, 2], by = list(mergeAll[, 3]), FUN = sum))},
                     error = function(e) { return(NULL)})
  if(is.null(result)){
    return(NULL)
  }
  return(finRes[, ])
}

getBestCat2 <- function(query){
  res <- getCatsQuery2(query)
  if(is.null(dim(res))) {
    return(NULL)
  }
  return(res[1, 1])
}

codeToLabel(getBestCat2("aéroclubs, école de pilotage"))

codeToLabel(getBestCatCode("papier de bureau"))

# getSortedCats(getAllCats(makeAllPairsfromESA(adaptEsa(cos_sim_req_doc("oxygène médical")))[1:globalArtNum, ]))
# getAllCats(makeAllPairsfromESA(adaptEsa(cos_sim_req_doc("bar tabac")))[1:globalArtNum, ])[[3]]
# makeAllCouples( c(4, 4, 2, 5))
# 
# retrieveMostPertinentPath("Kaufhaus_des_Westens", "Visby", 0.4)


