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

### Connexion avec Neo4j ###
db = startGraph("127.0.0.1:7474/db/data/")

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

retrieveMostPertinentPath <- function(title1, title2, pertinence1, pertinence2) {
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
  result[, 2] <- result[, 2]*pertinence1*pertinence2
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
  grid <- grid[grid[, 1] > grid[, 2], ]
  grid[, 3] <- rep(0, dim(grid)[1])
  for(i in 1:dim(grid)[1]){
    grid[i, 3] <- dataframe[grid[i, 1], 2] * dataframe[grid[i, 2], 2]
  }
  return(grid)
}

makeAllPairsfromESA(cbind(c(1, 2, 3), c(5, 6, 7)))

makeAllCouples( c(4, 4, 2, 5))

retrieveMostPertinentPath("Kaufhaus_des_Westens", "Visby", 0.4, 0.3)


