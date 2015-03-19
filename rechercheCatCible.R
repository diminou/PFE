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

retrieveMostPertinentPath <- function(title1, title2) {
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
  return(na.omit(result))
}



retrieveMostPertinentPath("Kaufhaus_des_Westens", "Visby")
